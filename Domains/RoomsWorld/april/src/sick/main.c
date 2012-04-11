/********************************************************************
  MIT DARPA Urban Challenge Team

  Module Name: sick

  Purpose: Serves as a translator between native Sick messages
    and LCM.

  Files:
    main.c : command line interface and glue
    sick.c : sick driver

  Maintainer: Edwin Olson (eolson@mit.edu)
********************************************************************/
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#include <math.h>

#include "sick.h"
#include "common/getopt.h"
#include "common/timestamp.h"
#include "common/timespec.h"
#include <common/serial_wrapper.h>

#include "lcm/lcm.h"
#include "lcmtypes/laser_t.h"

typedef struct state state_t;

struct state
{
    getopt_t *gopt;
    lcm_t    *lcm;
    sick_t   *sick;

    timestamp_sync_state_t *sync;

    int64_t scan_last_ts;          // time of the last scan

    pthread_mutex_t report_mutex;

    int64_t report_last_utime;
    int     report_scancount_in;  // scans received during report interval
    int     report_scancount_out; // scans published during report interval
    int64_t report_max_lag;       // maximum delay between any two scans during report interval
    int64_t report_us;            // target report rate

    int64_t publish_next_ts;      // time of our last sent message
    int     publish_us;           // target publish rate

    int     fail_count;
};

void on_report_timer(state_t *state)
{
    pthread_mutex_lock(&state->report_mutex);

    int64_t now = timestamp_now();

    double dt = (now - state->report_last_utime)/1000000.0;
    if (fabs(dt) < 0.001) 
        dt = 0.001;

    double in_hz = state->report_scancount_in / dt;

    printf("%s IN: %4.1f Hz   OUT: %4.1f Hz   Max Period: %7.3f ms\n",
           in_hz < 70 ? "WARNING:" : "SUMMARY:",
           in_hz,
           state->report_scancount_out / dt,
           state->report_max_lag / 1000.0);

    if (state->report_scancount_in == 0) {
        state->fail_count++;
        if (state->fail_count == 3 && getopt_get_bool(state->gopt, "exit-on-failure"))
            exit(-1);
    } else {
        state->fail_count = 0;
    }

    
    state->report_max_lag = 0;
    state->report_last_utime = now;
    state->report_scancount_in = 0;
    state->report_scancount_out = 0;

    pthread_mutex_unlock(&state->report_mutex);

}

void my_scan_callback(sick_t *sick, void *user, int64_t ts, 
                      float rad0, float radstep, int nranges, float *ranges, float *intensities)
{
    state_t *state = (state_t*) user;
    laser_t laser;

    laser.utime = ts;
    laser.rad0 = rad0;
    laser.radstep = radstep;

    laser.nranges = nranges;
    laser.ranges = ranges;

    if (intensities == NULL || !getopt_get_bool(state->gopt, "intensities")) {
        laser.nintensities = 0;
        laser.intensities = NULL;
    } else {
        laser.nintensities = nranges;
        laser.intensities = intensities;
    }

    laser_t_publish(state->lcm, getopt_get_string(state->gopt, "channel"), &laser);

    int64_t now = timestamp_now();
    pthread_mutex_lock(&state->report_mutex);

    int64_t lag = now - state->scan_last_ts;
    if (lag > state->report_max_lag)
        state->report_max_lag = lag;
    state->scan_last_ts = now;

    state->report_scancount_in++;
    state->report_scancount_out++;

    pthread_mutex_unlock(&state->report_mutex);
}


int main(int argc, char *argv[])
{
    int res;
    state_t *state = (state_t*) calloc(1, sizeof(state_t));
    
    setlinebuf (stdout);
    state->gopt = getopt_create();

    getopt_add_bool(state->gopt, 'h',"help", 0,"Show this");

    getopt_add_string(state->gopt, 'c', "channel", "LASER", "LC channel name");

    getopt_add_spacer(state->gopt, "");

    getopt_add_int(state->gopt,   '\0', "hz",              "15",     "Target update rate (in Hertz)");
    getopt_add_string(state->gopt,'d',  "device",          "/dev/ttyUSB0",  "Device to connect to");
    getopt_add_int(state->gopt,   'b',  "baud",            "500000", "Baud rate");
    getopt_add_int(state->gopt,   'r',  "resolution",      "25",     "Angular resolution (hundredths of a degree)");
    getopt_add_int(state->gopt,   'f',  "fov",             "180",    "Field of view (Degrees)");
    getopt_add_bool(state->gopt,  'i',  "interlaced",      1,        "Interlaced (required for most high-res modes)"); 
    getopt_add_bool(state->gopt,  '\0', "intensities",     0,        "Request intensities");
    getopt_add_bool(state->gopt,  '\0', "exit-on-failure", 1,        "Exit -1 if sick connection fails"); 
    getopt_add_bool(state->gopt,  '\0', "dump",            0,        "Dump all packets to stdout");

    getopt_add_spacer(state->gopt, "");

    if (!getopt_parse(state->gopt, argc, argv, 1) || getopt_get_bool(state->gopt,"help")
        || state->gopt->extraargs->len!=0) {
        
        printf("Usage: %s [options]\n\n", argv[0]);
        getopt_do_usage(state->gopt);
        return 0;
    }

    state->lcm = lcm_create(NULL);
    if (state->lcm == NULL) {
        printf("Unable to create an LCM object. Network not set up?");
        exit(-1);
    }

    pthread_mutex_init(&state->report_mutex, NULL);

    state->sick = sick_create ();

    char *device = getopt_get_string(state->gopt, "device");

    printf("Sick: Connecting to:%s\n",device); 

    if ((res=sick_connect(state->sick,device, getopt_get_int(state->gopt, "baud")))) {
        printf("Couldn't connect to scanner. Exiting. (%i)\n",res);                
        perror("Error");
        return -1;
    }

    if (getopt_get_bool(state->gopt, "dump"))
        state->sick->log_packets_file = stdout;

    if (sick_set_baud(state->sick, getopt_get_int(state->gopt, "baud"))) {
        printf("Couldn't change baud. Exiting.\n");
        return -1;
    }

    if ((res = sick_set_variant(state->sick,
                                getopt_get_int(state->gopt,"fov"), 
                                getopt_get_int(state->gopt, "resolution"), 
                                getopt_get_bool(state->gopt, "interlaced"),
                                getopt_get_bool(state->gopt, "intensities")))) {
        printf("Couldn't set variant (%d)\n", res);
        return -1;
    }

    int64_t now = timestamp_now();
    state->scan_last_ts = now;
    state->report_last_utime = 0;
    state->report_scancount_in = 0;
    state->report_scancount_out = 0;
    state->report_max_lag = 0;
    state->report_us = 5000000;
    state->publish_next_ts = 0;
    state->publish_us = 1000000/getopt_get_int(state->gopt, "hz");

    sick_set_scan_callback(state->sick, my_scan_callback, state);

    sick_set_continuous(state->sick, 1);

    while(1) {
        sleep(2);
        on_report_timer(state);
    }
}
