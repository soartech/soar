#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>

#include "common/serial.h"
#include "common/ioutils.h"
#include "common/timestamp.h"

#include "gps.h"
#include "common/getopt.h"
#include "lcmtypes/nmea_t.h"
#include "lcm/lcm.h"

void callback(void *_context, int64_t ts, const char *buf);

typedef struct gpsd gpsd_t;

struct gpsd
{
    getopt_t *gopt;
    gps_t *g;
    FILE *logf;
    lcm_t *lcm;
};

int main(int argc, char *argv[])
{
    gpsd_t *gpsd = (gpsd_t*) calloc(1, sizeof(gpsd_t));

    gpsd->gopt = getopt_create();
    //getopt_add_string(gpsd->gopt, 'd', "device", "/dev/gps",
    getopt_add_string(gpsd->gopt, 'd', "device", "/dev/ttyUSB0",
            "GPS serial device");
    getopt_add_int(gpsd->gopt,    'b', "baud", "19200", "Serial baud rate");
    //getopt_add_int(gpsd->gopt,    'b', "baud", "38400", "Serial baud rate");
    getopt_add_int(gpsd->gopt,    'p', "port", "7337", "TCP port");
    getopt_add_bool(gpsd->gopt,   'a', "allowremote", 0,
            "Allow remote connections");
    getopt_add_string(gpsd->gopt, 'l', "log", "", "Log file");

    if (!getopt_parse(gpsd->gopt, argc, argv, 1)) {
        getopt_do_usage(gpsd->gopt);
        return 0;
    }

    char *portname = getopt_get_string(gpsd->gopt, "device");

    gpsd->lcm = lcm_create(NULL);
    /*  replaced the following with above line
    gpsd->lcm = lcm_create();
    lc_params_t lcp;
    lc_params_init_defaults(&lcp);
    lcp.transmit_only = 1;

    int res = lc_init(gpsd->lcm, &lcp);
    if (res < 0) {
        printf("Error!\n");
        return 0;
    }
    */
    printf("Connecting to GPS at port: %s\n", portname);

    gpsd->g = gps_create(portname);
    if (gpsd->g==NULL) {
        perror(portname);
        return -1;
    }

    gps_autonegotiate_baud(gpsd->g, getopt_get_int(gpsd->gopt, "baud"));
    gps_set_baud(gpsd->g, getopt_get_int(gpsd->gopt, "baud"));

    char *logpath = getopt_get_string(gpsd->gopt, "log");
    if (strlen(logpath)>0) {
        gpsd->logf = fopen(logpath, "w");
        if (gpsd->logf == NULL) {
            perror(logpath);
            return -1;
        }
    }

    // A = use DGPS when available
    // 8 = 38.4kbps on next reboot
    // 0 = no velocity filter
    // 0.5 = dead reckoning time-out
    gps_command(gpsd->g, "$PGRMC,,,,,,,,,A,,,0,,,0.5*");

    // the above has a typo? This might actually turn off the velocity
    // filter
    // gps_command(gpsd->g, "$PGRMC,,,,,,,,,A,,0,,,0.5*");

    // output sentences we want:
    //   GPRMC : basic position information
    // (-) GSA : active satelite data (better precision data)
    // (-) GSV : verbose info about satelites in view
    //   PGRME : position error information
    //   PGRMV : velocity information

    // enable all output sentences
    gps_command(gpsd->g, "$PGRMO,,3*");

    // start the reader thread.
    gps_start(gpsd->g);

    if (0)
    {
        // turn off everything
        gps_command(gpsd->g, "$PGRMO,,2*");

        // enable what we want...
        gps_command(gpsd->g, "$PGRMO,GPRMC,1*");
        gps_command(gpsd->g, "$PGRMO,PGRME,1*");
        gps_command(gpsd->g, "$PGRMO,PGRMV,1*");
        gps_command(gpsd->g, "$PGRMO,GPGSA,1*");
    }

    gps_set_readline_callback(gpsd->g, callback, gpsd);

    while (1)
        sleep(1);

    gps_destroy(gpsd->g);

    fclose(gpsd->logf);
    free(gpsd);
}

void callback(void *_context, int64_t ts, const char *buf)
{
    gpsd_t *gpsd = (gpsd_t*) _context;

    nmea_t nm;
    nm.utime = ts;
    nm.nmea = (char*) buf;

    char d[1024];
    int dlen = nmea_t_encode(d, 0, 1000, &nm);
    lcm_publish(gpsd->lcm, "NMEA", d, dlen);
}

