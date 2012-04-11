#ifndef _GPSD_H
#define _GPSD_H

#define GPS_MESSAGE_MAXLEN 512

#define GPS_TIMEOUT -1
#define GPS_INVALID -2

#include <stdio.h>
#include <pthread.h>
#include <stdint.h>

typedef struct gps gps_t;

struct gps
{
	int fd;     // serial port fd
	FILE *logfile;  // log file (for raw data)

	void (*readline_callback)(void *arg, int64_t ts, const char *line);
	void *readline_callback_context;
};

// create & destroy
gps_t *gps_create(const char *portname);
void gps_destroy(gps_t *g);

// log file facility
int gps_set_logfile(gps_t *g, const char *logpath);

// call this after creating
int gps_autonegotiate_baud(gps_t *g, int guessbaud);

// issue a command and wait for successful completion. All commands
// must be called before gps_start.
int gps_command(gps_t *g, const char *cmd);

// request a new baud rate the next time the GPS receiver is powered
// up
int gps_set_baud(gps_t *g, int baud);

// register a callback, which will get GPS NMEA data after gps_start()
// is called
void gps_set_readline_callback(gps_t *g, void (*callback)(void *arg, int64_t ts, const char *line),
			       void *context);

// start the reader thread, which will cause callbacks upon receipt of
// NMEA strings
pthread_t gps_start(gps_t *g);

#endif
