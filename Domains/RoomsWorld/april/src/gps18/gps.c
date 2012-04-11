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

#define TIMEOUT_MS 500

int gps_readline(gps_t *g, uint8_t *buf, int maxlen);

static int hex2int(uint8_t c)
{
	if (c>='0' && c<='9')
		return c - '0';
	if (c>='a' && c<='f')
		return c - 'a' + 10;
	if (c>='A' && c<='F')
		return c - 'A' + 10;

	return -10000;
}

static uint8_t int2hex(uint8_t v)
{
	if (v < 10)
		return '0'+v;
	return 'A'+v-10;
}

void *gps_read_proc(void *v)
{
	gps_t *g = (gps_t*) v;
	uint8_t buf[GPS_MESSAGE_MAXLEN + 1];
	buf[GPS_MESSAGE_MAXLEN] = 0;

	while (1) {
		int res = gps_readline(g, buf, GPS_MESSAGE_MAXLEN);

		if (!res && g->readline_callback != NULL) {
			int64_t now = timestamp_now();

			g->readline_callback(g->readline_callback_context, now, (const char*) buf);
		}
	}

	return NULL;
}

gps_t *gps_create(const char *portname)
{
	gps_t *g;
	g = (gps_t*) calloc(sizeof(gps_t), 1);

	g->fd = serial_open(portname, 9600, 1);
	if (g->fd <= 0) {
		free(g);
		return NULL;
	}

	return g;
}

pthread_t gps_start(gps_t *g)
{
	pthread_t pid;
	pthread_create(&pid, NULL, gps_read_proc, g);

	return pid;
}

void gps_destroy(gps_t *g)
{
	close(g->fd);
	if (g->logfile != NULL) {
		fclose(g->logfile);
		g->logfile = NULL;
	}

	free(g);
}

int gps_set_logfile(gps_t *g, const char *logpath)
{
	FILE *f = fopen(logpath, "w");
	if (f==NULL)
		return -1;

	g->logfile = f;
	return 0;
}

// returns checksum, or -1 if buffer is invalid
int gps_compute_checksum(const uint8_t *buf)
{
	if (buf[0]!='$') {
//		printf("string doesn't start with $\n");
		return -1;
	}

	uint8_t chk = 0;
	int pos = 1;

	while (buf[pos]!=0 && buf[pos]!='*')
		chk ^= buf[pos++];

	return chk;
}

int gps_checksum_okay(const uint8_t *buf)
{

	int len = strlen((char*) buf);
	if (len < 3 || buf[len-3]!='*')
		return 0;

	uint8_t chk = 0;
	int chk0 = hex2int(buf[len-2]);
	int chk1 = hex2int(buf[len-1]);
	if (chk0 < 0 || chk1 < 0)
		return 0;

	chk = (chk0<<4) + chk1;

	if (chk == gps_compute_checksum(buf))
		return 1;

	return 0;
}


int gps_readline(gps_t *g, uint8_t *buf, int maxlen)
{
	int tries = 0;

loop:
	tries++;
	if (tries > 3)
		return GPS_INVALID;

	int res = read_line_timeout(g->fd, buf, maxlen, TIMEOUT_MS);
	if (res == 0) 
		return GPS_TIMEOUT;

	if (buf[0]==0)
		goto loop;

	if (!gps_checksum_okay(buf))
		goto loop;

	/*
	struct timespec ts;
	timespec_now(&ts);
	double t = timespec_seconds(&ts);

	if (g->logfile != NULL) {
		fprintf(g->logfile, "%.3f\tGPS\t0\t:\t%s\n", t, buf);
		printf(".");
		fflush(NULL);
	} else {
//		printf(".");
//		fflush(NULL);
//		printf("%.3f\tGPS\t0\t:\t%s\n", t, buf);
	}
	*/

	return 0;
}

int gps_write_command(gps_t *g, const char *cmd)
{
	int chk = gps_compute_checksum((uint8_t*) cmd);
	uint8_t cmdchk[5];

	cmdchk[0] = int2hex(chk>>4);
	cmdchk[1] = int2hex(chk&0x0f);
	cmdchk[2] = '\r';
	cmdchk[3] = '\n';

	write_fully(g->fd, cmd, strlen((char*) cmd));
	write_fully(g->fd, cmdchk, 4);

	return 0;
}

int gps_command(gps_t *g, const char *cmd)
{
	gps_write_command(g, cmd);

	// wait for confirmation
	uint8_t rpy[GPS_MESSAGE_MAXLEN + 1];
	rpy[GPS_MESSAGE_MAXLEN] = 0;

	// read up to 100 lines before giving up
	for (int i = 0; i < 100; i++) {

		int res = gps_readline(g, rpy, GPS_MESSAGE_MAXLEN);

		// the read failed, report failure.
		if (res)
			return res;

		// if we get a confirmation, return success.
		if (!strncmp((char*) rpy, cmd, strlen(cmd)))
			return 0;

		// we got a valid line, but not the one we were looking for.
		// loop.
//		printf("EXTRA: %s\n", rpy);
	}
		
	// we didn't get a response, report failure.
	return -1;
}

int gps_autonegotiate_baud(gps_t *g, int guessbaud)
{
	int bauds[] = { 4800, 9600, 19200, 38400, 600, 1200, 2400, -1 };
	int baudidx = -1;

	uint8_t rpy[GPS_MESSAGE_MAXLEN + 1];
	rpy[GPS_MESSAGE_MAXLEN] = 0;

	while (1) {
		int baud;

		if (baudidx < 0)
			baud = guessbaud;
		else {
			if (bauds[baudidx] < 0)
				baudidx = 0;
			baud = bauds[baudidx];
		}

		printf("Trying %6d baud...", baud);
		fflush(NULL);

		// enable all output sentences
		gps_write_command(g, "$PGRMO,,3*");

		serial_setbaud(g->fd, baud);
		read_flush(g->fd);
		
		for (int i = 0; i < 4; i++) {
			int res = gps_readline(g, rpy, GPS_MESSAGE_MAXLEN);
			if (!res) {
				printf("GPS found!\n");
				return 0;
			}

			printf(".");
			fflush(NULL);
		}
		printf("\n");

		baudidx++;
	}
	
	// no luck
	return -1;
}

// baud rate won't change until next power cycle
int gps_set_baud(gps_t *g, int baud)
{
	int bauds[] = { 0, 1200, 2400, 4800, 9600, 19200, 300, 600, 38400, -1 };

	int idx = 0;
	while (bauds[idx] != baud) {
		if (bauds[idx] < 0)
			return -1;
		idx++;
	}

	// invalid baud rate passed in.
	if (bauds[idx] < 0)
		return -1;

	char buf[GPS_MESSAGE_MAXLEN + 1];
	buf[GPS_MESSAGE_MAXLEN] = 0;

	sprintf(buf, "$PGRMC,,,,,,,,,,%i,,,,,*", idx);

	gps_command(g, buf);

	// baud rate won't change until next power cycle
//	serial_setbaud(g->fd, baud);

	return 0;
}

void gps_set_readline_callback(gps_t *g, void (*callback)(void *arg, int64_t ts, const char *line),
			       void *context)
{
	g->readline_callback_context = context;
	g->readline_callback = callback;
}

