#ifndef _MOXA_H
#define _MOXA_H

typedef struct _Moxa Moxa;

Moxa *moxa_serial_open_url(const char *url);
Moxa *moxa_serial_open(const char *ipaddr, int physport);
int moxa_serial_close(Moxa * moxa);
int moxa_serial_getfd(Moxa * moxa);
int moxa_serial_setbaud(Moxa * moxa, int baud);

#endif
