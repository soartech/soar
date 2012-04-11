#ifndef __SERIAL_WRAPPER_H__
#define __SERIAL_WRAPPER_H__

typedef struct _SerialWrapper SerialWrapper;

SerialWrapper *
serial_wrapper_open (const char * port);

int
serial_wrapper_getfd (SerialWrapper * s);

int
serial_wrapper_setbaud (SerialWrapper * s, int baud);

int
serial_wrapper_close (SerialWrapper * s);

#endif
