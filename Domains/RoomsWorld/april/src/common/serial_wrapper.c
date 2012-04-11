#include <stdlib.h>
#include "moxa.h"
#include "serial.h"
#include "serial_wrapper.h"

typedef enum {
    SERIAL_WRAPPER_MOXA,
    SERIAL_WRAPPER_SERIAL,
} serial_wrapper_type_t;

struct _SerialWrapper {
    serial_wrapper_type_t type;
    union {
        Moxa * moxa;
        int fd;
    };
};

SerialWrapper *
serial_wrapper_open (const char * port)
{
    SerialWrapper * s = calloc (1, sizeof (SerialWrapper));
    if (port[0] == '/') {
        s->type = SERIAL_WRAPPER_SERIAL;
        s->fd = serial_open (port, 9600, 1);
        if (s->fd < 0)
            goto failed;
    }
    else {
        s->type = SERIAL_WRAPPER_MOXA;
        s->moxa = moxa_serial_open_url (port);
        if (!s->moxa)
            goto failed;
    }

    return s;
failed:
    free (s);
    return NULL;
}

int
serial_wrapper_getfd (SerialWrapper * s)
{
    if (s->type == SERIAL_WRAPPER_SERIAL)
        return s->fd;
    else if (s->type == SERIAL_WRAPPER_MOXA)
        return moxa_serial_getfd (s->moxa);

    return -1;
}

int
serial_wrapper_setbaud (SerialWrapper * s, int baud)
{
    if (s->type == SERIAL_WRAPPER_SERIAL)
        return serial_setbaud (s->fd, baud);
    else if (s->type == SERIAL_WRAPPER_MOXA)
        return moxa_serial_setbaud (s->moxa, baud);

    return -1;
}

int
serial_wrapper_close (SerialWrapper * s)
{
    if (s->type == SERIAL_WRAPPER_SERIAL)
        serial_close (s->fd);
    else if (s->type == SERIAL_WRAPPER_MOXA)
        moxa_serial_close (s->moxa);
    free (s);
    return 0;
}
