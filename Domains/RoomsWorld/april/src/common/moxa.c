#include <stdlib.h>
#include <string.h>
#include "ssocket.h"
#include "moxa.h"


#define DCMD_IOCTL          16          /* IOCTL command           */
#define DCMD_FLOWCTRL       17          /* Flow control command        */
#define DCMD_LINECTRL       18          /* Line status control command */
#define DCMD_LSTATUS        19          /* Line status read command    */
#define DCMD_FLUSH          20          /* Buffer flush command        */
#define DCMD_IQUEUE         21          /* Input queue command         */
#define DCMD_OQUEUE         22          /* Output queue command        */
#define DCMD_BAUDRATE       23          /* Set port's baud rate command */
#define DCMD_BREAK          35          /* Send break in mini-seconds  */
#define DCMD_BREAKCNT       55          /* Get break count command */
#define DCMD_DSTATUS        56          /* Data status read command */

struct _Moxa
{
    char *ipaddr;
    int   port;

    ssocket_t *data_socket;
    ssocket_t *cmd_socket;
};

int moxa_serial_close(Moxa * moxa)
{
    if (moxa->ipaddr)
        free(moxa->ipaddr);
    if (moxa->data_socket)
        ssocket_destroy(moxa->data_socket);
    if (moxa->cmd_socket)
        ssocket_destroy(moxa->cmd_socket);

    free(moxa);
    return 0;
}

Moxa *moxa_serial_open(const char *ipaddr, int physport)
{
    int res;
    Moxa *moxa;

    moxa = (Moxa*) calloc(1, sizeof(Moxa));

    if (!moxa)
        return NULL;

    moxa->ipaddr = strdup(ipaddr);
    moxa->port = physport;

    moxa->data_socket = ssocket_create();
    res = ssocket_connect(moxa->data_socket, ipaddr, 4001 + physport);
    if (res) 
        goto error;

    moxa->cmd_socket = ssocket_create();
    res = ssocket_connect(moxa->cmd_socket, ipaddr, 966 + physport);
    if (res)
        goto error;

    return moxa;

error:
    moxa_serial_close(moxa);

    return NULL;
}

// port is of the format: moxa:ipaddr:physicalport
Moxa *moxa_serial_open_url(const char *url)
{
    if (strncmp(url, "moxa:", 5))
        return NULL;

    char *tmp = strdup(url);
    char *ipaddr = strchr(tmp, ':')+1;
    char *port = strrchr(tmp, ':');
    port[0] = 0;
    port = &port[1];

//    printf("ipaddr: %s, port: %s\n", ipaddr, port);
    Moxa *res = moxa_serial_open(ipaddr, atoi(port));
    free(tmp);

    return res;
}

int moxa_serial_getfd(Moxa * moxa)
{
    return ssocket_get_fd(moxa->data_socket);
}

static int moxa_wait_ack(Moxa *moxa, char cmd)
{
    fd_set  readfds;
    char    buf[20];

    struct timeval timeout;
    timeout.tv_usec = 0L;
    timeout.tv_sec = 2;
    FD_ZERO(&readfds);
    int fd = ssocket_get_fd(moxa->cmd_socket);

    FD_SET(fd, &readfds);
    if (select(fd + 1, &readfds, NULL, NULL, &timeout) < 0)
        return -1;
    if (!FD_ISSET(fd, &readfds))
        return -2;

    int len = recv(fd, buf, 20, 0);
    if (len < 3 || buf[0] != cmd || buf[1] != 'O' || buf[2] != 'K')
        return -3;

    return 0;
}

static int moxa_send_command(Moxa *moxa, void *cmd, int cmdlen)
{
    int tries = 0;

    while (1) {
        int res = send(ssocket_get_fd(moxa->cmd_socket), cmd, 6, 0);
        if (res != 6) 
            perror("send");
        
        // XXX to-do, wait for ACK
        res = moxa_wait_ack(moxa, ((unsigned char*) cmd)[0]);
        if (res) {
            printf("command %4i failed, reason %4i (tries = %4d)\n", 
                   ((unsigned char*) cmd)[0], res, tries++);
            continue;
        }

        return res;
    }
}

int moxa_serial_setbaud(Moxa * moxa, int baud)
{
    if (baud==500000) {
        // moxa 6650 computes dividers badly, and the actual baud rate
        // realized by a request of 500000 is very bad. We get a baud
        // rate closer to 500000 by requesting the baud rate below.
        baud = 491520;
    }

    unsigned char buf[6];
    buf[0] = DCMD_BAUDRATE;
    buf[1] = 4;
    buf[2] = (baud >> 0) & 0xff;
    buf[3] = (baud >> 8) & 0xff;
    buf[4] = (baud >> 16) & 0xff;
    buf[5] = (baud >> 24) & 0xff;

    return moxa_send_command(moxa, buf, 6);
}
