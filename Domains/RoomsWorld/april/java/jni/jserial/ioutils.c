#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/poll.h>

#include "ioutils.h"

int write_fully(int fd, const void *b, int len)
{
    int cnt=0;
    int thiscnt;
    unsigned char *bb=(unsigned char*) b;

    while (cnt<len)
    {
        thiscnt=write(fd, &bb[cnt], len-cnt);
        if (thiscnt<0)
            return -1;
        cnt+=thiscnt;
    }

    return cnt;
}

int readtimeout(int fd, void *buf, int maxlen, int msTimeout)
{
    struct pollfd pfd;
    int len;
    int res;

    pfd.fd=fd;
    pfd.events=POLLIN;

    res=poll(&pfd, 1, msTimeout);
    if (res<0) // error
    {
        perror("poll");
        return res;
    }

    if (res==0) // timeout
    {
        errno = ETIMEDOUT;
        return 0;
    }

    len=read(fd, buf, maxlen);
    if (len<0)
    {
        perror("read");
        return len;
    }

    // not a timeout at this point... some other error.
    if (len == 0)
        return -1;

    return len;
}

int read_fully_timeout(int fd, void *bufin, int len, int msTimeout)
{
    char *buf=(char*) bufin;
    int readsofar=0;
    int thisread=0;

    while (readsofar<len)
    {
        thisread = readtimeout(fd, &buf[readsofar], len-readsofar, msTimeout);
        if (thisread < 0)
            return thisread;

        readsofar+=thisread;
    }

    return len;
}

// reads number of bytes available, <0 on error.
int readavailable(int fd)
{
    long avail = 0;

    if(ioctl(fd, FIONREAD, &avail) == 0)
        return avail;
    else
        return -1;
}

// read (and discard) all available data.
void readflush(int fd)
{
    int avail;
    char buf[1024];

    do
    {
        avail=readavailable(fd);
        if (avail>1024)
            avail=1024;

        int ret = read(fd, buf, avail);
        if (ret < 0)
            break;

    } while (avail>0);

}

// returns length of string, 0 on error.
int readlinetimeout(int fd, void *buf_in, int maxlen, int msTimeout)
{
    int len=0;
    int thislen;
    char *buf=(char*) buf_in;

    while (len<maxlen)
    {
        thislen=readtimeout(fd, &buf[len], 1, msTimeout);
        if (thislen<=0)
            return 0;

        if (buf[len]=='\n' || buf[len]=='\r')
        {
            buf[len]=0;
            return len;
        }

        len++;
    }

    buf[len]=0;
    return len;
}

