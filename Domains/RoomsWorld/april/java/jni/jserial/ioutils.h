#ifndef _IOUTILS_H
#define _IOUTILS_H

int write_fully(int fd, const void *b, int len);
int readtimeout(int fd, void *buf, int maxlen, int msTimeout);
int read_fully_timeout(int fd, void *bufin, int len, int msTimeout);
int readavailable(int fd);
void readflush(int fd);
int readlinetimeout(int fd, void *buf, int maxlen, int msTimeout);

#endif
