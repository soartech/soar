#ifndef _SCIP2_H
#define _SCIP2_H

#include <stdio.h>
#include <pthread.h>
#include <stdint.h>
#include "common/varray.h"
#include "common/vhash.h"

#define RXBUF_SIZE 4096

typedef struct scip2 scip2_t;
struct scip2
{
    int fd;

    int debug;

    pthread_t reader_thread;
    uint32_t xid;

    vhash_t *transactions; // (void*) xid-> scip2_transaction_t

    pthread_mutex_t mutex; // protects writes on fd and xid, and transactions hash table.

    void (*on_99_data)(varray_t *response, void *user);
    void *on_99_data_user;

  char rxbuf[RXBUF_SIZE];
  int rxbuf_pos, rxbuf_avail;

};

scip2_t *scip2_create(const char *path);

varray_t *scip2_transaction(scip2_t *scip, const char *command, int timeoutms);
void scip2_set_99_data_handler(scip2_t *scip, void (*on_99_data)(varray_t *response, void *user), void *user);

void scip2_response_free(scip2_t *scip, varray_t *response);

#endif
