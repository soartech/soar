/** THIS IS AN AUTOMATICALLY GENERATED FILE.  DO NOT MODIFY
 * BY HAND!!
 *
 * Generated by lcm-gen
 **/

#include <string.h>
#include "procman_process_t.h"

static int __procman_process_t_hash_computed;
static int64_t __procman_process_t_hash;
 
int64_t __procman_process_t_hash_recursive(const __lcm_hash_ptr *p)
{
    const __lcm_hash_ptr *fp;
    for (fp = p; fp != NULL; fp = fp->parent)
        if (fp->v == __procman_process_t_get_hash)
            return 0;
 
    const __lcm_hash_ptr cp = { p, (void*)__procman_process_t_get_hash };
    (void) cp;
 
    int64_t hash = 0xe8e763f721e7f21fLL
         + __int32_t_hash_recursive(&cp)
         + __string_hash_recursive(&cp)
         + __string_hash_recursive(&cp)
         + __string_hash_recursive(&cp)
         + __boolean_hash_recursive(&cp)
         + __int32_t_hash_recursive(&cp)
         + __boolean_hash_recursive(&cp)
        ;
 
    return (hash<<1) + ((hash>>63)&1);
}
 
int64_t __procman_process_t_get_hash(void)
{
    if (!__procman_process_t_hash_computed) {
        __procman_process_t_hash = __procman_process_t_hash_recursive(NULL);
        __procman_process_t_hash_computed = 1;
    }
 
    return __procman_process_t_hash;
}
 
int __procman_process_t_encode_array(void *buf, int offset, int maxlen, const procman_process_t *p, int elements)
{
    int pos = 0, thislen, element;
 
    for (element = 0; element < elements; element++) {
 
        thislen = __int32_t_encode_array(buf, offset + pos, maxlen - pos, &(p[element].procid), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __string_encode_array(buf, offset + pos, maxlen - pos, &(p[element].name), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __string_encode_array(buf, offset + pos, maxlen - pos, &(p[element].cmdline), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __string_encode_array(buf, offset + pos, maxlen - pos, &(p[element].host), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __boolean_encode_array(buf, offset + pos, maxlen - pos, &(p[element].auto_restart), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __int32_t_encode_array(buf, offset + pos, maxlen - pos, &(p[element].restart_delay_ms), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __boolean_encode_array(buf, offset + pos, maxlen - pos, &(p[element].running), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
    }
    return pos;
}
 
int procman_process_t_encode(void *buf, int offset, int maxlen, const procman_process_t *p)
{
    int pos = 0, thislen;
    int64_t hash = __procman_process_t_get_hash();
 
    thislen = __int64_t_encode_array(buf, offset + pos, maxlen - pos, &hash, 1);
    if (thislen < 0) return thislen; else pos += thislen;
 
    thislen = __procman_process_t_encode_array(buf, offset + pos, maxlen - pos, p, 1);
    if (thislen < 0) return thislen; else pos += thislen;
 
    return pos;
}
 
int __procman_process_t_encoded_array_size(const procman_process_t *p, int elements)
{
    int size = 0, element;
    for (element = 0; element < elements; element++) {
 
        size += __int32_t_encoded_array_size(&(p[element].procid), 1);
 
        size += __string_encoded_array_size(&(p[element].name), 1);
 
        size += __string_encoded_array_size(&(p[element].cmdline), 1);
 
        size += __string_encoded_array_size(&(p[element].host), 1);
 
        size += __boolean_encoded_array_size(&(p[element].auto_restart), 1);
 
        size += __int32_t_encoded_array_size(&(p[element].restart_delay_ms), 1);
 
        size += __boolean_encoded_array_size(&(p[element].running), 1);
 
    }
    return size;
}
 
int procman_process_t_encoded_size(const procman_process_t *p)
{
    return 8 + __procman_process_t_encoded_array_size(p, 1);
}
 
int __procman_process_t_decode_array(const void *buf, int offset, int maxlen, procman_process_t *p, int elements)
{
    int pos = 0, thislen, element;
 
    for (element = 0; element < elements; element++) {
 
        thislen = __int32_t_decode_array(buf, offset + pos, maxlen - pos, &(p[element].procid), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __string_decode_array(buf, offset + pos, maxlen - pos, &(p[element].name), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __string_decode_array(buf, offset + pos, maxlen - pos, &(p[element].cmdline), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __string_decode_array(buf, offset + pos, maxlen - pos, &(p[element].host), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __boolean_decode_array(buf, offset + pos, maxlen - pos, &(p[element].auto_restart), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __int32_t_decode_array(buf, offset + pos, maxlen - pos, &(p[element].restart_delay_ms), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
        thislen = __boolean_decode_array(buf, offset + pos, maxlen - pos, &(p[element].running), 1);
        if (thislen < 0) return thislen; else pos += thislen;
 
    }
    return pos;
}
 
int __procman_process_t_decode_array_cleanup(procman_process_t *p, int elements)
{
    int element;
    for (element = 0; element < elements; element++) {
 
        __int32_t_decode_array_cleanup(&(p[element].procid), 1);
 
        __string_decode_array_cleanup(&(p[element].name), 1);
 
        __string_decode_array_cleanup(&(p[element].cmdline), 1);
 
        __string_decode_array_cleanup(&(p[element].host), 1);
 
        __boolean_decode_array_cleanup(&(p[element].auto_restart), 1);
 
        __int32_t_decode_array_cleanup(&(p[element].restart_delay_ms), 1);
 
        __boolean_decode_array_cleanup(&(p[element].running), 1);
 
    }
    return 0;
}
 
int procman_process_t_decode(const void *buf, int offset, int maxlen, procman_process_t *p)
{
    int pos = 0, thislen;
    int64_t hash = __procman_process_t_get_hash();
 
    int64_t this_hash;
    thislen = __int64_t_decode_array(buf, offset + pos, maxlen - pos, &this_hash, 1);
    if (thislen < 0) return thislen; else pos += thislen;
    if (this_hash != hash) return -1;
 
    thislen = __procman_process_t_decode_array(buf, offset + pos, maxlen - pos, p, 1);
    if (thislen < 0) return thislen; else pos += thislen;
 
    return pos;
}
 
int procman_process_t_decode_cleanup(procman_process_t *p)
{
    return __procman_process_t_decode_array_cleanup(p, 1);
}
 
int __procman_process_t_clone_array(const procman_process_t *p, procman_process_t *q, int elements)
{
    int element;
    for (element = 0; element < elements; element++) {
 
        __int32_t_clone_array(&(p[element].procid), &(q[element].procid), 1);
 
        __string_clone_array(&(p[element].name), &(q[element].name), 1);
 
        __string_clone_array(&(p[element].cmdline), &(q[element].cmdline), 1);
 
        __string_clone_array(&(p[element].host), &(q[element].host), 1);
 
        __boolean_clone_array(&(p[element].auto_restart), &(q[element].auto_restart), 1);
 
        __int32_t_clone_array(&(p[element].restart_delay_ms), &(q[element].restart_delay_ms), 1);
 
        __boolean_clone_array(&(p[element].running), &(q[element].running), 1);
 
    }
    return 0;
}
 
procman_process_t *procman_process_t_copy(const procman_process_t *p)
{
    procman_process_t *q = (procman_process_t*) malloc(sizeof(procman_process_t));
    __procman_process_t_clone_array(p, q, 1);
    return q;
}
 
void procman_process_t_destroy(procman_process_t *p)
{
    __procman_process_t_decode_array_cleanup(p, 1);
    free(p);
}
 
int procman_process_t_publish(lcm_t *lc, const char *channel, const procman_process_t *p)
{
      int max_data_size = procman_process_t_encoded_size (p);
      uint8_t *buf = (uint8_t*) malloc (max_data_size);
      if (!buf) return -1;
      int data_size = procman_process_t_encode (buf, 0, max_data_size, p);
      if (data_size < 0) {
          free (buf);
          return data_size;
      }
      int status = lcm_publish (lc, channel, buf, data_size);
      free (buf);
      return status;
}

struct _procman_process_t_subscription_t {
    procman_process_t_handler_t user_handler;
    void *userdata;
    lcm_subscription_t *lc_h;
};
static
void procman_process_t_handler_stub (const lcm_recv_buf_t *rbuf, 
                            const char *channel, void *userdata)
{
    int status;
    procman_process_t p;
    memset(&p, 0, sizeof(procman_process_t));
    status = procman_process_t_decode (rbuf->data, 0, rbuf->data_size, &p);
    if (status < 0) {
        fprintf (stderr, "error %d decoding procman_process_t!!!\n", status);
        return;
    }

    procman_process_t_subscription_t *h = (procman_process_t_subscription_t*) userdata;
    h->user_handler (rbuf, channel, &p, h->userdata);

    procman_process_t_decode_cleanup (&p);
}

procman_process_t_subscription_t* procman_process_t_subscribe (lcm_t *lcm, 
                    const char *channel, 
                    procman_process_t_handler_t f, void *userdata)
{
    procman_process_t_subscription_t *n = (procman_process_t_subscription_t*)
                       malloc(sizeof(procman_process_t_subscription_t));
    n->user_handler = f;
    n->userdata = userdata;
    n->lc_h = lcm_subscribe (lcm, channel, 
                                 procman_process_t_handler_stub, n);
    if (n->lc_h == NULL) {
        fprintf (stderr,"couldn't reg procman_process_t LCM handler!\n");
        free (n);
        return NULL;
    }
    return n;
}

int procman_process_t_subscription_set_queue_capacity (procman_process_t_subscription_t* subs, 
                              int num_messages)
{
    return lcm_subscription_set_queue_capacity (subs->lc_h, num_messages);
}

int procman_process_t_unsubscribe(lcm_t *lcm, procman_process_t_subscription_t* hid)
{
    int status = lcm_unsubscribe (lcm, hid->lc_h);
    if (0 != status) {
        fprintf(stderr, 
           "couldn't unsubscribe procman_process_t_handler %p!\n", hid);
        return -1;
    }
    free (hid);
    return 0;
}
