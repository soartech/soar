#ifndef _SICK_H
#define _SICK_H

#include <stdint.h>

#include <common/serial_wrapper.h>
#include <common/timestamp.h>

#define SICK_PARAMS_LENGTH 34

typedef struct _sick_t sick_t;

typedef void (*sick_scan_callback_t)(sick_t *sick, void *user, int64_t ts, 
                                     float rad0, float radstep,
                                     int nranges, float *ranges, float *intensities);

enum SICK_MODEL { SICK_MODEL_UNKNOWN,
                  SICK_MODEL_LMS291_S05,
                  SICK_MODEL_LMS291_S14 };

struct _sick_t
{
    SerialWrapper * serial;
    int serialfd;
    uint8_t *params;
    int paramslength;

    int fov_degrees;          // degrees
    int res_cdegrees;         // hundredths of a degree
    int interlaced;           // boolean
    int intensities;          // boolean

    int continuousmode; // are we in continuous mode?
  
    float rangescale;     // how many meters per returned range unit?

    pthread_mutex_t writelock; // used for finding responses to requests
    pthread_cond_t  writecond;
    uint8_t writereqid;
    uint8_t *writedata;
    uint8_t writevalid;

    sick_scan_callback_t scan_callback;
    void *scan_callback_user;

    timestamp_sync_state_t *sync;

    FILE *log_packets_file; // send human-readable packet dump here, or NULL
    int sick_model;

    // this state only used when in an intensity mode
    int intensity_fov_degrees; // the fov that we requested from the sensor
    int intensity_start_idx;
    int intensity_end_idx;
    double intensity_deg0;
};


/** Create a new sick object. **/
sick_t *sick_create();

/** Connect the sick object to the requested port. The baud rate is
    automatically negotiated. Baudhint can be initialized with a guess
    of the current baud rate; this can decrease initialization
    time. If you don't know, you can pass 0. Returns 0 on success. 

**/
int sick_connect(sick_t *s, char *port, int baudhint);

void sick_set_scan_callback(sick_t *s, sick_scan_callback_t callback, void *user);

/** Put the sick into continuous mode, where it constantly sends new
    scan data.  Returns 0 on success. **/
int sick_set_continuous(sick_t *s, int enable);

/** Request a baud rate change.  Returns 0 on success. **/
int sick_set_baud(sick_t *s, int baud);

/** Get the Sick scanner type, putting the result into buffer 'buf'.
    Returns 0 on success. **/
int sick_get_type(sick_t *s, char *buf, int bufmax);

/** Get the sick scanner status packet, putting the result into buffer
    'buf'.  Returns 0 on success. **/
int sick_get_status(sick_t *s, uint8_t *buf, int bufmax);

/** Change the scanning mode.  angle: in degrees (180, usually)
    resolution: hundredths of a degree. [25, 50, 100].  Returns 0 on
    success. 

    Possible modes (I = interlaced: you get all the data, but not at once.)

    ===================================================
    LMS200/291 No-intensity configuration modes
    
    FOV\RES   1.0     0.5     0.5I     0.25     0.25I
    ---------------------------------------------------
    180        X       X       X                  X
    100                                  X 

    ===================================================
    LMS200/291 Intensity configuration modes. 
    Interlacing is not possible.

    FOV\RES   1.0     0.5     0.25
    ---------------------------------------------------
    

    ===================================================
    LM291-S14 Mode (90 degree model)

    FOV\RES   1.0     0.5     0.5I     0.25     0.25I
    ---------------------------------------------------
    90                 X
    (NOT ACTUALLY IMPLEMENTED!)

    Note that most 100 deg FOV modes are NOT supported because they
    are strict subsets of the 180 deg modes. 

    I suggest you use the interlaced modes; you get high update rate
    and better timing resolution.

    Note that resolution should be 25, 50, 100 (centi-degrees).
**/
int sick_set_variant(sick_t *s, int fovdegrees, int resolution, int interlaced, int intensities);

/** Request a new scan.  Returns 0 on success. **/
//int sick_request_scan(sick_t *s);

#endif
