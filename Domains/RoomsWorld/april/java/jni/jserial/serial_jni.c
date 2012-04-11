#include <stdlib.h>
#include <string.h>
#include <jni.h>
#include <unistd.h>

#include <assert.h>

#include "serial.h"
#include "ioutils.h"

/*
 * Class:     jserial_JSerial
 * Method:    serial_open_jni
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1open_1jni
(JNIEnv *jenv, jclass cls, jstring _url, int baud, int blocking)
{
    const char *url=(*jenv)->GetStringUTFChars(jenv, _url, 0);

    int res = serial_open(url, baud, blocking);

    (*jenv)->ReleaseStringUTFChars(jenv, _url, url);
    return res;
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_set_baud_jni
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1set_1baud_1jni
  (JNIEnv *jenv, jclass cls, jint fd, jint baud)
{
    return serial_set_baud(fd, baud);
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_enable_ctsrts_jni
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1set_1ctsrts_1jni
(JNIEnv *jenv, jclass cls, jint fd, jint enable)
{
    return serial_set_ctsrts(fd, enable);
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_enable_xon_jni
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1set_1xon_1jni
(JNIEnv *jenv, jclass cls, jint fd, jint enable)
{
    return serial_set_xon(fd, enable);
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_set_mode_jni
 * Signature: (IIII)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1set_1mode_1jni
  (JNIEnv *jenv, jclass cls, jint fd, jint databits, jint parity, jint stopbits)
{
    return serial_set_mode(fd, databits, parity, stopbits);
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_enable_n82_jni
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1enable_1n82_1jni
(JNIEnv *jenv, jclass cls, jint fd, jint enable)
{
    assert(enable==1);
    return serial_set_N82(fd);
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_set_dtr_jni
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1set_1dtr_1jni
  (JNIEnv *jenv, jclass cls, jint fd, jint v)
{
    return serial_set_dtr(fd, v);
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_set_rts_jni
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_serial_1set_1rts_1jni
  (JNIEnv *jenv, jclass cls, jint fd, jint v)
{
    return serial_set_rts(fd, v);
}

/*
 * Class:     jserial_JSerial
 * Method:    serial_close_jni
 * Signature: (I)V
 */
JNIEXPORT int JNICALL Java_april_jserial_JSerial_serial_1close_1jni
  (JNIEnv *jenv, jclass cls, jint fd)
{
    return serial_close(fd);
}

/*
 * Class:     jserial_JSerial
 * Method:    write
 * Signature: (I[BII)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_write
  (JNIEnv *jenv, jclass jcls, jint fd, jbyteArray _buf, jint offset, jint len)
{
    char *buf = (char*) (*jenv)->GetByteArrayElements(jenv, _buf, 0);

    int res = write(fd, &buf[offset], len);

    (*jenv)->ReleaseByteArrayElements(jenv, _buf, (void*) buf, 0);

    return res;
}

/*
 * Class:     jserial_JSerial
 * Method:    read
 * Signature: (I[BII)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_read
  (JNIEnv *jenv, jclass jcls, jint fd, jbyteArray _buf, jint offset, jint len)
{
    char *buf = (char*) (*jenv)->GetByteArrayElements(jenv, _buf, 0);

    int res = read(fd, &buf[offset], len);

    (*jenv)->ReleaseByteArrayElements(jenv, _buf, (void*) buf, 0);

   return res;
}

/*
 * Class:     jserial_JSerial
 * Method:    readTimeout
 * Signature: (I[BIII)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_readTimeout
  (JNIEnv *jenv, jclass jcls, jint fd, jbyteArray _buf, jint offset, jint len, jint msTimeout)
{
    char *buf = (char*) (*jenv)->GetByteArrayElements(jenv, _buf, 0);

    int res = readtimeout(fd, &buf[offset], len, msTimeout);

    (*jenv)->ReleaseByteArrayElements(jenv, _buf, (void*) buf, 0);

    return res;
}

/*
 * Class:     jserial_JSerial
 * Method:    readTimeoutFully
 * Signature: (I[BIII)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_readTimeoutFully
  (JNIEnv *jenv, jclass jcls, jint fd, jbyteArray _buf, jint offset, jint len, jint msTimeout)
{
    char *buf = (char*) (*jenv)->GetByteArrayElements(jenv, _buf, 0);

    int res = read_fully_timeout(fd, &buf[offset], len, msTimeout);

    (*jenv)->ReleaseByteArrayElements(jenv, _buf, (void*) buf, 0);

    return res;
}

/*
 * Class:     jserial_JSerial
 * Method:    readAvailable
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_april_jserial_JSerial_readAvailable
  (JNIEnv *jenv, jclass jcls, jint fd)
{
    return readavailable(fd);
}

