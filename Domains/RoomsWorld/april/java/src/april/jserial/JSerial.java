package april.jserial;

import java.io.*;

public class JSerial
{
    protected static native int serial_open_jni(String url, int baud, int blocking);
    protected static native int serial_set_baud_jni(int fd, int baudrate);
    protected static native int serial_set_ctsrts_jni(int fd, int enable);
    protected static native int serial_set_xon_jni(int fd, int enable);
    protected static native int serial_set_mode_jni(int fd, int databits, int parity, int stopbits);
    protected static native int serial_set_dtr_jni(int fd, int v);
    protected static native int serial_set_rts_jni(int fd, int v);
    protected static native int serial_close_jni(int fd);

    static native int write(int fd, byte buf[], int offset, int len);
    static native int read(int fd, byte buf[], int offset, int len);

    static native int readTimeout(int fd, byte buf[], int offset, int len, int mstimeout);
    static native int readTimeoutFully(int fd, byte buf[], int offset, int len, int mstimeout);
    static native int readAvailable(int fd);

    int fd;

    static {
        System.loadLibrary("jserial");
    }

    public JSerial(String url, int baudrate, String mode, boolean blocking) throws IOException
    {
        fd = serial_open_jni(url, baudrate, blocking ? 1 : 0);
        setMode(mode);

        if (fd < 0)
            throw new IOException("Couldn't open serial port "+url);
    }

    public JSerial(String url, int baudrate, String mode) throws IOException
    {
        fd = serial_open_jni(url, baudrate, 1);
        setMode(mode);

        if (fd < 0)
            throw new IOException("Couldn't open serial port "+url);
    }

    public JSerial(String url, int baudrate) throws IOException
    {
        fd = serial_open_jni(url, baudrate, 1);

        if (fd < 0)
            throw new IOException("Couldn't open serial port "+url);
    }

    public int getFd()
    {
        return fd;
    }

    /** Returns negative on error. **/
    public int readFully(byte buf[], int offset, int len)
    {
        int sofar = 0;
        while (sofar < len) {
            int res = readTimeout(fd, buf, offset + sofar, len - sofar, 1000);
            if (res < 0)
                return res;
            sofar += res;
        }
        return sofar;
    }

    /** How many bytes are available to be read without blocking? **/
    public int available()
    {
        return readAvailable(fd);
    }

    /** Returns 0 on timeout, negative on other errors. **/
    public int readTimeout(byte buf[], int offset, int len, int mstimeout)
    {
        return readTimeout(fd, buf, offset, len, mstimeout);
    }

    public int readFullyTimeout(byte buf[], int offset, int len, int mstimeout)
    {
        return readTimeoutFully(fd, buf, offset, len, mstimeout);
    }

    public int read(byte buf[], int offset, int len)
    {
        return read(fd, buf, offset, len);
    }

    /** Read a single byte. Return negative value on error**/
    public int read()
    {
        byte b[] = new byte[1];
        int ret = readFully(b, 0, 1);

        if (ret < 0)
            return ret;

        if (ret == 0)
            return -1;

        return b[0]&0xff;
    }

    public int read32()
    {
        return (read()<<24) + (read()<<16) + (read()<<8) + read();
    }

    public long read64()
    {
        return (read32()<<32L) + (read32());
    }

    /** Combines characters until a newline is read; the newline is
     * stripped. The timeout applies to each individual character, not
     * the whole string. **/
    public String readLine(int mstimeout)
    {
        StringBuffer sb = new StringBuffer();
        while (true) {
            byte buf[] = new byte[1];
            int len = readTimeout(buf, 0, 1, mstimeout);
            if (len < 1)
                return null;
            char c = (char) buf[0];
            if (c=='\n' || c=='\r')
                break;
            sb.append(c);
        }

        return sb.toString();
    }

    public int write(int v)
    {
        byte b[] = new byte[] {(byte) v};
        return write(b, 0, b.length);
    }

    public int write(String s)
    {
        byte b[] = s.getBytes();
        return write(b, 0, b.length);
    }

    public int write(byte buf[], int offset, int len)
    {
        return write(fd, buf, offset, len);
    }

    public void setBaud(int baudrate) throws IOException
    {
        int res = serial_set_baud_jni(fd, baudrate);
        if (res != 0)
            throw new IOException("Could not set baudrate: "+baudrate);
    }

    public void setCTSRTS(boolean v)
    {
        serial_set_ctsrts_jni(fd, v ? 1 : 0);
    }

    public void setDTR(boolean v)
    {
        serial_set_dtr_jni(fd, v ? 1 : 0);
    }

    public void setRTS(boolean v)
    {
        serial_set_rts_jni(fd, v ? 1 : 0);
    }

    /** Mode e.g. 8N1, 7E1 **/
    public void setMode(String mode)
    {
        assert(mode.length()==3);
        int databits = 8;
        int parity = 0;
        int stopbits = 1;

        switch (mode.charAt(0))
	    {
            case '5':
            case '6':
            case '7':
            case '8':
                databits = mode.charAt(0)-'0';
                break;
            default:
                assert(false);
	    }

        switch (Character.toUpperCase(mode.charAt(1)))
	    {
            case 'N':
                parity = 0;
                break;
            case 'O':
                parity = 1;
                break;
            case 'E':
                parity = 2;
                break;
            default:
                assert(false);
                break;
	    }

        switch (mode.charAt(2))
	    {
            case '1':
            case '2':
                stopbits = mode.charAt(2)-'0';
                break;
            default:
                assert(false);
	    }

        setMode(databits, parity, stopbits);
    }

    public void setMode(int databits, int parity, int stopbits)
    {
        serial_set_mode_jni(fd, databits, parity, stopbits);
    }

    public void close()
    {
        serial_close_jni(fd);
    }

}
