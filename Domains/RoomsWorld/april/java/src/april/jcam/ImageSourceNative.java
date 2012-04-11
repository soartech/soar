package april.jcam;

import java.io.*;
import java.util.*;

/** A provider of image data, such as a camera. **/
public class ImageSourceNative extends ImageSource
{
    int srcid;

    protected static native int image_source_open_jni(String url);
    protected static native int image_source_num_formats_jni(int isrc);
    protected static native ImageSourceFormat image_source_get_format_jni(int isrc, int idx);
    protected static native int image_source_set_format_jni(int isrc, int idx);
    protected static native int image_source_get_current_format_jni(int isrc);
    protected static native int image_source_start_jni(int isrc);
    protected static native byte[] image_source_get_frame_jni(int isrc);
    protected static native int image_source_stop_jni(int isrc);
    protected static native int image_source_close_jni(int isrc);
    protected static native int image_source_get_num_features(int isrc);
    protected static native String image_source_get_feature_name(int isrc, int idx);
    protected static native double image_source_get_feature_min(int isrc, int idx);
    protected static native double image_source_get_feature_max(int isrc, int idx);
    protected static native double image_source_get_feature_value(int isrc, int idx);
    protected static native int image_source_set_feature_value(int isrc, int idx, double v);
    protected static native ArrayList<String> image_source_enumerate_jni();

    static {
        System.loadLibrary("imagesource");
    }

    /** Deprecated constructor: use factory method instead, which
     * supports additional URL types.
     **/
    public ImageSourceNative(String url) throws IOException
    {
        srcid = image_source_open_jni(url);
        if (srcid < 0)
            throw new IOException("Unable to open device "+url);
    }

    public static ArrayList<String> getCameraURLs()
    {
        return image_source_enumerate_jni();
    }

    public synchronized void start()
    {
        image_source_start_jni(srcid);
    }

    public synchronized void stop()
    {
        image_source_stop_jni(srcid);
    }

    /** Will return null in the event of an I/O error. **/
    public synchronized byte[] getFrame()
    {
        byte b[] = image_source_get_frame_jni(srcid);

        return b;
    }

    public synchronized int getNumFormats()
    {
        return image_source_num_formats_jni(srcid);
    }

    public synchronized ImageSourceFormat getFormat(int idx)
    {
        return image_source_get_format_jni(srcid, idx);
    }

    /** Try to find a format with the given format string. The first such format will be selected.
        e.g., format = "GRAY8" or "BAYER_RGGB". Returns true on success. **/
    public synchronized boolean setFormat(String format)
    {
        for (int i = 0; i < getNumFormats(); i++) {
            ImageSourceFormat ifmt = getFormat(i);
            if (ifmt.format.equals(format)) {
                setFormat(i);
                return true;
            }
        }

        return false;
    }

    public synchronized void setFormat(int idx)
    {
        image_source_set_format_jni(srcid, idx);
    }

    public synchronized int getCurrentFormatIndex()
    {
        return image_source_get_current_format_jni(srcid);
    }

    public synchronized ImageSourceFormat getCurrentFormat()
    {
        return getFormat(getCurrentFormatIndex());
    }

    public synchronized int getNumFeatures()
    {
        return image_source_get_num_features(srcid);
    }

    public synchronized String getFeatureName(int idx)
    {
        return image_source_get_feature_name(srcid, idx);
    }

    public synchronized double getFeatureMin(int idx)
    {
        return image_source_get_feature_min(srcid, idx);
    }

    public synchronized double getFeatureMax(int idx)
    {
        return image_source_get_feature_max(srcid, idx);
    }

    public synchronized double getFeatureValue(int idx)
    {
        return image_source_get_feature_value(srcid, idx);
    }

    public synchronized int setFeatureValue(int idx, double v)
    {
        return image_source_set_feature_value(srcid, idx, v);
    }

    public synchronized int close()
    {
        return image_source_close_jni(srcid);
    }
}
