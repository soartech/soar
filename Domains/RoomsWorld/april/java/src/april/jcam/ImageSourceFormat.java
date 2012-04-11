package april.jcam;

/** Information about the format of an image. **/
public class ImageSourceFormat
{
    public int width, height;
    public String format; // usually a fourcc

    public String toString()
    {
        return String.format("ImageSourceFormat[%s %d x %d]", format, width, height);
    }
}
