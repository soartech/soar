package april.jcam;

import java.awt.image.*;
import java.io.*;
import java.util.*;
import javax.imageio.*;

/** handles
    file:///path/to/a/file.png
    dir:///path/to/a/directory/

    optional parameters:

    fps=XXX
    loop=true

    i.e.:

    dir:///home/ebolson/images/?fps=15&loop=false
**/
public class ImageSourceFile extends ImageSource
{
    ArrayList<String> paths = new ArrayList<String>();
    int pos = 0;
    ImageSourceFormat ifmt;
    boolean loop = true;
    double fps = Double.MAX_VALUE;
    long lastmtime;

    public ImageSourceFile(String url) throws IOException
    {
        int argidx = url.indexOf("?");
        if (argidx >= 0) {
            String arg = url.substring(argidx+1);
            url = url.substring(0, argidx);

            String params[] = arg.split("&");
            for (String param : params) {
                String keyval[] = param.split("=");
                if (keyval[0].equals("fps"))
                    fps = Double.parseDouble(keyval[1]);
                else if (keyval[0].equals("loop"))
                    loop = Boolean.parseBoolean(keyval[1]);
                else
                    System.out.println("ImageSourceFile: Unknown parameter "+keyval[0]);
            }
        }

        if (url.startsWith("file://")) {
            int idx = url.indexOf("://");
            String path = url.substring(idx + 2);

            paths.add(path);
        } else if (url.startsWith("dir://")) {
            int idx = url.indexOf("://");
            String dirpath = url.substring(idx + 2);

            File dir = new File(dirpath);

            for (String child : dir.list()) {
                String childpath = dirpath+"/"+child;
                String tmp = childpath.toLowerCase();
                if (tmp.endsWith("jpeg") || tmp.endsWith("jpg") || tmp.endsWith("png") ||
                    tmp.endsWith("bmp") || tmp.endsWith("wbmp") || tmp.endsWith("gif"))
                    paths.add(childpath);
            }

            Collections.sort(paths);

        } else {
            throw new IOException("ImageSourceFile: invalid URL");
        }

        if (paths.size()==0)
            throw new IOException("ImageSourceFile: found no files");

        // load the first image so we can get the dimensions.
        BufferedImage im = ImageIO.read(new File(paths.get(pos)));

        ifmt = new ImageSourceFormat();
        ifmt.width = im.getWidth();
        ifmt.height = im.getHeight();
        ifmt.format = "RGB";
    }

    public void start()
    {
        pos = 0;
        lastmtime = 0;
    }

    public void stop()
    {
    }

    // ignores timeout.
    public byte[] getFrame()
    {
        /////////////////////////////////////
        // wait until it's time to produce a frame.
        long nowmtime = System.currentTimeMillis();
        long wakeuptime = lastmtime + ((int) (1000/fps));
        long waittime = wakeuptime - nowmtime;

        if (waittime > 0) {

            try {
                Thread.sleep(waittime);
            } catch (InterruptedException ex) {
            }
        }

        lastmtime = System.currentTimeMillis();

        // produce a frame.
        try {
            if (pos >= paths.size()) {
                if (loop)
                    pos = 0;
                else
                    return null;
            }

            BufferedImage im = ImageIO.read(new File(paths.get(pos++)));
            int width = im.getWidth(), height = im.getHeight();
            byte b[] = new byte[width*height*3];
            int bpos = 0;
            for (int y = 0; y < height; y++) {
                for (int x = 0; x < width; x++) {
                    int rgb = im.getRGB(x, y);
                    b[bpos++] = (byte) ((rgb>>16)&0xff);
                    b[bpos++] = (byte) ((rgb>>8)&0xff);
                    b[bpos++] = (byte) ((rgb)&0xff);
                }
            }

            return b;

        } catch (IOException ex) {
            System.out.println("ImageSourceFile exception: "+ex);
        }

        return null;
    }

    public int getNumFormats()
    {
        return 1;
    }

    public ImageSourceFormat getFormat(int idx)
    {
        assert(idx==0);
        return ifmt;
    }

    public void setFormat(int idx)
    {
        assert(idx==0);
    }

    public int getCurrentFormatIndex()
    {
        return 0;
    }

    public int close()
    {
        return 0;
    }

    public int getNumFeatures()
    {
        return 1;
    }

    public String getFeatureName(int idx)
    {
        return "FPS";
    }

    public double getFeatureMin(int idx)
    {
        return 1;
    }

    public double getFeatureMax(int idx)
    {
        return 100;
    }

    public double getFeatureValue(int idx)
    {
        return Math.max(1, Math.min(fps, 100));
    }

    /** returns non-zero on error. **/
    public int setFeatureValue(int idx, double v)
    {
        fps = v;
        return 0;
    }
}
