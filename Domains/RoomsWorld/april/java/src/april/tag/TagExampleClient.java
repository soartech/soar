package april.tag;

import java.awt.image.*;
import java.util.*;
import java.io.*;
import java.net.*;
import javax.imageio.*;

import april.util.*;

/** Demonstrates how to connect to the server for tag detections. **/
public class TagExampleClient
{
    Socket sock;
    DataInputStream ins;
    DataOutputStream outs;
    TagFamily tf;

    public TagExampleClient(TagFamily tf, String host) throws IOException
    {
        this.tf = tf;
        sock = new Socket(host, TagServer.TCP_PORT);
    }

    public ArrayList<TagDetection> process(BufferedImage im, double opticalCenter[]) throws IOException
    {
        // we only directly support grayscale images. This converts the image if necessary.
        im = ImageUtil.convertImage(im, BufferedImage.TYPE_BYTE_GRAY);

        DataInputStream ins = new DataInputStream(new BufferedInputStream(sock.getInputStream()));
        DataOutputStream outs = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));

        int width = im.getWidth(), height = im.getHeight();

        //////////////////////////////////////////////
        // Send request
        outs.writeUTF(tf.getClass().getName()); // e.g. "april.tag.Tag36h11"
        outs.writeInt(width);
        outs.writeInt(height);
        for (int i = 0; i < 2; i++)
            outs.writeDouble(opticalCenter[i]);

        byte b[] = ((DataBufferByte) (im.getRaster().getDataBuffer())).getData();
        outs.write(b);
        outs.flush();

        //////////////////////////////////////////////
        // Read response
        ArrayList<TagDetection> detections = new ArrayList<TagDetection>();
        int ndetections = ins.readInt();

        for (int detidx = 0; detidx < ndetections; detidx++) {
            TagDetection d = new TagDetection();
            d.good = ins.readBoolean();
            d.obsCode = ins.readLong();
            d.code = ins.readLong();
            d.id = ins.readInt();
            d.hammingDistance = ins.readInt();
            d.rotation = ins.readInt();
            d.p = new double[4][2];
            for (int i = 0; i < 4; i++)
                for (int j = 0; j < 2; j++)
                    d.p[i][j] = ins.readDouble();
            d.cxy = new double[2];
            for (int i = 0; i < 2; i++)
                d.cxy[i] = ins.readDouble();
            d.observedPerimeter = ins.readDouble();
            d.homography = new double[3][3];
            for (int i = 0; i < 3; i++)
                for (int j = 0; j < 3; j++)
                    d.homography[i][j] = ins.readDouble();
            d.hxy = new double[2];
            for (int i = 0; i < 2; i++)
                d.hxy[i] = ins.readDouble();

            detections.add(d);
        }

        return detections;
    }

    public void close()
    {
        try {
            sock.close();
        } catch (IOException ex) {
        }
    }

    public static void main(String args[])
    {
        try {
            BufferedImage im = ImageIO.read(new File(args[0]));

            TagExampleClient client = new TagExampleClient(new Tag36h11(), "localhost");

            ArrayList<TagDetection> detections = client.process(im, new double[] { im.getWidth()/ 2.0,
                                                                                   im.getHeight()/2.0 });
            System.out.printf("Got %d detections\n", detections.size());
            for (TagDetection td: detections)
                System.out.println(td);

            client.close();

        } catch (IOException ex) {
            System.out.println("Ex: "+ex);
        }
    }
}
