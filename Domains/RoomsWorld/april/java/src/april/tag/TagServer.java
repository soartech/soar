package april.tag;

import java.awt.image.*;
import java.util.*;
import java.io.*;
import java.net.*;

import april.util.*;

/** A server that detects tags over a TCP connection. **/
public class TagServer
{
    public static final int TCP_PORT = 15339;

    public TagServer()
    {
        new AcceptThread().start();
    }

    class AcceptThread extends Thread
    {
        public void run()
        {
            try {
                ServerSocket serverSock = new ServerSocket(TCP_PORT);

                while (true) {
                    Socket sock = serverSock.accept();
                    new ClientThread(sock).start();
                }
            } catch (IOException ex) {
                System.out.println("ex: "+ex);
                System.exit(1);
            }
        }
    }

    class ClientThread extends Thread
    {
        Socket sock;

        public ClientThread(Socket sock)
        {
            this.sock = sock;
        }

        public void run()
        {
            try {
                DataInputStream ins = new DataInputStream(new BufferedInputStream(sock.getInputStream()));
                DataOutputStream outs = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));

                System.out.println("Client connected");

                while (true) {
                    Tic tic = new Tic();

                    //////////////////////////////////////////////////////
                    // read image/request

                    String tagFamilyClass = ins.readUTF();
                    int width = ins.readInt();
                    int height = ins.readInt();
                    double opticalCenter[] = new double[] { ins.readDouble(),
                                                            ins.readDouble() };

                    // read grayscale image, one byte per pixel
                    BufferedImage im = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY);
                    byte b[] = ((DataBufferByte) (im.getRaster().getDataBuffer())).getData();

                    ins.readFully(b);

                    TagFamily tf = (TagFamily) ReflectUtil.createObject(tagFamilyClass);
                    if (tf == null)
                        break;

                    TagDetector td = new TagDetector(tf);

                    ArrayList<TagDetection> detections = td.process(im, opticalCenter);

                    //////////////////////////////////////////////////////
                    // output results
                    outs.writeInt(detections.size());
                    for (TagDetection d : detections) {
                        outs.writeBoolean(d.good);
                        outs.writeLong(d.obsCode);
                        outs.writeLong(d.code);
                        outs.writeInt(d.id);
                        outs.writeInt(d.hammingDistance);
                        outs.writeInt(d.rotation);
                        for (int i = 0; i < 4; i++)
                            for (int j = 0; j < 2; j++)
                                outs.writeDouble(d.p[i][j]);
                        for (int i = 0; i < 2; i++)
                            outs.writeDouble(d.cxy[i]);
                        outs.writeDouble(d.observedPerimeter);
                        for (int i = 0; i < 3; i++)
                            for (int j = 0; j < 3; j++)
                                outs.writeDouble(d.homography[i][j]);
                        for (int i = 0; i < 2; i++)
                            outs.writeDouble(d.hxy[i]);
                        outs.flush();
                    }

                    System.out.printf("   Processed %d x %d image, detected %d tags in %.2f ms\n", width, height, detections.size(), tic.toc() * 1000);
                }

                sock.close();
            } catch (EOFException ex) {
                System.out.println("Client disconnected");
            } catch (IOException ex) {
                System.out.println("ex: "+ex);
            }
        }
    }

    public static void main(String args[])
    {
        new TagServer();
    }
}
