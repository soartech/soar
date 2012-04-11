package april.viewer;

import java.awt.event.*;
import java.io.*;
import java.net.*;

import april.config.*;
import april.vis.*;

/** Allows VisCanvas to send events directly to a lcm-logplayer-gui **/
public class RemoteLogEventHandler extends VisCanvasEventAdapter implements ViewObject
{
    VisCanvas vc;
    String name;
    Config config;

    public RemoteLogEventHandler(VisCanvas vc, String name, Config config)
    {
        this.vc = vc;
        this.name = name;
        this.config = config;
        vc.addEventHandler(this, 0);
    }

    public RemoteLogEventHandler(Viewer viewer, String name, Config config)
    {
        this(viewer.getVisCanvas(), name, config);
    }

    public String getName()
    {
        return "RemoteLogEventHandler";
    }

    static void udpSendString(String addr, int port, String s)
    {
        try
        {
            DatagramSocket sock = new DatagramSocket();
            byte b[] = s.getBytes();
            DatagramPacket p = new DatagramPacket(b, b.length, InetAddress.getByName(addr), port);
            sock.send(p);
        } catch (IOException ex)
        {
            System.out.println("Ex: " + ex);
        }
    }

    public boolean keyTyped(VisCanvas vc, KeyEvent e)
    {
        switch (e.getKeyChar())
        {
            case 'P':
            case 'p':
                udpSendString("127.0.0.1", 53261, "PLAYPAUSETOGGLE");
                break;
            case 'N':
            case 'n':
                udpSendString("127.0.0.1", 53261, "STEP");
                break;
            case '=':
            case '+':
                udpSendString("127.0.0.1", 53261, "FASTER");
                break;
            case '_':
            case '-':
                udpSendString("127.0.0.1", 53261, "SLOWER");
                break;
            case '[':
                udpSendString("127.0.0.1", 53261, "BACK5");
                break;
            case ']':
                udpSendString("127.0.0.1", 53261, "FORWARD5");
                break;
            case '{':
                udpSendString("127.0.0.1", 53261, "BACK60");
                break;
            case '}':
                udpSendString("127.0.0.1", 53261, "FORWARD60");
                break;
            default:
                return false;
        }
        return true;
    }
}
