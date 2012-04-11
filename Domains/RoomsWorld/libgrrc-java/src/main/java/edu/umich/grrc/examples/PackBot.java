package edu.umich.grrc.examples;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.SocketException;
import java.net.UnknownHostException;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeListener;

import edu.umich.grrc.GrrcPackBot;

public class PackBot
{
    private final GrrcPackBot pb;
    
    private final JFrame frame = new JFrame();

    private JSlider lvSlider = new JSlider(JSlider.HORIZONTAL, -100, 100, 0);

    private JSlider avSlider = new JSlider(JSlider.HORIZONTAL, -100, 100, 0);

    private float av = 0;
    
    private float lv = 0;
    
    private String hostname = "192.168.1.115";
    
    private int port = 3192;

    public PackBot(String[] args) throws UnknownHostException, SocketException
    {        
        if (args.length > 0)
            hostname = args[0];
        if (args.length > 1)
            port = Integer.valueOf(args[1]);
        
        pb = new GrrcPackBot(hostname, port);
        pb.setVelocities(0, 0);
        
        lvSlider.addChangeListener(lvListener);
        lvSlider.setMajorTickSpacing(50);
        lvSlider.setMinorTickSpacing(1);
        lvSlider.setPaintLabels(true);
        lvSlider.setPaintTicks(true);
        JPanel lvPanel = new JPanel();
        lvPanel.setLayout(new BorderLayout());
        lvPanel.add(new JLabel("Linear %"), BorderLayout.LINE_START);
        lvPanel.add(lvSlider, BorderLayout.CENTER);
        
        avSlider.addChangeListener(avListener);
        avSlider.setMajorTickSpacing(50);
        avSlider.setMinorTickSpacing(1);
        avSlider.setPaintLabels(true);
        avSlider.setPaintTicks(true);
        JPanel avPanel = new JPanel();
        avPanel.setLayout(new BorderLayout());
        avPanel.add(new JLabel("Angular %"), BorderLayout.LINE_START);
        avPanel.add(avSlider, BorderLayout.CENTER);
        
        frame.setLayout(new FlowLayout());
        frame.add(lvPanel);
        frame.add(avPanel);
        
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter()
        {
            @Override
            public void windowClosed(WindowEvent e)
            {
                pb.setVelocities(0, 0);
                try
                {
                    Thread.sleep(200);
                }
                catch (InterruptedException ex)
                {
                }
                pb.shutdown();
            }
        });
        frame.pack();
        frame.setVisible(true);
    }
    
    private final ChangeListener avListener = new ChangeListener() 
    {
        public void stateChanged(javax.swing.event.ChangeEvent e) 
        {
            av = Float.valueOf(Integer.valueOf(avSlider.getValue()) / 100.0f);
            System.out.println("av: " + av);
            pb.setVelocities(lv, av);
        }
    };

    private final ChangeListener lvListener = new ChangeListener() 
    {
        public void stateChanged(javax.swing.event.ChangeEvent e) 
        {
            lv = Float.valueOf(Integer.valueOf(lvSlider.getValue()) / 100.0f);
            System.out.println("lv: " + lv);
            pb.setVelocities(lv, av);
        }
    };

    public static void main(String[] args) throws UnknownHostException, SocketException
    {
        new PackBot(args);
    }
}
