package april.jcam;

import april.util.*;

import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.imageio.*;

import java.io.*;
import java.util.*;

/** JCam example application. **/
public class JCamView
{
    JFrame jf;
    JImage jim;
    JList  cameraList;
    JList  formatList;
    JPanel featurePanel;
    JSlider whitebalance_b;
    JSlider whitebalance_r;

    ImageSource isrc;

    JLabel infoLabel = new JLabel("(please wait)");

    ArrayList<String> urls;
    RunThread runThread;

    MyMouseListener mouseListener;

    JPanel mainPanel;
    JPanel leftPanel;

    RecordPanel recordPanel = new RecordPanel();

    public JCamView(ArrayList<String> urls)
    {
        this.urls = urls;

        jf = new JFrame("JCamView");
        jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        jf.setLayout(new BorderLayout());

        jim = new JImage();
        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(jim, BorderLayout.CENTER);

        mouseListener = new MyMouseListener();
        jim.addMouseMotionListener(mouseListener);

        cameraList = new JList(urls.toArray(new String[0]));
        formatList = new JList(new String[0]);
        featurePanel = new JPanel();

        cameraList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                cameraChanged();
            }
	    });

        formatList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        formatList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                formatChanged();
            }
	    });

        cameraList.setSelectedIndex(0); // will trigger call to cameraChanged().

        infoLabel.setFont(new Font("Monospaced", Font.PLAIN, 12));

        leftPanel = new JPanel();
        int vspace = 15;
        leftPanel.setLayout(new VFlowLayout());
        cameraList.setFixedCellWidth(40); // prevent long camera urls from
                                          // screwing up the panel width
        leftPanel.add(makeChoicePanel("Cameras", cameraList));
        leftPanel.add(Box.createVerticalStrut(vspace));
        leftPanel.add(makeChoicePanel("Formats", formatList));
        leftPanel.add(Box.createVerticalStrut(vspace));
        leftPanel.add(makeChoicePanel("Controls", featurePanel));
        leftPanel.add(Box.createVerticalStrut(vspace));
        leftPanel.add(makeChoicePanel("Record", recordPanel));

        JPanel bottomPanel = new JPanel();
        bottomPanel.setLayout(new FlowLayout());
        bottomPanel.add(infoLabel);
        mainPanel.add(bottomPanel, BorderLayout.SOUTH);

        JSplitPane jsp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(leftPanel), mainPanel);
        jsp.setDividerLocation(30 + (int) leftPanel.getPreferredSize().getWidth());
        jsp.setResizeWeight(0.2);

        jf.add(jsp, BorderLayout.CENTER);

        int width = Math.min(1000,
                             800 + (int) leftPanel.getPreferredSize().getWidth() + 30);
        jf.setSize(width,600);
        jf.setVisible(true);
    }

    JPanel indentPanel(JComponent c)
    {
        JPanel jp = new JPanel();
        jp.setLayout(new BorderLayout());
        jp.add(c, BorderLayout.CENTER);
        jp.add(Box.createHorizontalStrut(20), BorderLayout.WEST);
        return jp;
    }

    JPanel makeChoicePanel(String title, JComponent c)
    {
        JPanel jp = new JPanel();
        jp.setLayout(new BorderLayout());
        jp.add(c, BorderLayout.CENTER);
        jp.setBorder(BorderFactory.createTitledBorder(title));
        return jp;
    }

    class RecordPanel extends JPanel implements ActionListener, ChangeListener
    {
        JTextField pathBox = new JTextField("/tmp/jcam-capture/camera.islog");

        JButton startStopButton = new JButton("Record");

        boolean recording = false;

        JSlider fpsSlider = new JSlider(1, 100, 10);
        JLabel fpsLabel = new JLabel("");

        long firstms;
        long lastms = System.currentTimeMillis();
        int framesWritten = 0;
        JLabel framesWrittenLabel = new JLabel("");

        JComboBox formatComboBox = new JComboBox(new String[] { "Log file" , "Individual PNGs" });

        ISLogWriter logWriter;

        public RecordPanel()
        {
            setLayout(new VFlowLayout());

            add(new JLabel("Maximum frame rate: "));
            JPanel fpsPanel = new JPanel();
            fpsPanel.setLayout(new BorderLayout());
            fpsPanel.add(fpsSlider, BorderLayout.CENTER);
            fpsPanel.add(fpsLabel, BorderLayout.EAST);
            fpsSlider.addChangeListener(this);
            add(indentPanel(fpsPanel));

            add(new JLabel("Format: "));
            add(indentPanel(formatComboBox));

            add(new JLabel("Destination path: "));
            add(indentPanel(pathBox));


            add(Box.createVerticalStrut(10));
            add(startStopButton);

            add(framesWrittenLabel);

            stateChanged(null);
            startStopButton.addActionListener(this);
        }

        public void stateChanged(ChangeEvent e)
        {
            fpsLabel.setText(String.format("%4d", fpsSlider.getValue()));
        }

        public synchronized void actionPerformed(ActionEvent e)
        {
            recording ^= true;

            if (recording) {
                framesWritten = 0;
                firstms = System.currentTimeMillis();

                if (formatComboBox.getSelectedIndex() == 0) {
                    new File(pathBox.getText()).getParentFile().mkdirs();

                    // log format
                    try {
                        logWriter = new ISLogWriter(new FileOutputStream(pathBox.getText()));
                    } catch (IOException ex) {
                        recording = false;
                        System.out.println("ex: "+ex);
                        return;
                    }

                } else {
                    new File(pathBox.getText()).mkdirs();

                    // directory of PNGs

                    // nothing to do here.
                }

                startStopButton.setText("Stop");
                pathBox.setEnabled(false);
                formatComboBox.setEnabled(false);
            } else {
                if (formatComboBox.getSelectedIndex() == 0) {
                    try {
                        logWriter.close();
                    } catch (IOException ex) {
                        System.out.println("ex: "+ex);
                    }
                } else {
                }

                startStopButton.setText("Record");
                formatComboBox.setEnabled(true);
                pathBox.setEnabled(true);
            }
        }

        public synchronized void handleImage(BufferedImage im, ImageSourceFormat ifmt, byte imbuf[])
        {
            if (!recording)
                return;

            long nowms = System.currentTimeMillis();
            long dms = 1000 / fpsSlider.getValue();

            if (nowms >= lastms + dms) {
                lastms = nowms;

                try {
                    if (formatComboBox.getSelectedIndex() == 0) {
                        logWriter.write(ifmt, imbuf);
                    } else {
                        String path = String.format("%s/image_%06d.png", pathBox.getText(), nowms - firstms);
                        ImageIO.write(im, "png", new File(path));
                        framesWritten++;
                        framesWrittenLabel.setText(String.format("%d frames written", framesWritten));
                    }

                } catch (IOException ex) {
                    System.out.println("Ex: "+ex);
                }
            }
        }
    }

    class MyMouseListener implements MouseMotionListener
    {
        int imx, imy;

        public void mouseMoved(MouseEvent e)
        {
            Point2D xy = jim.componentToImage(e.getPoint());
            BufferedImage im = jim.getImage();

            imx = (int) xy.getX();
            imy = (int) xy.getY();
        }

        public void mouseDragged(MouseEvent e)
        {
        }
    }

    class FeatureControl extends JPanel implements ChangeListener, ActionListener
    {
        String name;
        double min, max, value;
        double scale = 100;
        ImageSource isrc;
        int idx;

        JLabel valueLabel = new JLabel();
        JSlider js;
        JCheckBox jcb;

        public FeatureControl(ImageSource isrc, int idx)
        {
            this.isrc = isrc;
            this.idx = idx;

            min = isrc.getFeatureMin(idx);
            max = isrc.getFeatureMax(idx);
            value = isrc.getFeatureValue(idx);

            setLayout(new BorderLayout());

            if (min==0 && max==1) {
                jcb = new JCheckBox(isrc.getFeatureName(idx), value==1);
                jcb.addActionListener(this);
                add(jcb, BorderLayout.CENTER);
            } else {
                js = new JSlider((int) (min*scale), (int) (max*scale), (int) (value*scale));
                js.addChangeListener(this);
                add(new JLabel(isrc.getFeatureName(idx)), BorderLayout.NORTH);
                add(js, BorderLayout.CENTER);
                add(valueLabel, BorderLayout.EAST);
            }

            updateLabel();
        }

        void updateLabel()
        {
            value = isrc.getFeatureValue(idx);
            if (value == (int) value)
                valueLabel.setText(""+value);
            else
                valueLabel.setText(String.format("%.2f", value));
        }

        public void actionPerformed(ActionEvent e)
        {
            if (jcb != null) {
                int res = isrc.setFeatureValue(idx, jcb.isSelected() ? 1 : 0);
                if (res != 0)
                    System.out.println("Error setting feature");
            }
            updateLabel();
        }

        public void stateChanged(ChangeEvent e)
        {
            if (js != null) {
                int res = isrc.setFeatureValue(idx, js.getValue()/scale);
                if (res != 0)
                    System.out.println("Error setting feature");
            }

            updateLabel();
        }
    }

    synchronized void cameraChanged()
    {
        stopRunThread();

        String url = urls.get(cameraList.getSelectedIndex());
        System.out.println("set camera: "+url);

        if (isrc != null)
            isrc.close();

        try {
            isrc = ImageSource.make(url);
        } catch (IOException ex) {
            System.out.println("Ex: "+ex);
            return;
        }

        if (true) {
            featurePanel.removeAll();
            int nfeatures = isrc.getNumFeatures();
            featurePanel.setLayout(new GridLayout(nfeatures, 1));

            for (int i = 0; i < nfeatures; i++) {
//                System.out.printf("Feature %d : %s (%f, %f, %f)\n", i, isrc.getFeatureName(i), isrc.getFeatureMin(i), isrc.getFeatureMax(i), isrc.getFeatureValue(i));

                featurePanel.add(new FeatureControl(isrc, i));
            }
        }

        // update the list of formats.
        ArrayList<String> fmts = new ArrayList<String>();
        for (int i = 0; i < isrc.getNumFormats(); i++) {
            ImageSourceFormat fmt = isrc.getFormat(i);
            fmts.add(String.format("%d x %d (%s)", fmt.width, fmt.height, fmt.format));
        }

        formatList.setListData(fmts.toArray(new String[0]));

        formatList.setSelectedIndex(isrc.getCurrentFormatIndex()); // will trigger call to formatChanged.
    }

    synchronized void formatChanged()
    {
        stopRunThread();

        int idx = formatList.getSelectedIndex();

        System.out.println("set format "+idx);
        if (idx < 0)
            return;

        isrc.setFormat(idx);

        runThread = new RunThread();
        runThread.start();
    }

    synchronized void stopRunThread()
    {
        if (runThread != null)  {
            runThread.stopRequest = true;
            try {
                runThread.join();
            } catch (InterruptedException ex) {
                System.out.println("ex: "+ex);
            }
            runThread = null;
        }
    }

    class RunThread extends Thread
    {
        boolean stopRequest = false;

        public void run()
        {
            isrc.start();
            // use seconds per frame, not frames per second, because
            // linearly blending fps measures gives funny
            // results. E.g., suppose frame 1 takes 1 second, frame 2
            // takes 0 seconds. Averaging fps would give INF,
            // averaging spf would give 0.5. As a (related) plus,
            // there's no risk of a divide-by-zero.
            double spf = 0;
            double spfAlpha = 0.95;

            ImageSourceFormat ifmt = isrc.getCurrentFormat();
            long last_frame_mtime = System.currentTimeMillis();

            long last_info_mtime = System.currentTimeMillis();

            while (!stopRequest) {
                byte imbuf[] = null;
                BufferedImage im = null;

                imbuf = isrc.getFrame();
                if (imbuf == null) {
                    System.out.println("getFrame() failed");
                    continue;
                }

                im = ImageConvert.convertToImage(ifmt.format, ifmt.width, ifmt.height, imbuf);
                if (im == null)
                    continue;

                jim.setImage(im);
                recordPanel.handleImage(im, ifmt, imbuf);

                if (true) {
                    long frame_mtime = System.currentTimeMillis();
                    double dt = (frame_mtime - last_frame_mtime)/1000.0;
                    spf = spf*spfAlpha + dt*(1.0-spfAlpha);
                    last_frame_mtime = frame_mtime;

                    int rgb = 0;
                    if (mouseListener != null && mouseListener.imx >= 0 && mouseListener.imx < im.getWidth() && mouseListener.imy >= 0 && mouseListener.imy < im.getHeight())
                        rgb = im.getRGB(mouseListener.imx, mouseListener.imy) & 0xffffff;

                    if (frame_mtime - last_info_mtime > 100) {
                        infoLabel.setText(String.format("fps: %6.2f, RGB: %02x %02x %02x", 1.0/(spf+0.00001), (rgb>>16)&0xff, (rgb>>8)&0xff, rgb&0xff));
                        last_info_mtime = frame_mtime;
                    }

                    last_frame_mtime = frame_mtime;
                }

                if (imbuf == null) {
                    System.out.println("get frame failed");
                    continue;
                }

                Thread.yield();
            }

            isrc.stop();
        }
    }

    public static void main(String args[])
    {
        System.out.println("java.library.path: "+System.getProperty("java.library.path"));

        ArrayList<String> urls = new ArrayList<String>();

        for (String arg : args)
            urls.add(arg);

        if (urls.size()==0) {
            System.out.println("Found cameras: ");
            for (String s : ImageSource.getCameraURLs()) {
                System.out.println("  "+s);
                urls.add(s);
            }
        }

        if (urls.size() == 0) {
            System.out.println("No image sources found or specified on command line.");
            return;
        }

        new JCamView(urls);
    }
}
