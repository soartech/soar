package april.util;

import java.awt.*;
import java.awt.image.*;
import java.awt.geom.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.util.*;

/** JComponent used to view sets of images. **/
public class JImageAlbum extends JPanel
{
    JList jlist;
    public JImage jim;
    MyListModel model;

    class ImageRecord
    {
        String name;
        BufferedImage im;
    }

    ArrayList<ImageRecord> images = new ArrayList<ImageRecord>();

    public JImageAlbum()
    {
        model = new MyListModel();
        jlist = new JList(model);
        jlist.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        jim = new JImage();

        setLayout(new BorderLayout());
        JSplitPane jsp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, new JScrollPane(jlist), jim);

        add(jsp, BorderLayout.CENTER);

        jlist.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e)
            {
                update();
            }
	    });

        jsp.setDividerLocation(100);
        jsp.setResizeWeight(0);
    }

    public void setFit(boolean fit)
    {
        jim.setFit(fit);
    }

    public int getSelectedIndex()
    {
        return jlist.getSelectedIndex();
    }

    public void setSelectedIndex(int i)
    {
        jlist.setSelectedIndex(i);
    }

    public BufferedImage getImage(int idx)
    {
        ImageRecord ir = (ImageRecord) model.getElementAt(idx);
        return ir.im;
    }

    public int getNumImages()
    {
        return model.getSize();
    }

    public synchronized void clear()
    {
        model.clear();
        jim.setImage(null);
    }

    public synchronized void addImage(String name, BufferedImage im)
    {
        model.addImage(name, im);
    }

    public synchronized void setImage(String name, BufferedImage im)
    {
        model.setImage(name, im);
    }

    void update()
    {
        int idx = jlist.getSelectedIndex();

        if (idx >= 0 && idx < images.size())
            jim.setImage(images.get(idx).im);
    }

    class MyListModel extends AbstractListModel
    {
        public Object getElementAt(int index)
        {
            return images.get(index).name;
        }

        public int getSize()
        {
            return images.size();
        }

        public void setImage(String name, BufferedImage im)
        {
            boolean found = false;

            for (ImageRecord record : images) {
                if (record.name.equals(name)) {
                    record.im = im;
                    found = true;
                    break;
                }
            }

            if (!found) {
                addImage(name, im);
            }

            update();
        }

        public void addImage(String name, BufferedImage im)
        {
            ImageRecord record = new ImageRecord();
            record.name = name;
            record.im = im;

            images.add(record);

            model.fireIntervalAdded(model, images.size()-1, images.size()-1);

            if (images.size()==1)
                jlist.setSelectedIndex(0);
        }

        public void clear()
        {
            int lastidx = images.size()-1;
            images.clear();
            if (lastidx > 0)
                model.fireIntervalRemoved(model, 0, lastidx);
        }
    }
}
