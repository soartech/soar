package april.util;

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;

/** Provides a means of easily and rapidly manipulating parameters.

    Parameters typically have a shortname (which is passed to the
    parameterListener) and a description (which is displayed to the
    user.)

    By default, ParameterListeners are notified only in response to
    *USER GENERATED* messages: calls to the set methods should not
    result in callbacks. This behavior can be changed by calling
    setNotifyOnSet().
**/
public class ParameterGUI extends JPanel
{
    HashMap<String, PValue> parammap = new HashMap<String, PValue>();
    int     row = 0;
    GridBagConstraints gA, gB, gC, gD, gBC, gCD, gBCD, gABCD;
    ArrayList<ParameterListener> listeners = new ArrayList<ParameterListener>();

    // If setDepth > 0, we are programmatically setting a
    // parameter. We use this to inhibit notifyListeners. This is an
    // integer, rather than a flag, so that if one parameter ends up
    // setting another, we don't get confused. (But a flag would
    // probably suffice with the current implementation.)
    int setDepth = 0;

    // If true, we will attempt to supress parameterChanged()
    // callbacks as a result of a call to a set() method.
    boolean notifyOnSet = false;

    /** This text field becomes highlighted when the *user* changes
     * it. When the user hits return (generating an actionPerformed
     * event), the highlight is cleared. This highlighting is used to
     * indicate that a change is pending and that the users edits have
     * not yet taken effect. **/
    static class HighlightingTextField extends JTextField
    {
        public HighlightingTextField(String contents)
        {
            super(contents);
            addActionListener(new MyActionListener());
            setHighlight(false);
        }

        protected Document createDefaultModel()
        {
            return new MyDoc();
        }

        class MyDoc extends PlainDocument
        {
            public void insertString(int offs, String str, AttributeSet a) throws BadLocationException
            {
                setHighlight(true);
                super.insertString(offs, str, a);
            }

            public void remove(int offs, int len) throws BadLocationException
            {
                setHighlight(true);
                super.remove(offs, len);
            }
        }

        class MyActionListener implements ActionListener
        {
            public void actionPerformed(ActionEvent e)
            {
                setHighlight(false);
            }
        }

        public void setHighlight(boolean e)
        {
            if (e)
                setBackground(Color.yellow);
            else
                setBackground(Color.white);
        }

        public void setText(String s)
        {
            super.setText(s);
            setHighlight(false);
        }
    }

    /** Parent interface of all value elements **/
    abstract class PValue
    {
        String name;  // name of element, e.g., "logthresh"
        String desc;  // description, e.g., "Logarithmic threshold"

        PValue(String name, String desc)
        {
            this.name = name;
            this.desc = desc;
        }

        abstract void setEnabled(boolean enabled);
    }

    class ActionNotifier implements ActionListener
    {
        String name;

        public ActionNotifier(String name)
        {
            this.name = name;
        }

        public void actionPerformed(ActionEvent e)
        {
            notifyListeners(name);
        }
    }

    class BooleanValue extends PValue
    {
        JCheckBox  jcb;
        boolean value;

        BooleanValue(String name, String desc, boolean value)
        {
            super(name, desc);

            this.value = value;
        }

        JCheckBox getCheckBox()
        {
            if (jcb == null) {
                jcb = new JCheckBox(desc, value);
                jcb.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        setBooleanValue(jcb.isSelected(), true);
                    }});

            }
            return jcb;
        }

        boolean getBooleanValue()
        {
            return value;
        }

        void setBooleanValue(boolean v, boolean notifyIfChanged)
        {
            boolean changed = v != value;
            value = v;

            if (notifyIfChanged && changed)
                notifyListeners(name);
        }

        void setEnabled(boolean v)
        {
            jcb.setEnabled(v);
        }
    }

    class IntegerValue extends PValue
    {
        JSlider    slider;
        JTextField textField;
        JLabel     label;
        int        min, max;
        int        value;

        IntegerValue(String name, String desc, int min, int max, int value)
        {
            super(name, desc);

            this.min = min;
            this.max = max;
            this.value = value;
        }

        JSlider getSlider()
        {
            if (slider == null) {
                slider = new JSlider(min, max, value);
                slider.addChangeListener(new ChangeListener() {
                    public void stateChanged(ChangeEvent e) {
                        setIntegerValue(slider.getValue(), true);
                    }});
            }
            return slider;
        }

        JLabel getLabel()
        {
            if (label == null)
                label = new JLabel(""+value);
            return label;
        }

        JTextField getTextField()
        {
            if (textField == null) {
                textField = new HighlightingTextField(""+value);
                textField.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        try {
                            int v = Integer.parseInt(textField.getText());
                            setIntegerValue(v, true);
                        } catch (Exception ex) {
                            setIntegerValue(value, false);
                        }
                    }});
            }
            return textField;
        }

        int getIntegerValue()
        {
            return value;
        }

        void setIntegerValue(int v, boolean notifyIfChanged)
        {
            v = Math.min(max, Math.max(min, v));
            boolean changed = value != v;

            value = v;
            if (slider != null)
                slider.setValue(v);
            if (textField != null)
                textField.setText(""+v);
            if (label != null)
                label.setText(""+v);

            if (notifyIfChanged && changed)
                notifyListeners(name);
        }

        void setMinMax(int min, int max)
        {
            this.min = min;
            this.max = max;
            if (slider != null) {
                slider.setMinimum(min);
                slider.setMaximum(max);
            }
            setIntegerValue(value, true);
        }

        void setEnabled(boolean v)
        {
            if (slider!=null)
                slider.setEnabled(v);
            if (textField!=null)
                textField.setEnabled(v);
        }
    }

    class DoubleValue extends PValue
    {
        JSlider    slider;
        JTextField textField;
        JLabel     label;
        double     min, max;
        double     value;
        static final int SLIDER_CLICKS = 100000;

        DoubleValue(String name, String desc, double min, double max, double value)
        {
            super(name, desc);

            this.min = min;
            this.max = max;
            this.value = value;

            updateSliderValue();
        }

        void updateSliderValue()
        {
            int ivalue = (int) (SLIDER_CLICKS*(value-min)/(max-min));
            if (slider != null)
                slider.setValue(ivalue);
        }

        JSlider getSlider()
        {
            if (slider == null) {
                slider = new JSlider(0, SLIDER_CLICKS, 0);
                updateSliderValue();

                slider.addChangeListener(new ChangeListener() {
                    public void stateChanged(ChangeEvent e) {
                        double sv = ((double) slider.getValue())/SLIDER_CLICKS*(max-min)+min;
                        setDoubleValue(sv, true);
                    }});
            }
            return slider;
        }

        JLabel getLabel()
        {
            if (label == null)
                label = new JLabel(""+value);
            return label;
        }

        JTextField getTextField()
        {
            if (textField == null) {
                textField = new HighlightingTextField(""+value);
                textField.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        try {
                            double v = Double.parseDouble(textField.getText());
                            setDoubleValue(v, true);
                        } catch (Exception ex) {
                            setDoubleValue(value, false);
                        }
                    }});
            }

            return textField;
        }

        double getDoubleValue()
        {
            return value;
        }

        void setDoubleValue(double v, boolean notifyIfChanged)
        {
            v = Math.min(max, Math.max(min, v));
            boolean changed = value != v;

            value = v;
            updateSliderValue();

            int log10 = (int) (Math.log(value)/Math.log(10));
            int decimalPlaces = Math.max(0, 6 - log10);
            String svalue=String.format("%."+decimalPlaces+"f", value);

            if (textField != null)
                textField.setText(svalue);

            if (label != null)
                label.setText(svalue);

            if (notifyIfChanged && changed)
                notifyListeners(name);
        }

        void setMinMax(double min, double max)
        {
            this.min = min;
            this.max = max;

            setDoubleValue(value, false);
        }

        void setEnabled(boolean v)
        {
            if (slider!=null)
                slider.setEnabled(v);
            if (textField!=null)
                textField.setEnabled(v);
        }
    }

    class StringValue extends PValue
    {
        JTextField textField;
        JComboBox  comboBox;

        String     value;
        String     values[];
        int        idx; // if values is set, idx is such that value=values[idx].

        public StringValue(String name, String desc, String values[], String value)
        {
            super(name, desc);
            this.value = value;
            this.values = values;
            updateIndex();
        }

        JTextField getTextField()
        {
            if (textField == null) {
                textField = new HighlightingTextField(value);
                textField.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        String v = textField.getText();
                        setStringValue(v, true);
                    }});
            }
            return textField;
        }

        JComboBox getComboBox()
        {
            assert(values != null);

            if (comboBox == null) {
                comboBox = new JComboBox(values);
                comboBox.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        setStringValue(values[comboBox.getSelectedIndex()], true);
                    }});
                comboBox.setSelectedIndex(idx);
            }

            return comboBox;
        }

        String getStringValue()
        {
            return value;
        }

        void updateIndex()
        {
            if (values!=null) {
                idx = -1;

                for (int i = 0; i < values.length; i++) {
                    if (value.equals(values[i]))
                        idx = i;
                }

                if (idx == -1)
                    System.out.println("Warning: illegal string value specified: "+value);
            }
        }

        void setStringValue(String v, boolean notifyIfChanged)
        {
            boolean changed = !v.equals(value);
            value = v;
            updateIndex();

            if (textField != null)
                textField.setText(v);
            if (comboBox != null)
                comboBox.setSelectedIndex(idx);

            if (notifyIfChanged && changed)
                notifyListeners(name);
        }

        void setEnabled(boolean v)
        {
            if (textField!=null)
                textField.setEnabled(v);
        }
    }

    // Our grid:
    //
    //    0                1                   2       3
    //  AAAAA  BBBBBBBBBBBBBBBBBBBBBBBBBBBB  CCCCCC  DDDDDD
    //    gA               gB                  gC      gD
    //
    //  gCD spans columns C and D.
    //  gBCD spans columsn B, C, and D. (etc)

    /** Create a new ParameterGUI **/
    public ParameterGUI()
    {
        super(new GridBagLayout());

        gA = new GridBagConstraints();
        gA.gridx = 0;
        gA.weightx = 0.05;
        gA.fill=GridBagConstraints.HORIZONTAL;

        gB = new GridBagConstraints();
        gB.gridx = 1;
        gB.weightx = 1;
        gB.fill=GridBagConstraints.HORIZONTAL;

        gC = new GridBagConstraints();
        gC.gridx = 2;
        gC.weightx = .1;
        gC.fill=GridBagConstraints.HORIZONTAL;

        gD = new GridBagConstraints();
        gD.gridx = 3;
        gD.weightx = .05;
        gD.fill=GridBagConstraints.HORIZONTAL;
        gD.anchor = GridBagConstraints.CENTER;

        gBC = new GridBagConstraints();
        gBC.gridx = 1;
        gBC.gridwidth = 2;
        gBC.weightx = gB.weightx + gC.weightx;
        gBC.fill=GridBagConstraints.HORIZONTAL;

        gCD = new GridBagConstraints();
        gCD.gridx = 2;
        gCD.gridwidth = 2;
        gCD.weightx = gC.weightx + gD.weightx;
        gCD.fill=GridBagConstraints.HORIZONTAL;
        gCD.anchor = GridBagConstraints.EAST;

        gBCD = new GridBagConstraints();
        gBCD.gridx = 1;
        gBCD.gridwidth = 3;
        gBCD.weightx = gB.weightx + gC.weightx + gD.weightx;
        gBCD.fill=GridBagConstraints.HORIZONTAL;
        gBCD.anchor = GridBagConstraints.EAST;

        gABCD = new GridBagConstraints();
        gABCD.gridx = 0;
        gABCD.gridwidth = 4;
        gABCD.weightx = gA.weightx + gB.weightx + gC.weightx + gD.weightx;
        gABCD.fill=GridBagConstraints.HORIZONTAL;

        this.add(Box.createHorizontalStrut(80), gD);
        row++;
    }

    /** Notify all listeners that parameter 'name' has changed. **/
    public void notifyListeners(String name)
    {
        if (setDepth > 0 && !notifyOnSet)
            return;

        for (ParameterListener pl : listeners)
            pl.parameterChanged(this, name);
    }

    public void addListener(ParameterListener l)
    {
        listeners.add(l);
    }

    /** Remove all listeners. **/
    public void removeListeners()
    {
        listeners.clear();
    }

    /** Add an integer parameter. **/
    public void addInt(String name, String desc, int value)
    {
        addInt(name, desc, -Integer.MAX_VALUE, Integer.MAX_VALUE, value);
    }

    /** Add an integer parameter. **/
    public void addInt(String name, String desc, int min, int max, int value)
    {
        IntegerValue val = new IntegerValue(name, desc, min, max, value);
        parammap.put(name, val);

        gA.gridy = row;
        gBCD.gridy = row;
        this.add(new JLabel(desc), gA);
        this.add(val.getTextField(), gBCD);
        row++;
    }

    /** Add an integer parameter with a slider. **/
    public void addIntSlider(String name, String desc, int min, int max, int value)
    {
        IntegerValue val = new IntegerValue(name, desc, min, max, value);
        parammap.put(name, val);

        gA.gridy = row;
        gBC.gridy = row;
        gD.gridy = row;

        this.add(new JLabel(desc), gA);
        this.add(val.getSlider(), gBC);
        this.add(val.getTextField(), gD);
        row++;
    }

    /** Add a double parameter. **/
    public void addDouble(String name, String desc, double value)
    {
        addDouble(name, desc, -Double.MAX_VALUE, Double.MAX_VALUE, value);
    }

    /** Add a double parameter. **/
    public void addDouble(String name, String desc, double min, double max, double value)
    {
        DoubleValue val = new DoubleValue(name, desc, min, max, value);
        parammap.put(name, val);

        gA.gridy = row;
        gBCD.gridy = row;
        this.add(new JLabel(desc), gA);
        this.add(val.getTextField(), gBCD);
        row++;
    }

    /** Add a double parameter with a slider. **/
    public void addDoubleSlider(String name, String desc, double min, double max, double value)
    {
        DoubleValue val = new DoubleValue(name, desc, min, max, value);
        parammap.put(name, val);

        gA.gridy = row;
        gBC.gridy = row;
        gD.gridy = row;

        this.add(new JLabel(desc), gA);
        this.add(val.getSlider(), gBC);
        this.add(val.getTextField(), gD);
        row++;
    }

    /** Add a string-valued parameter. **/
    public void addString(String name, String desc, String value)
    {
        StringValue val = new StringValue(name, desc, null, value);
        parammap.put(name, val);

        gA.gridy = row;
        gBCD.gridy = row;

        this.add(new JLabel(desc), gA);
        this.add(val.getTextField(), gBCD);
        row++;
    }

    /** Add a combo-box whose value is an integer representing the
     * index into the values array.
     **/
    public void addChoice(String name, String desc, String values[], int value)
    {
        StringValue val = new StringValue(name, desc, values, values[value]);
        parammap.put(name, val);

        gA.gridy = row;
        gBCD.gridy = row;

        this.add(new JLabel(desc), gA);
        this.add(val.getComboBox(), gBCD);
        row++;
    }

    /** Add a checkbox item. **/
    public void addBoolean(String name, String desc, boolean value)
    {
        BooleanValue val = new BooleanValue(name, desc, value);
        parammap.put(name, val);

        gA.gridy = row;
        gBCD.gridy = row;
        this.add(new JLabel(desc), gA);
        this.add(val.getCheckBox(), gBCD);
        row++;
    }

    /** Add one or more buttons, with each button consisting of two
     * arguments: a shortname (string) and a description (string). The
     * description is displayed in the button itself. **/
    public void addButtons(Object... args)
    {
        JPanel p = new JPanel();
        p.setLayout(new GridLayout(1, args.length/2));

        for (int i = 0; i < args.length/2; i++) {

            String name = (String) args[i*2];
            String desc = (String) args[i*2+1];

            JButton button = new JButton(desc);
            button.addActionListener(new ActionNotifier(name));

            p.add(button);
        }

        gABCD.gridy = row;
        this.add(p, gABCD);
        row++;
    }

    /** Add one or more checkboxes, with each checkbox consisting of
     * three arguments: shortname (string), description (string),
     * current state (boolean) **/
    public void addCheckBoxes(Object... args)
    {
        JPanel p = new JPanel();
        p.setLayout(new GridLayout(1, args.length/3));

        for (int i = 0; i < args.length/3; i++) {
            String name = (String) args[i*3+0];
            String desc = (String) args[i*3+1];
            boolean value = (Boolean) args[i*3+2];

            BooleanValue bv = new BooleanValue(name, desc, value);
            parammap.put(name, bv);

            p.add(bv.getCheckBox());
        }

        gABCD.gridy = row;
        this.add(p, gABCD);
        row++;
    }

    /** Get a double-valued parameter **/
    public double gd(String name)
    {
        PValue p = parammap.get(name);
        assert(p!=null);
        assert(p instanceof DoubleValue);

        return ((DoubleValue) p).getDoubleValue();
    }

    /** Set a double-valued parameter **/
    public void sd(String name, double v)
    {
        setDepth++;

        PValue p = parammap.get(name);
        assert(p!=null);
        assert(p instanceof DoubleValue);

        ((DoubleValue) p).setDoubleValue(v, false);

        setDepth--;
    }

    /** Get an integer-valued parameter **/
    public int gi(String name)
    {
        PValue p = parammap.get(name);
        assert(p!=null);

        if (p instanceof StringValue)
            return ((StringValue) p).idx;

        assert(p instanceof IntegerValue);

        return ((IntegerValue) p).getIntegerValue();
    }

    /** Set an integer-valued parameter **/
    public void si(String name, int v)
    {
        setDepth++;

        PValue p = parammap.get(name);
        assert(p!=null);

        if (p instanceof IntegerValue) {
            ((IntegerValue) p).setIntegerValue(v, false);
        } else {
            assert(false);
        }

        setDepth--;
    }

    /** Set the minimum and maximum values for an integer or double
     * valued parameter. **/
    public void setMinMax(String name, int min, int max)
    {
        PValue p = parammap.get(name);
        assert(p!=null);

        if (p instanceof DoubleValue) {
            ((DoubleValue) p).setMinMax(min, max);
        } else if (p instanceof IntegerValue) {
            ((IntegerValue) p).setMinMax(min, max);
        } else {
            // can't set this value.
            assert(false);
        }
    }

    /** Set the minimum and maximum values for a double valued
     * parameter. **/
    public void setMinMax(String name, double min, double max)
    {
        PValue p = parammap.get(name);
        assert(p!=null);
        assert(p instanceof DoubleValue);

        ((DoubleValue) p).setMinMax(min, max);
    }

    /** Get a string-valued parameter. **/
    public String gs(String name)
    {
        PValue p = parammap.get(name);
        assert(p!=null);
        assert(p instanceof StringValue);

        return ((StringValue) p).getStringValue();
    }

    /** Set a string-valued parameter. **/
    public void ss(String name, String v)
    {
        setDepth++;

        PValue p = parammap.get(name);
        assert(p!=null);
        assert(p instanceof StringValue);

        ((StringValue) p).setStringValue(v, false);

        setDepth--;
    }

    /** Get a boolean-valued parameter. **/
    public boolean gb(String name)
    {
        PValue p = parammap.get(name);
        assert(p!=null);
        assert(p instanceof BooleanValue);

        return ((BooleanValue) p).getBooleanValue();
    }

    /** Set a boolean-valued parameter **/
    public void sb(String name, boolean v)
    {
        setDepth++;

        PValue p = parammap.get(name);
        assert(p!=null);
        assert(p instanceof BooleanValue);

        ((BooleanValue) p).setBooleanValue(v, false);
        setDepth--;
    }

    /** Enable or disable a parameter. **/
    public void setEnabled(String name, boolean e)
    {
        PValue p = parammap.get(name);
        assert(p!=null);
        p.setEnabled(e);
    }

    /** Deprecated. **/
    public Component getPanel()
    {
        return this;
    }

    /** If set to true, calls to a set method (ss, sd, si, etc.) will
     * result in callbacks on the ParameterListeners. When set to
     * false, only user interactions with the GUI widgets generate
     * callbacks. The default behavior is false (only user
     * interactions generate callbacks). **/
    public void setNotifyOnSet(boolean e)
    {
        notifyOnSet = e;
    }

    public static void main(String[] args)
    {
        JFrame f = new JFrame("ParameterGUI Test");
        f.setLayout(new BorderLayout());
        f.setSize(400,400);

        ParameterGUI pg = new ParameterGUI();

        pg.addDouble("Double Value", "A double value", 3.1415926);
        pg.addInt("IntValue1", "An integer value", 45);
        pg.addIntSlider("IntValue2", "An integer value", 100, 1000, 999);
        pg.addDoubleSlider("DoubleSlider", "A sliding double", -1, 1, 0.5);
        pg.addDoubleSlider("DoubleSlider2", "Another sliding double",2500000, 3000000, 2500000);

        pg.addString("StringValue1", "A string value", "Hi");
        pg.addString("StringValue2", "A string value", "world");

        pg.addChoice("Combo", "A combo box", new String[] {"Choice 1", "Choice 2", "Choice 3"}, 1);

        pg.addBoolean("BoolVal1","A boolean value", true);
        pg.addBoolean("BoolVal2","A boolean value", false);

        pg.addButtons("button1", "button one (change DoubleSlider)");
        pg.addButtons("button2", "button two",
                      "button3", "button three",
                      "button4", "button four");

        pg.addCheckBoxes("name1", "Checkbox 1", true,
                         "name2", "Checkbox 2", false);

        pg.addListener(new ParameterListener() {
            public void parameterChanged(ParameterGUI pg, String name)
            {
                System.out.println("Changed "+name);
                if (name.equals("button1")) {
                    pg.sd("DoubleSlider", new Random().nextDouble());
                }
            }
	    });
        f.add(pg.getPanel(), BorderLayout.CENTER);
        f.setVisible(true);
    }
}
