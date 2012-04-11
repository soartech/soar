package april.jmat.menv;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.*;

import april.jmat.*;
import april.util.*;

/*
  function fibb(n) { if (n<2) return 1; else return fibb(n-1)+fibb(n-2); }

  a=0;  for (i=0;i<300000;i++) a++;

  for (i=0;i<10;i++) { print(i "    " fibb(i) "\n"); if (i>5) break; }

  for (i=0;i<10;i++) { if (i<5) { print(i "    " fibb(i) "\n"); } else { print (i "\n"); continue; }}

  for (i=0;i<10;i++) {
  for (j=0;j<i;j++) {
  if (j>5)
  break;
  print(i " " j "\n");
  }
  }
*/

public class MEnvGUI
{
    MEnv menv;

    JFrame jf;
    JTextPane editor;
    MyStyledDocument document = new MyStyledDocument();
    Style inputStyle, outputStyle, errorStyle, diagnosticStyle;

    boolean userTurn = false;  // if true, then edits are disallowed before readOnlyBoundary
    int readOnlyBoundary = -1; // disallow edits to the buffer before this offset.

    ArrayList<String> history = new ArrayList<String>();
    boolean inHistory = false;
    int historyIndex = 0;

    JMenuBar menuBar;
    JMenu fileMenu, optionMenu, helpMenu;

    static final int MAX_HISTORY = 50;

    boolean showTimes = false;

    public static void main(String args[])
    {
        new MEnvGUI();
    }

    public MEnvGUI()
    {
        this(new MEnv());
    }

    public MEnvGUI(MEnv menv)
    {
        this.menv = menv;

        Style defaultStyle = document.getStyle(StyleContext.DEFAULT_STYLE);
        inputStyle = document.addStyle("INPUT", defaultStyle);
        outputStyle = document.addStyle("OUTPUT", defaultStyle);
        errorStyle = document.addStyle("ERROR", defaultStyle);
        diagnosticStyle = document.addStyle("ERROR", defaultStyle);

        StyleConstants.setBold(inputStyle, true);
        StyleConstants.setBold(errorStyle, true);
        StyleConstants.setForeground(errorStyle, Color.red);

        StyleConstants.setForeground(diagnosticStyle, Color.blue);
        StyleConstants.setItalic(diagnosticStyle, true);

        jf = new JFrame("MEnv");
        jf.setLayout(new BorderLayout());

        ////////////////////////////////////////////////////////////////
        // Set up the menu bar

        menuBar = new JMenuBar();
        fileMenu = new JMenu("File");
        menuBar.add(fileMenu);
        fileMenu.setMnemonic(KeyEvent.VK_F);
        fileMenu.add(makeMenuItem("New Dialog", KeyEvent.VK_N,
                                  KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK),
                                  new ActionListener() {
                                      public void actionPerformed(ActionEvent e) {
                                          doNewDialog();
                                          //  history.clear();
                                      }}));

        JMenu debugMenu = new JMenu("Debug");
        menuBar.add(debugMenu);

        fileMenu.setMnemonic(KeyEvent.VK_D);
        JCheckBoxMenuItem optimizeItem = new JCheckBoxMenuItem("Optimize expressions", Optimizer.enable);
        debugMenu.add(optimizeItem);
        optimizeItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Optimizer.enable = ((JCheckBoxMenuItem) e.getSource()).getState();
            }});

        JCheckBoxMenuItem verboseOptimizeItem = new JCheckBoxMenuItem("Verbose optimization", Optimizer.verbose);
        debugMenu.add(verboseOptimizeItem);
        verboseOptimizeItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Optimizer.verbose = ((JCheckBoxMenuItem) e.getSource()).getState();
            }});

        JCheckBoxMenuItem showTimesItem = new JCheckBoxMenuItem("Show times", showTimes);
        debugMenu.add(showTimesItem);
        showTimesItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                showTimes = ((JCheckBoxMenuItem) e.getSource()).getState();
            }});

        jf.add(menuBar, BorderLayout.NORTH);

        ////////////////////////////////////////////////////////////////

        editor = new JTextPane(document);
        editor.addCaretListener(new MyCaretListener());

        menv.env.menv.out = new PrintStream(new MyOutputStream());

        // return triggers evaluation.
        editor.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0),
                                                 new EvaluateUserInputAction());

        // shift-return inserts a new-line without triggering evaluation.
        editor.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.SHIFT_MASK),
                                                 new InsertNonTerminatingNewlineAction());

        // control-c aborts this input.
        editor.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK),
                                                 new AbortCommandAction());

        // control-a goes to beginning of input
        editor.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_A, InputEvent.CTRL_MASK),
                                                 new AbstractAction() {
                                                     public void actionPerformed(ActionEvent e) {
                                                         editor.setCaretPosition(readOnlyBoundary);
                                                     }
                                                 });

        editor.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_E, InputEvent.CTRL_MASK),
                                                 new AbstractAction() {
                                                     public void actionPerformed(ActionEvent e) {
                                                         editor.setCaretPosition(document.getLength());
                                                     }
                                                 });

        // up arrow triggers history.
        editor.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0),
                                                 new PreviousHistoryAction());
        editor.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0),
                                                 new NextHistoryAction());

        jf.add(new JScrollPane(editor), BorderLayout.CENTER);

        jf.setSize(600,400);
        jf.setVisible(true);

        usersTurn();
    }

    protected JMenuItem makeMenuItem(String name, int mnemonic, KeyStroke ks, ActionListener listener)
    {
        JMenuItem jmi = new JMenuItem(name, mnemonic);
        jmi.addActionListener(listener);
        jmi.setAccelerator(ks);
        return jmi;
    }

    void usersTurn()
    {
        try {
            document.insertString(document.getLength(), "> ", inputStyle);
            document.beginUserTurn();
            editor.setCaretPosition(document.getLength());

            inHistory = false;

        } catch (BadLocationException ex) {
            System.out.println(ex);
        }
    }

    void processUserInput(String s)
    {
        document.endUserTurn();

        if (s.length() > 0) {
            // don't add to history if this command is the same as the last command.
            if (history.size() == 0 || !history.get(history.size()-1).equals(s))
                history.add(s);
        }

        if (history.size() > MAX_HISTORY)
            history.remove(0);

        try {
            try {
                document.insertString(document.getLength(), "\n", outputStyle);
                Tic tic = new Tic();
                Object o = menv.evaluate(s);
                if (o != null)
                    document.insertString(document.getLength(), ""+o, outputStyle);

                double elapsedTime = tic.toc();
                if (showTimes) {
                    document.insertString(document.getLength(), String.format("Time: %.3fms\n", elapsedTime*1000), diagnosticStyle);
                }
            } catch (MEnvRuntimeException ex) {
                document.insertString(document.getLength(), "Error: "+ex.getMessage()+"\n", errorStyle);
            } catch (RuntimeException ex) {
                document.insertString(document.getLength(), "Error: "+ex.getMessage()+"\n", errorStyle);
            }

            document.insertString(document.getLength(), "\n", outputStyle);
        } catch (BadLocationException ex) {
            System.out.println(ex);
        }

        usersTurn();
    }
    class MyStyledDocument extends DefaultStyledDocument
    {
        public void beginUserTurn()
        {
            // user cannot edit anything older.
            readOnlyBoundary = getLength();
            userTurn = true;
        }

        public void endUserTurn()
        {
            userTurn = false;
        }

        public void remove(int off, int len) throws BadLocationException
        {
            if (userTurn && off < readOnlyBoundary) {
                int reduce = readOnlyBoundary - off;
                off += reduce;
                len -= reduce;
            }

            super.remove(off, len);
        }

        public void insertString(int offset, String s, AttributeSet attrs) throws BadLocationException
        {
            if (userTurn && offset < readOnlyBoundary)
                return;

            super.insertString(offset, s, attrs);
        }
    }

    class MyCaretListener implements CaretListener
    {
        public void caretUpdate(CaretEvent e)
        {
            if (userTurn && editor.getCaretPosition() < readOnlyBoundary)
                editor.setCaretPosition(readOnlyBoundary);
        }
    }

    class MyOutputStream extends OutputStream
    {
        public void write(byte b[], int off, int len)
        {
            String s = new String(b, off, len);

            try {
                document.insertString(document.getLength(), s, outputStyle);
            } catch (BadLocationException ex) {
                System.out.println(ex);
            }
        }

        public void write(int b)
        {
            String s = ""+((char) b);

            try {
                document.insertString(document.getLength(), s, outputStyle);
            } catch (BadLocationException ex) {
                System.out.println(ex);
            }
        }
    }

    class EvaluateUserInputAction extends AbstractAction
    {
        public void actionPerformed(ActionEvent e)
        {
            if (!userTurn)
                return;

            try {
                String cmd = document.getText(readOnlyBoundary, document.getLength() - readOnlyBoundary);
                processUserInput(cmd);
            } catch (BadLocationException ex) {
                System.out.println(ex);
            }
        }
    }

    class InsertNonTerminatingNewlineAction extends AbstractAction
    {
        public void actionPerformed(ActionEvent e)
        {
            if (!userTurn)
                return;

            try {
                document.insertString(document.getLength(), "\n", outputStyle);
            } catch (BadLocationException ex) {
                System.out.println(ex);
            }
        }
    }

    class PreviousHistoryAction extends AbstractAction
    {
        public void actionPerformed(ActionEvent e)
        {
            if (!userTurn)
                return;

            try {
                if (!inHistory) {
                    inHistory = true;
                    String cmd = document.getText(readOnlyBoundary, document.getLength() - readOnlyBoundary);
                    if (cmd.length() > 0) {
                        history.add(document.getText(readOnlyBoundary, document.getLength() - readOnlyBoundary));
                        historyIndex = history.size() - 1;
                    } else {
                        historyIndex = history.size();
                    }
                }

                String cmd = "";
                if (historyIndex >= 0)
                    historyIndex --;
                if (historyIndex >= 0)
                    cmd = history.get(historyIndex);

                document.remove(readOnlyBoundary, document.getLength() - readOnlyBoundary);
                document.insertString(readOnlyBoundary, cmd, inputStyle);
            } catch (BadLocationException ex) {
                System.out.println(ex);
            }
        }
    }

    class NextHistoryAction extends AbstractAction
    {
        public void actionPerformed(ActionEvent e)
        {
            if (!userTurn)
                return;

            if (!inHistory)
                return;

            try {
                historyIndex++;
                if (historyIndex >= history.size())
                    historyIndex = history.size() - 1;

                String cmd = history.get(historyIndex);

                document.remove(readOnlyBoundary, document.getLength() - readOnlyBoundary);
                document.insertString(readOnlyBoundary, cmd, inputStyle);
            } catch (BadLocationException ex) {
                System.out.println(ex);
            }
        }
    }

    class AbortCommandAction extends AbstractAction
    {
        public void actionPerformed(ActionEvent e)
        {
            if (!userTurn)
                return;

            processUserInput("");
        }
    }

    void doNewDialog()
    {
        document.endUserTurn();
        try {
            document.remove(0, document.getLength());
        } catch (BadLocationException ex) {
        }
        menv.clear();
        usersTurn();
    }
}
