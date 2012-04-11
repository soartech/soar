package april.procman;

import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.table.*;
import javax.swing.text.*;

import lcm.lcm.*;
import april.lcmtypes.*;

import april.util.*;

/**
 * Issues:
 * 1. Auto-scrolling is in a funny state, and must be done manually
 * because it is hard to find out if the pane isn't at the end because
 * a user wants to scroll to the middle, or because new content was
 * added. Particularly, adding output to a Document doesnt seem to be
 * reflected immediately in the JScrollBar, so before adding input, if
 * the view is not at the end, it may be because of content which was
 * added last time, yet we didn't find out about it soon enough to
 * update the run that time, vs. if the change was due to the user.
 * 2. Document styling changes appear to occur when a buffer becomes
 * the selected one.  -JHS
 * 3. There is a display bug in the buttons when the processes are
 * sorted by name. Investigate the convertRow style functions..
 *
 */
class Spy implements LCMSubscriber
{
    public static final int WIN_WIDTH = 1024;
    public static final int WIN_HEIGHT = 600;
    public static final int HOST_WIDTH = 250;

    JFrame    jf;
    JTable    proctable, hosttable;
    JTextPane textSelected, textError;
    JScrollPane textSelectedScroll, textErrorScroll;

    JButton   startStopButton, stopAllButton, startAllButton, clearButton;
    JCheckBox autoScrollBox;

    ProcGUIDocument outputSummary = new ProcGUIDocument();

    ProcMan proc;

    ArrayList<ProcRecordG> processes;
    HashMap<Integer, ProcRecordG> processesMap;
    ArrayList<HostRecord> hosts = new ArrayList<HostRecord>();

    static LCM lcm = LCM.getSingleton();

    ProcessTableModel processTableModel = new ProcessTableModel();
    HostTableModel hostTableModel = new HostTableModel();

    boolean scrollToEnd;

    Spy(ProcMan _proc)
    {
        proc = _proc;
        init();
    }

    Spy()
    {
        proc = null;
        init();
    }

    public void init()
    {

        processes = new ArrayList<ProcRecordG>();
        processesMap = new HashMap<Integer, ProcRecordG>();

        proctable = new JTable(processTableModel);
        proctable.setRowSorter(new TableRowSorter(processTableModel));

        // only allow one row to be selected at a time
        proctable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        // allow section of processes via mouse section
        proctable.addMouseListener(new MouseAdapter() {
                public void mouseClicked(MouseEvent e) {
                    updateTableSelection();
                }
            });

        // allow section of processes via keyboard (up and down) keys
        ListSelectionModel rowSM = proctable.getSelectionModel();
        rowSM.addListSelectionListener(new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    if (e.getValueIsAdjusting())
                        return;
                    updateTableSelection();
                }
            });

        hosttable = new JTable(hostTableModel);
        hosttable.setRowSorter(new TableRowSorter(hostTableModel));

        textSelected = new JTextPane();
        textSelected.setEditable(false);
        textSelected.setDocument(outputSummary);

        textError = new JTextPane();
        textError.setEditable(false);
        textError.setDocument(outputSummary);

        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayout(1,5));
        startStopButton = new JButton("Start/Stop");
        startStopButton.setEnabled(false);
        startAllButton = new JButton("Start All");
        stopAllButton = new JButton("Stop All");
        clearButton = new JButton("Clear");
        autoScrollBox = new JCheckBox("Auto Scroll", true);
        scrollToEnd = true;

        buttonPanel.add(startStopButton);
        buttonPanel.add(startAllButton);
        buttonPanel.add(stopAllButton);
        buttonPanel.add(clearButton);
        buttonPanel.add(autoScrollBox);

        if (proc == null) {
            startStopButton.setEnabled(false);
            startAllButton.setEnabled(false);
            stopAllButton.setEnabled(false);
        } else {
            startStopButton.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        int row = proctable.getSelectedRow();
                        if (row >= 0) {
                            row = proctable.convertRowIndexToModel(row);
                            proc.toggleRunStatus(processes.get(row).procid);
                        }
                        updateStartStopText(row);
                    }
                });

            startAllButton.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        for (ProcRecordG pr : processes) {
                            proc.setRunStatus(pr.procid, true);
                        }
                        int rc = proctable.getRowCount();
                        for(int row = 0;  row < rc; row++)
                            updateStartStopText(row);
                    }
                });

            stopAllButton.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        for (ProcRecordG pr : processes) {
                            proc.setRunStatus(pr.procid, false);
                        }
                        int rc = proctable.getRowCount();
                        for(int row = 0;  row < rc; row++)
                            updateStartStopText(row);
                    }
                });
        }

        clearButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    synchronized(Spy.this)
                    {
                        clear();
                    }
                }
            });

        autoScrollBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    scrollToEnd = !scrollToEnd;
                }
            });

        JSplitPane jsp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                        new JScrollPane(proctable),
                                        new JScrollPane(hosttable));
        jsp.setDividerLocation(WIN_WIDTH - HOST_WIDTH);
        jsp.setResizeWeight(1);


        JPanel jp = new JPanel();
        jp.setLayout(new BorderLayout());
        jp.add(jsp, BorderLayout.CENTER);
        jp.add(buttonPanel, BorderLayout.SOUTH);

        textSelectedScroll = new JScrollPane(textSelected);
        textSelectedScroll.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
                public void adjustmentValueChanged(AdjustmentEvent e){
                    JScrollBar jsb = textSelectedScroll.getVerticalScrollBar();
                    if (scrollToEnd)
                        jsb.setValue(jsb.getMaximum());
                }
            });

        textErrorScroll = new JScrollPane(textError);
        textErrorScroll.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
                public void adjustmentValueChanged(AdjustmentEvent e){
                    JScrollBar jsb = textErrorScroll.getVerticalScrollBar();
                    if (scrollToEnd)
                        jsb.setValue(jsb.getMaximum());
                }
            });
        JSplitPane textJsp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                            textErrorScroll, textSelectedScroll);
        JSplitPane leftJsp = new JSplitPane(JSplitPane.VERTICAL_SPLIT, jp, textJsp);

        textJsp.setDividerLocation(0.4);
        textJsp.setResizeWeight(0.5);

        Dimension minimumSize = new Dimension(300, 100);
        textErrorScroll.setMinimumSize(minimumSize);
        textSelectedScroll.setMinimumSize(minimumSize);

        leftJsp.setDividerLocation(0.3);
        leftJsp.setResizeWeight(0.3);

        jf = new JFrame("ProcMan Spy " +
                        (proc == null ? "(Read Only)" : "(Privileged)"));
        jf.setLayout(new BorderLayout());
        jf.add(leftJsp, BorderLayout.CENTER);

        TableColumnModel tcm = proctable.getColumnModel();
        tcm.getColumn(0).setPreferredWidth(50);
        tcm.getColumn(1).setPreferredWidth(500);
        tcm.getColumn(2).setPreferredWidth(100);
        tcm.getColumn(3).setPreferredWidth(100);
        tcm.getColumn(4).setPreferredWidth(100);

        jf.setSize(WIN_WIDTH, WIN_HEIGHT);
        jf.setVisible(true);
        jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        lcm.subscribe("PROCMAN_STATUS_LIST", this);
        lcm.subscribe("PROCMAN_OUTPUT", this);
        lcm.subscribe("PROCMAN_PROCESS_LIST", this);
    }

    public synchronized void updateTableSelection()
    {
        int row = proctable.getSelectedRow();
        if (row >= 0) {
            row = proctable.convertRowIndexToModel(row);
            textSelected.setDocument(processes.get(row).output);
            textSelected.setCaretPosition(processes.get(row).output.getLength());

        } else {
            textSelected.setDocument(outputSummary);
            textSelected.setCaretPosition(outputSummary.getLength());
        }
        updateStartStopText(row);
    }

    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try {
            messageReceivedEx(lcm, channel, ins);
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }

    public void messageReceivedEx(LCM lcm, String channel, LCMDataInputStream ins) throws IOException
    {
        if (channel.equals("PROCMAN_OUTPUT")) {
            procman_output_t po = new procman_output_t(ins);

            ProcRecordG pr = ensureProcRecord(po.procid); // processesMap.get(po.procid);

            if (pr == null)
                return;

            if (po.stream == 0) {
                pr.output.appendDefault(po.data + "\n");
            } else {
                pr.output.appendError(po.data + "\n");
                outputSummary.appendError(po.data + "\n");
            }
        }else if (channel.equals("PROCMAN_STATUS_LIST")) {
            procman_status_list_t psl = new procman_status_list_t(ins);

            /////////// Update Process Statistics
            for (int i = 0; i < psl.nprocs; i++) {

                procman_status_t ps = psl.statuses[i];

                ProcRecordG pr = ensureProcRecord(ps.procid); // processesMap.get(ps.procid);
                if (pr == null) {
                    System.out.println("unknown procid "+ps.procid);
                    continue;
                }

                pr.lastStatus = ps;
                pr.restartCount = ps.restarts;
                pr.lastStatusUtime = psl.utime;

                processTableModel.fireTableRowsUpdated(pr.pridx, pr.pridx);
            }

            ////////// Update Host Statistics
            HostRecord hr = ensureHost(psl.host);
            hr.skew = psl.utime - TimeUtil.utime();
            hr.rtt = TimeUtil.utime() - psl.received_utime;

            hostTableModel.fireTableCellUpdated(hr.hridx,1);
            hostTableModel.fireTableCellUpdated(hr.hridx,2);
        } else if(channel.equals("PROCMAN_PROCESS_LIST")){
            procman_process_list_t proc_list = new procman_process_list_t(ins);
            for(int i = 0; i < proc_list.nprocs; i++){
                procman_process_t p = proc_list.processes[i];
                ProcRecordG pr = ensureProcRecord(p.procid);
                pr.cmdline = p.cmdline;
                pr.host = p.host;
                pr.name = p.name;
                processTableModel.fireTableRowsUpdated(pr.pridx, pr.pridx);
            }
        }
    }


    class HostRecord
    {
        String host;

        // how long did it take for it to reply to our last send
        // command? (usecs)
        long   rtt;

        // what is the difference between the utime on dameon and
        // master?  (includes latency) (usecs)
        long   skew;

        int hridx;
    }


    class ProcessTableModel extends AbstractTableModel
    {
        String columnNames[] = { "ProcID", "Command", "Name", "Host", "Status" };

        public int getColumnCount()
        {
            return columnNames.length;
        }

        public String getColumnName(int col)
        {
            return columnNames[col];
        }

        public int getRowCount()
        {
            return processes.size();
        }

        public Object getValueAt(int row, int col)
        {
            ProcRecordG pr = processes.get(row);

            switch (col) {
                case 0:
                    return pr.procid;
                case 1:
                    return pr.cmdline;
                case 2:
                    return pr.name;
                case 3:
                    return pr.host;
                case 4:
                    if (pr.lastStatus == null)
                        return "Unknown";
                    else {
                        int exitCode = (pr.lastStatus != null) ? pr.lastStatus.last_exit_code : 0;
                        return pr.lastStatus.running ? "Running" : "Stopped ("+exitCode+")";
                    }
            }

            return "??";
        }
    }

    class HostTableModel extends AbstractTableModel
    {
        String columnNames[] = { "Host", "RTT", "Skew" };

        public int getColumnCount()
        {
            return columnNames.length;
        }

        public String getColumnName(int col)
        {
            return columnNames[col];
        }

        public int getRowCount()
        {
            return hosts.size();
        }

        public Object getValueAt(int row, int col)
        {
            HostRecord hr = hosts.get(row);

            switch (col) {
                case 0:
                    return hr.host;
                case 1:
                    return String.format("%.1f ms", hr.rtt/1000.0);
                case 2:
                    return String.format("%.1f ms", hr.skew/1000.0);
            }
            return "??";
        }
    }

    synchronized void clear()
    {
        // XXX need to fire some events here...
        int removedProc  = processes.size();
        processes.clear();
        processesMap.clear();

        int removedHost  = hosts.size();
        hosts.clear();

        processTableModel.fireTableRowsDeleted(0,removedProc - 1);
        hostTableModel.fireTableRowsDeleted(0,removedHost - 1);
    }

    synchronized ProcRecordG ensureProcRecord(int procid)
    {
        ProcRecordG pr = processesMap.get(procid);
        if(pr != null)
            return pr;

        // otherwise, we've got some data to create and fill in

        pr = new ProcRecordG();
        pr.procid = procid;
        pr.cmdline = "???";
        pr.host = "???";
        pr.name = "???";
        pr.pridx = processes.size();

        processes.add(pr);
        processesMap.put(procid, pr);
        processTableModel.fireTableRowsInserted(processes.size() - 1,
                                                processes.size() - 1);

        return pr;
    }

    synchronized HostRecord ensureHost(String hostStr)
    {
        for (HostRecord host : hosts)
            if (host.host.equals(hostStr))
                return host;

        HostRecord hr = new HostRecord();
        hr.host = hostStr;
        hr.hridx = hosts.size();
        hosts.add(hr);
        hostTableModel.fireTableRowsInserted(hr.hridx, hr.hridx);
        return hr;
    }

    /**
     * This method should ideally be called whenever the GUI state of
     * running is changed for a process in row 'row'
     */
    void updateStartStopText(int row)
    {
        if (proc == null)
            return;

        if (row < 0) {
            startStopButton.setText("*");
            startStopButton.setEnabled(false);
            return;
        }

        int row2 = proctable.convertRowIndexToModel(row);
        int procid  = processes.get(row2).procid;
        if (proc.getRunStatus(procid))
            startStopButton.setText("Stop");
        else
            startStopButton.setText("Start");
        startStopButton.setEnabled(true);
    }

    class ProcRecordG extends ProcRecord
    {
        ProcGUIDocument output;
        int pridx;

        procman_status_t lastStatus;
        long lastStatusUtime;

        ProcRecordG()
        {
            output = new ProcGUIDocument();
        }
    }

    class ProcGUIDocument extends DefaultStyledDocument
    {
        Style defaultStyle, errorStyle, summaryStyle;

        static final int MAX_LENGTH = 128*1024;

        ProcGUIDocument()
        {
            defaultStyle = getStyle(StyleContext.DEFAULT_STYLE);
            StyleConstants.setFontFamily(defaultStyle, "Monospaced");
            StyleConstants.setFontSize(defaultStyle, 10);

            errorStyle   = addStyle("ERROR", defaultStyle);
            StyleConstants.setFontFamily(errorStyle, "Monospaced");
            StyleConstants.setFontSize(errorStyle, 10);
            StyleConstants.setForeground(errorStyle, Color.red);

            summaryStyle = addStyle("SUMMARY", defaultStyle);
            StyleConstants.setFontFamily(summaryStyle, "Monospaced");
            StyleConstants.setFontSize(summaryStyle, 10);
            StyleConstants.setForeground(summaryStyle, Color.blue);
        }

        void insertStringEx(int pos, String s, Style style)
        {
            // avoid synchrony with UpdateTableSelection, which causes an exception.
            synchronized(Spy.this) {

                try {
                    if (getLength() > MAX_LENGTH) {
                        remove(0, MAX_LENGTH / 10);
                    }

                    insertString(getLength(), s, style);
                } catch (Exception ex) {
                    System.out.print("caught: ");
                    ex.printStackTrace();
                }
            }
        }

        void appendDefault(String s)
        {
            insertStringEx(getLength(), s, defaultStyle);
        }

        void appendError(String s)
        {
            insertStringEx(getLength(), s, errorStyle);
        }

        void appendSummary(String s)
        {
            insertStringEx(getLength(), s, summaryStyle);
        }

    }

    public static void main(String args[])
    {
        Spy pg = new Spy();
    }

}
