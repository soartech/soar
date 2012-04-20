package edu.umich.dice3.ui;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;

import edu.umich.dice3.RulesData;
import edu.umich.dice3.gamestate.Bid;
import edu.umich.dice3.gamestate.DiceGameState;
import edu.umich.dice3.gamestate.HistoryItem;
import edu.umich.dice3.gamestate.Player;
import edu.umich.dice3.ui.UIAction.Type;

public class DiceFrame implements ActionListener
{

    private static final long serialVersionUID = -9201916931970063107L;

    private List<UIAction> actions;
    private Map<UIAction.Type, JButton> buttonMap;
    private UIAction action = null;

    private DiceGameState game;
    private JTextArea textPane;
    private BidPanel bidPanel;
    private HistoryPanel history;
    private JCheckBox debugBox;
    private JButton saveButton;

    private boolean doStuff;

    private int[] wins;
    private RulesData[] rulesData;

    int playerId;

    // Instead of extending JFrame
    private JFrame frame;

    public DiceFrame(int playerId, int numAgents, boolean visible)
    {
        if (visible)
        {
            frame = new JFrame("Dice Game");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        }
        else
        {
            frame = null;
        }

        doStuff = true;
        this.playerId = playerId;
        wins = new int[numAgents];
        rulesData = new RulesData[numAgents];
        
        actions = new ArrayList<UIAction>();
        for (Type type : UIAction.Type.values())
        {
            actions.add(new UIAction(type));
        }

        Container pane = null;
        if (frame != null)
        {
            pane = frame.getContentPane();
            pane.setLayout(new BorderLayout());
            frame.setSize(1100, 700);
        }

        JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

        history = new HistoryPanel(new ArrayList<HistoryItem>());
        split.add(history);

        textPane = new JTextArea();
        textPane.setEditable(false);

        bidPanel = new BidPanel(this, playerId);
        if (frame != null) if (pane != null)
        {
            pane.add(bidPanel, BorderLayout.EAST);
        }

        JScrollPane scroll = new JScrollPane(textPane);

        split.add(scroll);
        split.setDividerLocation(400);
        if (pane != null)
        {
            pane.add(split, BorderLayout.CENTER);
        }

        JPanel panel = new JPanel(new FlowLayout());
        buttonMap = new HashMap<UIAction.Type, JButton>();
        for (final UIAction action : actions)
        {
            // Don't include a button for sleeping
            if (action.getType() == Type.SLEEP) continue;

            JButton button = new JButton(action.getName());
            button.addActionListener(new ActionListener()
            {

                @Override
                public void actionPerformed(ActionEvent e)
                {
                    actionButtonPressed(action);
                }
            });
            panel.add(button);
            buttonMap.put(action.getType(), button);
        }
        debugBox = new JCheckBox("Run Soar Manually");
        debugBox.setSelected(false);
        panel.add(debugBox);
        saveButton = new JButton("Save History");
        panel.add(saveButton);
        saveButton.addActionListener(this);

        if (pane != null)
        {
            pane.add(panel, BorderLayout.SOUTH);
            panel.doLayout();
            pane.doLayout();
        }
        
        if (frame != null)
        {
            frame.setVisible(visible);
        }
    }

    public void setDoStuff(boolean doStuff)
    {
        this.doStuff = doStuff;
    }

    public boolean getRunSoarManually()
    {
        synchronized (debugBox)
        {
            return debugBox.isSelected();
        }
    }
    
    private void actionButtonPressed(UIAction action)
    {
        synchronized (this)
        {
            System.out.println("Action: " + action.getName());
            switch (action.getType())
            {
            case BID:
                action.setBid(promptBid());
                if (action.getBid() == null) return;
                break;
            case CHALLENGE_BID:
                break;
            case CHALLENGE_PASS:
                break;
            case EXACT:
                break;
            }
            this.action = action;
        }
    }

    private Bid promptBid()
    {
        Bid bid = new Bid(playerId, bidPanel.getCount(), bidPanel.getValue());
        bid.setPush(bidPanel.getPush());
        return bid;
    }

    public void setGame(DiceGameState game)
    {
        this.game = game;
    }

    public void refreshText()
    {
        if (doStuff)
        {
            String displayString = getDisplayString();
            textPane.setText(displayString);
        }
    }

    private String getDisplayString()
    {
        return "Wins so far:\n" + getWinsString() + "\n\n" + "Current game state:\n" + game.getStateString(playerId) + "\n\n" + "Current Bid:\n"
                + game.getPreviousBid() + "\n\n" + "Next player:\n" + game.getCurrentPlayer();
    }

    public String getWinsString()
    {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < wins.length; ++i)
        {
            sb.append("Player #" + i + ": " + wins[i]);
            if (i + 1 < wins.length)
            {
                sb.append('\n');
            }
        }
        return sb.toString();
    }

    public void refreshHistory()
    {
        history.setHistory(game.getFlatHistory(), doStuff);
    }

    public UIAction getAction(int playerId)
    {
        if (game.getCurrentPlayerId() != playerId)
        {
            return new UIAction(Type.SLEEP);
        }
        action = null;
        this.playerId = playerId;
        textPane.setText(getDisplayString() + "\n\nWAITING FOR USER INPUT...");
        bidPanel.refresh();

        Player player = game.getPlayer(playerId);
        for (UIAction.Type action : UIAction.Type.values())
        {
            JButton button = buttonMap.get(action);
            switch (action)
            {
            case BID:
                button.setEnabled(player.canBid());
                break;
            case CHALLENGE_BID:
                button.setEnabled(player.canChallengeBid());
                break;
            case CHALLENGE_PASS:
                button.setEnabled(player.canChallengeLastPass());
                break;
            case EXACT:
                button.setEnabled(player.canExact());
                break;
            case PASS:
                button.setEnabled(player.canPass());
                break;
            }
        }
        while (true)
        {
            try
            {
                Thread.sleep(1000);
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
            synchronized (this)
            {
                if (action != null)
                {
                    break;
                }
            }
        }

        refreshText();

        // Now that we have the action, disable buttons
        for (UIAction.Type action : UIAction.Type.values())
        {
            JButton button = buttonMap.get(action);
            switch (action)
            {
            case BID:
                button.setEnabled(false);
                break;
            case CHALLENGE_BID:
                button.setEnabled(false);
                break;
            case CHALLENGE_PASS:
                button.setEnabled(false);
                break;
            case EXACT:
                button.setEnabled(false);
                break;
            case PASS:
                button.setEnabled(false);
                break;
            }
        }

        synchronized (this)
        {
            return action;
        }
    }

    public DiceGameState getGame()
    {
        return game;
    }

    public int getPlayerId()
    {
        return playerId;
    }

    @Override
    public void actionPerformed(ActionEvent event)
    {
        assert (frame != null);
        Object source = event.getSource();
        if (source == saveButton)
        {
            JFileChooser chooser = new JFileChooser(System.getProperty("user.dir"));
            int result = chooser.showSaveDialog(frame);
            if (result != JFileChooser.APPROVE_OPTION)
            {
                return;
            }
            File file = chooser.getSelectedFile();
            if (file.exists())
            {
                result = JOptionPane.showConfirmDialog(frame, "The file " + file.getName() + " exists, overwrite?", "Overwrite Existing File?",
                        JOptionPane.YES_NO_OPTION);
                if (result != JOptionPane.YES_OPTION)
                {
                    return;
                }
            }
            FileOutputStream os = null;
            try
            {
                os = new FileOutputStream(file);
                writeHistoryToStream(history.getHistory(), os);
            }
            catch (FileNotFoundException e)
            {
                e.printStackTrace();
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
            finally
            {
                try
                {
                    os.close();
                }
                catch (IOException e)
                {
                    e.printStackTrace();
                }
            }
        }
    }

    public static void writeHistoryToStream(List<HistoryItem> history, FileOutputStream stream) throws IOException
    {
        for (HistoryItem item : history)
        {
            stream.write(item.toString().getBytes());
            stream.write('\n');
            stream.write(item.getState().getBytes());
            stream.write("\n\n".toString().getBytes());
        }
    }

    public List<HistoryItem> getHistory()
    {
        return history.getHistory();
    }

    public int[] getWins()
    {
        return wins;
    }

    public void setVisible(boolean b)
    {
        if (frame != null)
        {
            frame.setVisible(true);
        }
    }

    public void setRunSoarManually(boolean runManually)
    {
        synchronized (debugBox)
        {
            debugBox.setSelected(runManually);            
        }
    }

    public void setRulesData(RulesData rulesData, int i)
    {
        this.rulesData[i] = rulesData;
    }
    
    public RulesData[] getRulesData()
    {
        return rulesData;
    }
}
