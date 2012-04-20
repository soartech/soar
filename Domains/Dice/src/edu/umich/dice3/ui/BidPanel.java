package edu.umich.dice3.ui;

import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import edu.umich.dice3.gamestate.DiceGameState;
import edu.umich.dice3.gamestate.Die;

public class BidPanel extends JPanel {
	
	private static final long serialVersionUID = -7454966615620220739L;
	
	Integer[] countInts = new Integer[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	Integer[] valueInts = new Integer[] { 1, 2, 3, 4, 5, 6 };
	
	private ButtonGroup countGroup;
	private ButtonGroup valueGroup;
	
	private List<JRadioButton> countButtons;
	private List<JRadioButton> valueButtons;
	
	private DiceFrame frame;
	
	private List<JCheckBox> pushBoxes;
	private List<Die> dice;
	
	public BidPanel(DiceFrame frame, int playerId)
	{
		super();
		GridLayout layout = new GridLayout(0, 2);
		layout.setHgap(20);
		setLayout(layout);
		this.frame = frame;
		refresh();
	}
	
	public int getCount()
	{
		for (int i = 0; i < countButtons.size(); ++i)
		{
			if (countButtons.get(i).isSelected())
			{
				return i + 1;
			}
		}
		return -1;
	}
	
	public int getValue()
	{
		for (int i = 0; i < valueButtons.size(); ++i)
		{
			if (valueButtons.get(i).isSelected())
			{
				return i + 1;
			}
		}
		return -1;
	}
	
	public int[] getPush()
	{
		if (dice == null)
		{
			return new int[0];
		}
		List<Integer> values = new ArrayList<Integer>();
		for (int i = 0; i < dice.size(); ++i)
		{
			if (pushBoxes.get(i).isSelected())
			{
				values.add(dice.get(i).getValue());
			}
		}
		int[] ret = new int[values.size()];
		for (int i = 0; i < values.size(); ++i)
		{
			ret[i] = values.get(i);
		}
		return ret;
	}
	
	public void refresh()
	{
		removeAll();
		
		countGroup = new ButtonGroup();
		valueGroup = new ButtonGroup();
		
		countButtons = new ArrayList<JRadioButton>();
		valueButtons = new ArrayList<JRadioButton>();
		
		add(new JLabel("Bid"));
		add(new JPanel());

		add(new JLabel("Count:"));
		add(new JLabel("Value:"));
		
		for (int i = 0; i < Math.max(countInts.length, valueInts.length); ++i)
		{
			if (i < countInts.length)
			{
				JRadioButton button = new JRadioButton("" + (i + 1));
				add(button);
				countGroup.add(button);
				countButtons.add(button);
			}
			else
			{
				add(new JPanel());
			}
			if (i < valueInts.length)
			{
				JRadioButton button = new JRadioButton("" + (i + 1));
				add(button);
				valueGroup.add(button);
				valueButtons.add(button);
			}
			else
			{
				add(new JPanel());
			}
		}
		
		countButtons.get(0).setSelected(true);
		valueButtons.get(0).setSelected(true);
				
		DiceGameState game = frame.getGame();
		if (game != null)
		{
			int playerId = frame.getPlayerId();
			dice = frame.getGame().getPlayer(playerId).getHiddenDice();
			pushBoxes = new ArrayList<JCheckBox>();
		
			add(new JLabel("Push:"));
			add(new JPanel());
			
			for (Die die : dice)
			{
				add(new JLabel(die + ":"));
				JCheckBox box = new JCheckBox();
				add(box);
				pushBoxes.add(box);
			}
		}
		
		validate();
	}
}
