package edu.umich.dice3.ui;

import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import edu.umich.dice3.gamestate.DiceGameState;
import edu.umich.dice3.gamestate.Die;

public class PushPanel extends JPanel {
	
	private static final long serialVersionUID = -7454966615620220739L;
		
	private List<JCheckBox> boxes;
	private List<Die> dice;
	DiceFrame frame;
	
	public PushPanel(DiceFrame frame)
	{
		super(new GridLayout(0, 2));
		this.frame = frame;
		refresh();
	}
	
	public void refresh()
	{
		removeAll();
		
		DiceGameState game = frame.getGame();
		if (game != null)
		{
			int playerId = frame.getPlayerId();
			dice = frame.getGame().getPlayer(playerId).getHiddenDice();
			boxes = new ArrayList<JCheckBox>();
		
			for (Die die : dice)
			{
				add(new JLabel(die + ":"));
				JCheckBox box = new JCheckBox();
				add(box);
				boxes.add(box);
			}
		}
	}
	
	public int[] getValues()
	{
		if (dice == null)
		{
			return new int[0];
		}
		List<Integer> values = new ArrayList<Integer>();
		for (int i = 0; i < dice.size(); ++i)
		{
			if (boxes.get(i).isSelected())
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
}
