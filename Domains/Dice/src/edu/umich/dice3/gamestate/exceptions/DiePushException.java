package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class DiePushException extends DiceActionException
{
	
	private static final long serialVersionUID = -535047410836734761L;

	public DiePushException(Player player, int dieIndex) {
		super(player + " tried to push die at index " + dieIndex + ", which had been previously pushed.");
	}
}
