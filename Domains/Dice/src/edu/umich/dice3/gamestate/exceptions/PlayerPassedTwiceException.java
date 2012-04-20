package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class PlayerPassedTwiceException extends DiceActionException
{
	private static final long serialVersionUID = 2079927679328802206L;

	public PlayerPassedTwiceException(Player player) {
		super(player + " tried to pass after previously passing during this game.");
	}
}
