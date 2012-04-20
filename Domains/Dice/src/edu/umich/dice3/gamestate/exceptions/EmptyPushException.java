package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class EmptyPushException extends DiceActionException
{

	private static final long serialVersionUID = -2587840981716327163L;

	public EmptyPushException(Player player) {
		super(player + " took action push without pushing any dice.");
	}
}
