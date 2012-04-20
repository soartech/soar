package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class InvalidExactException extends DiceActionException
{

	private static final long serialVersionUID = -2587840981716327163L;

	public InvalidExactException(Player player) {
		super(player + " exacted when they weren't allowed to.");
	}
}
