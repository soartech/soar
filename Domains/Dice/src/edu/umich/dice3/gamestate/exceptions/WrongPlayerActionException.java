package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class WrongPlayerActionException extends DiceActionException
{
	private static final long serialVersionUID = 7438256558913126799L;

	public WrongPlayerActionException(Player actingPlayer, Player currentPlayer) {
		super("Expected " + currentPlayer + " to take an action, but " + actingPlayer + " took one instead.");
	}
}
