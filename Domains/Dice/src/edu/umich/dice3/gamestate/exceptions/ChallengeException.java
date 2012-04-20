package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class ChallengeException extends DiceActionException
{
	
	private static final long serialVersionUID = -535047410836734761L;

	public ChallengeException(Player player, Player target) {
		super(player + " tried to challenge " + target + ", who was not challengeable.");
	}
}
