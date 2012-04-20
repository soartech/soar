package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Bid;

public class InvalidRaiseException extends DiceActionException
{

	private static final long serialVersionUID = 5466122496980012902L;

	public InvalidRaiseException(Bid bid, Bid previousBid) {
		super("Invalid bid " + bid + " after previous bid " + previousBid);
	}
}
