package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Bid;

public class InvalidBidException extends DiceActionException
{
    private static final long serialVersionUID = 5360150292806342638L;

    public InvalidBidException(Bid bid) {
		super("Invalid bid " + bid);
	}
}
