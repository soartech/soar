package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class IllegalMoveException extends DiceActionException
{

    private static final long serialVersionUID = -7761124654665233420L;

    public IllegalMoveException(Player player)
    {
        super(player.getName() + " made an illegal move and lost the round.");
    }
}
