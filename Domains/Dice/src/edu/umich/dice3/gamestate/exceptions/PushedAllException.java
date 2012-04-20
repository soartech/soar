package edu.umich.dice3.gamestate.exceptions;

import edu.umich.dice3.gamestate.Player;

public class PushedAllException extends DiceActionException
{

    private static final long serialVersionUID = 2371088516410293845L;

    public PushedAllException(Player player)
    {
        super(player.getName() + " pushed all their dice.");
    }

}
