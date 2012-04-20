package edu.umich.dice3.gamestate;

import java.util.ArrayList;
import java.util.List;
import edu.umich.dice3.gamestate.HistoryItem.Type;
import edu.umich.dice3.gamestate.exceptions.DiceActionException;
import edu.umich.dice3.gamestate.exceptions.DiePushException;
import edu.umich.dice3.gamestate.exceptions.PushedAllException;

public class Player
{

    private int id;
    private int numDice;
    private boolean lost;
    private List<Die> dice;
    boolean hasPassed;
    boolean hasExacted;
    private String name;
    private DiceGameState game;
    private int maxDice;
    private boolean inSpecialRules;
    private boolean hasDoneSpecialRules;

    public Player(String name, int id, int numDice, DiceGameState game)
    {
        this.name = name;
        this.id = id;
        this.numDice = numDice;
        maxDice = numDice;
        lost = false;
        dice = new ArrayList<Die>();
        hasPassed = false;
        hasExacted = false;
        this.game = game;
        inSpecialRules = false;
        hasDoneSpecialRules = false;
    }

    public int getId()
    {
        return id;
    }

    public void setLost(boolean lost)
    {
        this.lost = lost;
    }

    public boolean getLost()
    {
        return lost;
    }

    public void newRound()
    {
        if (numDice == 1 && !hasDoneSpecialRules)
        {
            inSpecialRules = true;
            hasDoneSpecialRules = true;
        }
        else if (inSpecialRules = true)
        {
            inSpecialRules = false;
        }
        dice.clear();
        hasPassed = false;
        for (int i = 0; i < numDice; ++i)
        {
            dice.add(new Die());
        }
    }

    public void loseDie()
    {
        --numDice;
    }

    public boolean gainDie()
    {
        if (numDice < maxDice)
        {
            ++numDice;
            return true;
        }
        return false;
    }

    public int getNumDice()
    {
        return numDice;
    }

    public void setHasPassed(boolean hasPassed)
    {
        this.hasPassed = hasPassed;
    }

    public boolean getHasPassed()
    {
        return hasPassed;
    }

    public void setHasExacted(boolean hasExacted)
    {
        this.hasExacted = hasExacted;
    }

    public boolean getHasExacted()
    {
        return hasExacted;
    }

    public List<Die> getDice()
    {
        if (lost) return new ArrayList<Die>();
        return dice;
    }

    @Override
    public String toString()
    {
        return name;
    }

    public void push(int[] push) throws DiceActionException
    {
        hasPassed = false;
        for (int i : push)
        {
            Die die = findUnpushedDie(i);
            if (die == null)
            {
                throw new DiePushException(this, i);
            }
            die.push();
        }
        boolean diceLeft = false;
        for (int i = 0; i < dice.size(); ++i)
        {
            Die die = dice.get(i);
            if (!die.getPushed())
            {
                diceLeft = true;
                die.roll();
            }
        }
        if (!diceLeft)
        {
            throw new PushedAllException(this);
        }
    }

    private Die findUnpushedDie(int value)
    {
        for (Die die : getHiddenDice())
        {
            if (die.getValue() == value)
            {
                return die;
            }
        }
        return null;
    }

    public List<Die> getPushedDice()
    {
        List<Die> pushed = new ArrayList<Die>();
        for (Die die : dice)
        {
            if (die.getPushed())
            {
                pushed.add(die);
            }
        }
        return pushed;
    }

    public List<Die> getHiddenDice()
    {
        List<Die> pushed = new ArrayList<Die>();
        for (Die die : dice)
        {
            if (!die.getPushed())
            {
                pushed.add(die);
            }
        }
        return pushed;
    }

    public String getName()
    {
        return name;
    }

    public boolean canBid()
    {
        if (lost) return false;
        return game.getCurrentPlayer() == this;
    }

    public boolean canChallengeBid()
    {
        if (lost) return false;
        if (canChallengeSecondLastPass()) return false;
        Bid previousBid = game.getPreviousBid();
        return game.getCurrentPlayer() == this && previousBid != null && previousBid.getPlayerId() != id;
    }

    public boolean canChallengeLastPass()
    {
        if (lost) return false;
        HistoryItem item = game.getLastHistoryItem();
        return item != null && game.getCurrentPlayer() == this && item.getType() == Type.PASS;
    }

    public boolean canChallengeSecondLastPass()
    {
        if (lost) return false;
        if (!canChallengeLastPass())
        {
            return false;
        }
        List<HistoryItem> history = game.getHistory();
        if (history == null || history.size() < 2)
        {
            return false;
        }

        HistoryItem item = history.get(history.size() - 2);
        return item != null && game.getCurrentPlayer() == this && item.getType() == Type.PASS;
    }

    public boolean canExact()
    {
        if (lost) return false;
        Bid previousBid = game.getPreviousBid();
        return !hasExacted && game.getCurrentPlayer() == this && previousBid != null && previousBid.getPlayerId() != id;
    }

    public boolean canPass()
    {
        if (lost) return false;
        return game.getCurrentPlayer() == this && !hasPassed && dice.size() > 1;
    }

    public boolean canPush()
    {
        if (lost) return false;
        HistoryItem item = game.getLastHistoryItem();
        return item != null && item.getPlayer() == this && item.getType() == Type.BID && getHiddenDice().size() > 1;
    }

    public boolean canAccept()
    {
        if (lost) return false;
        return false;
    }
    
    public boolean isInSpecialRules()
    {
        return inSpecialRules;
    }

    public boolean allDiceSame()
    {
        int value = -1;
        for (Die die : getDice())
        {
            if (value == -1)
            {
                value = die.getValue();
            }
            else
            {
                if (die.getValue() != value)
                {
                    return false;
                }
            }
        }
        return true;
    }

    public String getStateString(boolean showHidden)
    {
        StringBuilder sb = new StringBuilder();
        sb.append(id + " (" + name + "): ");
        if (showHidden)
        {
            for (int i = 0; i < dice.size(); ++i)
            {
                Die die = dice.get(i);
                sb.append("" + die);
                if (i + 1 < dice.size())
                {
                    sb.append(' ');
                }
            }
        }
        else
        {
            for (int i = 0; i < dice.size(); ++i)
            {
                Die die = dice.get(i);
                sb.append("" + (die.getPushed() ? die : "?"));
                if (i + 1 < dice.size())
                {
                    sb.append(' ');
                }
            }
        }
        return sb.toString();
    }
}
