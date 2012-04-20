package edu.umich.dice3.gamestate;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import edu.umich.dice3.SoarMatch;
import edu.umich.dice3.gamestate.HistoryItem.Type;
import edu.umich.dice3.gamestate.exceptions.ChallengeException;
import edu.umich.dice3.gamestate.exceptions.DiceActionException;
import edu.umich.dice3.gamestate.exceptions.EmptyPushException;
import edu.umich.dice3.gamestate.exceptions.InvalidBidException;
import edu.umich.dice3.gamestate.exceptions.InvalidExactException;
import edu.umich.dice3.gamestate.exceptions.InvalidRaiseException;
import edu.umich.dice3.gamestate.exceptions.PlayerPassedTwiceException;
import edu.umich.dice3.gamestate.exceptions.WrongPlayerActionException;

public class DiceGameState
{

    private List<Player> players;
    int playersLeft;
    private int currentTurn;
    private Bid previousBid;
    private Player gameWinner;
    private ArrayList<HistoryItem> history;
    private List<ArrayList<HistoryItem>> rounds;
    private boolean inSpecialRules;

    public DiceGameState(List<String> agentNames, int numDice)
    {
        Random random = new Random();
        long seed = random.nextLong();
        // seed = 2033829077412427714L;
        random.setSeed(seed);
        log("Setting seed " + seed);
        Die.setRandom(random);

        int numPlayers = agentNames.size();
        players = new ArrayList<Player>();
        rounds = new ArrayList<ArrayList<HistoryItem>>();
        inSpecialRules = false;
        for (int i = 0; i < numPlayers; ++i)
        {
            players.add(new Player(agentNames.get(i) + "-" + i, i, numDice, this));
        }
        playersLeft = numPlayers;
        gameWinner = null;
        currentTurn = random.nextInt(numPlayers);
        findNextPlayer();
        newRound();
    }

    /****************
     * Turn Actions *
     ****************/

    public void handleBidAction(int playerId, Bid bid) throws DiceActionException
    {
        checkPlayer(playerId);
        Player player = getPlayer(playerId);
        boolean playerSpecialRules = inSpecialRules && player.getNumDice() > 1;
        checkBid(bid, playerSpecialRules);
        previousBid = bid;
        history.add(new HistoryItem(this, player, bid));
        nextTurn();
    }

    public void handlePushAction(int playerId, int[] push) throws DiceActionException
    {
        // checkPlayer(playerId);
        // Check that the last bid was from this player, and that the next
        // player hasn't gone yet.
        HistoryItem item = getLastHistoryItem();
        if (item == null)
        {
            throw new WrongPlayerActionException(getPlayer(playerId), getCurrentPlayer());
        }
        if (item.getPlayer().getId() != playerId)
        {
            throw new WrongPlayerActionException(getPlayer(playerId), getCurrentPlayer());
        }
        if (item.getType() != Type.BID)
        {
            throw new WrongPlayerActionException(getPlayer(playerId), getCurrentPlayer());
        }

        Player player = getPlayer(playerId);
        if (push.length == 0)
        {
            throw new EmptyPushException(player);
        }
        log("Player " + playerId + " pushing:");
        for (int i : push)
        {
            log("  " + i + ", ");
        }
        log("");
        history.add(new HistoryItem(this, getPlayer(playerId), Type.PUSH));
        player.push(push);
        // nextTurn();
    }

    public void handlePassAction(int playerId) throws DiceActionException
    {
        checkPlayer(playerId);
        Player player = getPlayer(playerId);
        if (player.getHasPassed())
        {
            throw new PlayerPassedTwiceException(player);
        }
        player.setHasPassed(true);
        history.add(new HistoryItem(this, player, Type.PASS, player.allDiceSame() ? 1 : 0));
        nextTurn();
    }

    public void handleChallangeAction(int playerId, int target) throws DiceActionException
    {
        checkPlayer(playerId);
        Player player = getPlayer(playerId);
        HistoryItem item = getLastHistoryItem();
        HistoryItem secondItem = getSecondLastHistoryItem();
        if (previousBid != null && previousBid.getPlayerId() == target)
        {
            if (bidIsCorrect(previousBid))
            {
                playerLosesRound(playerId);
                HistoryItem newItem = new HistoryItem(this, player, Type.CHALLENGE_BID, previousBid.getPlayerId(), 0);
                newItem.setBid(previousBid);
                newItem.setLosingPlayer(playerId);
                history.add(newItem);
            }
            else
            {
                int loserId = previousBid.getPlayerId();
                playerLosesRound(loserId);
                HistoryItem newItem = new HistoryItem(this, player, Type.CHALLENGE_BID, previousBid.getPlayerId(), 1);
                newItem.setBid(previousBid);
                history.add(newItem);
                newItem.setLosingPlayer(loserId);
            }
        }
        else if (item.getType() == Type.PASS && item.getPlayer().getId() == target)
        {
            if (item.getResult() == 1) // Pass was legal
            {
                playerLosesRound(playerId);
                HistoryItem newItem = new HistoryItem(this, player, Type.CHALLENGE_PASS, target, 0);
                item.setLosingPlayer(playerId);
                history.add(newItem);
            }
            else
            {
                int loserId = item.getPlayer().getId();
                playerLosesRound(loserId);
                HistoryItem newItem = new HistoryItem(this, player, Type.CHALLENGE_PASS, target, 1);
                newItem.setLosingPlayer(loserId);
                history.add(newItem);
            }
        }
        else if (secondItem.getType() == Type.PASS && secondItem.getPlayer().getId() == target)
        {
            if (secondItem.getResult() == 1) // Pass was legal
            {
                playerLosesRound(playerId);
                HistoryItem newItem = new HistoryItem(this, player, Type.CHALLENGE_PASS, target, 0);
                secondItem.setLosingPlayer(playerId);
                history.add(newItem);
            }
            else
            {
                int loserId = secondItem.getPlayer().getId();
                playerLosesRound(loserId);
                HistoryItem newItem = new HistoryItem(this, player, Type.CHALLENGE_PASS, target, 1);
                newItem.setLosingPlayer(loserId);
                history.add(newItem);
            }
        }
        else
        {
            throw new ChallengeException(getPlayer(playerId), getPlayer(target));
        }
        newRound();
    }

    public void handleExactAction(int playerId) throws DiceActionException
    {
        checkPlayer(playerId);
        Player player = getPlayer(playerId);
        Bid bid = getPreviousBid();
        if (bid == null || player.getHasExacted())
        {
            throw new InvalidExactException(getPlayer(playerId));
        }
        player.setHasExacted(true);
        if (countDice(bid.getRank()) == bid.getCount())
        {
            log(getPlayer(playerId) + ": exact was correct");
            HistoryItem item = new HistoryItem(this, player, Type.EXACT, 1);
            item.setBid(bid);
            if (player.gainDie())
            {
                item.setWinningPlayer(playerId);
            }
            history.add(item);
        }
        else
        {
            log(getPlayer(playerId) + ": exact was wrong");
            playerLosesRound(playerId);
            HistoryItem item = new HistoryItem(this, player, Type.EXACT, 0);
            item.setBid(bid);
            item.setLosingPlayer(playerId);
            history.add(item);
        }
        currentTurn = playerId;
        findNextPlayer();
        newRound();
    }

    public void handleAcceptAction(int playerId) throws DiceActionException
    {
        checkPlayer(playerId);
        nextTurn();
    }

    /********************
     * Helper Functions *
     ********************/

    private void newRound()
    {
        inSpecialRules = false;
        for (Player player : players)
        {
            player.newRound();
            if (player.isInSpecialRules() && playersLeft > 2)
            {
                inSpecialRules = true;
            }
        }
        previousBid = null;
        if (history != null)
        {
            rounds.add(history);
        }
        history = new ArrayList<HistoryItem>();
    }

    private void playerLosesRound(int playerId)
    {
        log("" + getPlayer(playerId) + " lost the round.");
        log(getStateString(-1));
        Player player = getPlayer(playerId);
        player.loseDie();
        if (player.getNumDice() == 0)
        {
            playerLosesGame(playerId);
            findNextPlayer();
        }
        else
        {
            currentTurn = playerId;
            findNextPlayer();
        }
    }

    private void playerLosesGame(int playerId)
    {
        log("" + getPlayer(playerId) + " lost the game.");
        Player player = getPlayer(playerId);
        player.setLost(true);
        --playersLeft;
        if (playersLeft == 1)
        {
            someoneWinsGame();
        }
    }

    private void someoneWinsGame()
    {
        assert (playersLeft == 1);
        for (Player player : players)
        {
            if (!player.getLost())
            {
                gameWinner = player;
                log("" + player + " won the game.");
                break;
            }
        }
    }

    /**
     * 
     * @param bid
     * @param specialRules True if the game is in special rules AND the surrent player has more than one die.
     * @throws DiceActionException
     */
    private void checkBid(Bid bid, boolean specialRules) throws DiceActionException
    {
        // Check that the bid is sensical
        if (bid.getCount() < 1)
        {
            throw new InvalidBidException(bid);
        }
        int rank = bid.getRank();
        if (rank < 1 || rank > Die.NUM_SIDES)
        {
            throw new InvalidBidException(bid);
        }
        
        // Check that the current bid is an appropriate raise, if there was a previous bid.
        if (previousBid == null) return;
        if (!bid.isLegalRaise(previousBid, specialRules))
        {
            throw new InvalidRaiseException(bid, previousBid);
        }
    }

    private void checkPlayer(int playerId) throws DiceActionException
    {
        if (playerId != currentTurn)
        {
            throw new WrongPlayerActionException(getPlayer(playerId), getPlayer(currentTurn));
        }
    }

    public Player getPlayer(int playerId)
    {
        return players.get(playerId);
    }

    private void nextTurn()
    {
        currentTurn = (currentTurn + 1) % players.size();
        findNextPlayer();
    }

    /**
     * Increments currentTurn until the current player is one that hasn't lost.
     */
    private void findNextPlayer()
    {
        while (getPlayer(currentTurn).getLost())
        {
            currentTurn = (currentTurn + 1) % players.size();
        }
    }

    private int countDice(int rank)
    {
        int totalCount = 0;
        for (Player player : players)
        {
            for (Die die : player.getDice())
            {
                if (die.getValue() == rank || (!inSpecialRules && die.getValue() == 1))
                {
                    ++totalCount;
                }
            }
        }
        return totalCount;
    }

    private boolean bidIsCorrect(Bid bid)
    {
        if (countDice(bid.getRank()) >= bid.getCount())
        {
            return true;
        }
        return false;
    }

    public List<Player> getPlayers()
    {
        return players;
    }

    public int getCurrentPlayerId()
    {
        return currentTurn;
    }

    public Player getCurrentPlayer()
    {
        return getPlayer(currentTurn);
    }

    public boolean someoneWonGame()
    {
        return gameWinner != null;
    }

    public Player getWinner()
    {
        return gameWinner;
    }

    public boolean isInSpecialRules()
    {
        return inSpecialRules;
    }

    public boolean inProgress()
    {
        return !someoneWonGame();
    }

    public List<HistoryItem> getHistory()
    {
        return history;
    }

    public List<ArrayList<HistoryItem>> getRoundHistory()
    {
        return rounds;
    }

    public List<HistoryItem> getFlatHistory()
    {
        List<HistoryItem> ret = new ArrayList<HistoryItem>();
        for (ArrayList<HistoryItem> al : rounds)
        {
            ret.addAll(al);
        }
        if (history != null)
        {
            ret.addAll(history);
        }
        return ret;
    }

    // not including "visitor" for now.
    public String getPlaterStatus(int id)
    {
        Player player = getPlayer(id);
        if (player.getLost())
        {
            return "lost";
        }
        if (someoneWonGame())
        {
            // we know that player.getLost() == false.
            return "won";
        }
        return "play";
    }

    public Bid getPreviousBid()
    {
        return previousBid;
    }

    public String getStateString(int playerId)
    {
        StringBuilder sb = new StringBuilder();
        if (inSpecialRules)
        {
            sb.append("SPECIAL RULES\n\n");
        }
        for (int i = 0; i < players.size(); ++i)
        {
            Player player = players.get(i);
            sb.append(player.getStateString(player.getId() == playerId || playerId == -1));
            if (i + 1 < players.size())
            {
                sb.append('\n');
            }
        }
        // sb.append("Next: " + currentTurn + '\n');
        return sb.toString();
    }

    @Override
    public String toString()
    {
        return getStateString(-1);
    }

    public int getHistorySize()
    {
        if (history == null) return 0;
        return history.size();
    }

    public HistoryItem getLastHistoryItem()
    {
        if (history == null || history.size() == 0)
        {
            return null;
        }
        return history.get(history.size() - 1);
    }
    
    public HistoryItem getSecondLastHistoryItem()
    {
        if (history == null || history.size() < 2)
        {
            return null;
        }
        return history.get(history.size() - 2);
    }

    /**
     * @return The id of the player that made the last move if that move was a
     *         pass, or -1 otherwise.
     */
    public int getLastPassPlayerId()
    {
        HistoryItem item = getLastHistoryItem();
        if (item == null || item.getType() != Type.PASS)
        {
            return -1;
        }
        return item.getPlayer().getId();
    }

    /**
     * @return -1 if the last two moves were not both passes, or the id of the
     *         second to last player to pass otherwise.
     */
    public int getSecondLastPassPlayerId()
    {
        if (getLastPassPlayerId() == -1)
        {
            return -1;
        }

        HistoryItem item = getSecondLastHistoryItem();
        if (item == null || item.getType() != Type.PASS)
        {
            return -1;
        }
        return item.getPlayer().getId();
    }

    public static void log(String message)
    {
        SoarMatch.debug(message);
    }

    public void playerMadeIllegalMove(int playerId)
    {
        playerLosesRound(playerId);
        history.add(new HistoryItem(this, getPlayer(playerId), Type.ILLEGAL));
        newRound();
    }
}
