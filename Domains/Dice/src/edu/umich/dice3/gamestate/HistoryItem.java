package edu.umich.dice3.gamestate;

public class HistoryItem {
	public static enum Type
	{
		ACCEPT,
		BID,
		PUSH,
		CHALLENGE_BID,
		CHALLENGE_PASS,
		EXACT,
		PASS,
		ILLEGAL;

		public String getName() {
		    if (this == CHALLENGE_BID || this == CHALLENGE_PASS)
		    {
		        return "challenge";
		    }
			return toString().toLowerCase();
		}
	}
	
	private DiceGameState game;
	private Player player;
	private Type type;
	private int value;
	private int result;
	private String state;
	
	// if losingPlayer != -1, indicates a player that lost a die on this turn.
	private int losingPlayer;
	
    // if winningPlayer != -1, indicates a player that won back a die on this turn.
    private int winningPlayer;	
	
    // only for when type == Type.BID
	private Bid bid;
	
	public HistoryItem(DiceGameState game, Player player, Type type, int value, int result)
	{
		this.game = game;
		this.player = player;
		this.type = type;
		this.value = value;
		this.result = result;
		state = game.getStateString(-1);
        losingPlayer = -1;
        winningPlayer = -1;
	}
	
	public HistoryItem(DiceGameState game, Player player, Type type, int value)
	{
		this(game, player, type, value, value);
	}
	
	public HistoryItem(DiceGameState game, Player player, Type type)
	{
		this(game, player, type, -1, -1);
	}
	
	public HistoryItem(DiceGameState game, Player player, Bid bid)
	{
		this(game, player, Type.BID);
		this.bid = bid;
	}

	public HistoryItem(DiceGameState game, Player player, Bid bid, int result)
	{
		this(game, player, Type.EXACT, -1, result);
		this.bid = bid;
	}
	
	public Player getPlayer()
	{
		return player;
	}
	
	public Type getType()
	{
		return type;
	}
	
	public int getValue()
	{
		return value;
	}
	
	public int getResult()
	{
		return result;
	}
	
	public Bid getBid()
	{
		return bid;
	}
	
	public void setBid(Bid bid)
	{
	    this.bid = bid;
	}
	
	public String getState()
	{
		return state;
	}
	
	@Override
	public String toString() {
	    String first = null;
	    String playerName = "Player #" + player.getId();
		switch (type)
		{
		case ACCEPT:
			first = playerName + " accepted";
			break;
		case BID:
		    first = "" + bid;
		    break;
		case PUSH:
		    first = playerName + " pushed";
		    break;
		case CHALLENGE_BID:
		    first = playerName + " challenged bid (" + bid + "), result " + result;
		    break;
		case CHALLENGE_PASS:
		    first = playerName + " challenged player #" + value + "'s pass, result " + result;
		    break;
		case EXACT:
		    first = playerName + " exacted (" + bid + "), result " + result;
		    break;
		case PASS:
		    first = playerName + " passed";
		    break;
		case ILLEGAL:
		    first = playerName + " made an illegal move";
		    break;
		}
		String second = null;
		if (losingPlayer != -1)
		{
		    second = "(Player #" + losingPlayer + " lost a die.)";
		}
		else if (winningPlayer != -1)
		{
            second = "(Player #" + winningPlayer + " won a die.)";
		}
		if (second == null)
		{
		    return first;
		}
		return first + ' ' + second;
	}

    public void setLosingPlayer(int playerId)
    {
        this.losingPlayer = playerId;
    }
    
    public void setWinningPlayer(int playerId)
    {
        this.winningPlayer = playerId;
    }
}
