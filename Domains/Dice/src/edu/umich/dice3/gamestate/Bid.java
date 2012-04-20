package edu.umich.dice3.gamestate;

public class Bid {

	
	private int playerId;
	private int count;
	private int rank;
	private int[] push;
	
	public Bid(int player, int count, int rank)
	{
		this.playerId = player;
		this.count = count;
		this.rank = rank;
		this.push = new int[0];
	}
	
	@Override
	public String toString() {
		return "Player #" + playerId + " bids " + count + " " + rank + "s";
	}
	
	public void setPush(int[] push)
	{
		this.push = push;
	}
	
	public int[] getPush()
	{
		return push;
	}
	
	/**
	 * 
	 * @param previous
	 * @param specialRules True if the game is in special rules AND the current player has more than one die.
	 * @return
	 */
	public boolean isLegalRaise(Bid previous, boolean specialRules)
	{
	    // In special rules, this player may not change the face of the bid.
	    if (specialRules && previous.rank != this.rank)
	    {
	        return false;
	    }
	    
		// You can increase a bid by bidding ones by dividing the quantity of dice by two, rounding up if it's necessary
		if (this.rank == 1 && previous.rank != 1)
		{
			int required = (int) Math.ceil(previous.count / 2.0);
			if (this.count >= required)
			{
				return true;
			}
		}
		else if (previous.rank == 1 && this.rank != 1)
		{
			int required = previous.count * 2 + 1;
			if (this.count >= required)
			{
				return true;
			}
		}
		else
		{
			// If a bid increases in dice rank, the number of dice may remain constant.
			if (this.rank > previous.rank
					&& this.count == previous.count)
			{
				return true;
			}
		
			// If a bid increases the number of dice, then the rank of dice can either be changed or kept the same.
			if (this.count > previous.count)
			{
				return true;
			}
		}
		return false;
	}

	public int getRank() {
		return rank;
	}
	
	public int getCount() {
		return count;
	}

	public int getPlayerId() {
		return playerId;
	}
	
}
