package edu.umich.dice3.ui;

import edu.umich.dice3.gamestate.Bid;

public class UIAction {
	
	public static enum Type
	{
		BID,
		CHALLENGE_BID,
		CHALLENGE_PASS,
		EXACT,
		PASS,
		SLEEP;
		
		public String getName()
		{
			char[] chars = toString().toLowerCase().toCharArray();
			boolean lastWasSpace = true;
			for (int i = 0; i < chars.length; ++i)
			{
				if (chars[i] == '_')
				{
					chars[i] = ' ';
					lastWasSpace = true;
				}
				else if (lastWasSpace)
				{
					chars[i] = Character.toUpperCase(chars[i]);
					lastWasSpace = false;
				}
			}
			return new String(chars);
		}
	}
	
	private Type type;
	private Bid bid;
	private int[] faces;
	
	public UIAction(Type type)
	{
		this.type = type;
	}
	
	public String getName()
	{
		return type.getName();
	}

	public Type getType() {
		return type;
	}
	
	public void setBid(Bid bid)
	{
		this.bid = bid;
	}

	public Bid getBid() {
		return bid;
	}

	public void setFaces(int[] faces)
	{
		this.faces = faces;
	}
	
	public int[] getFaces() {
		return faces;
	}
}
