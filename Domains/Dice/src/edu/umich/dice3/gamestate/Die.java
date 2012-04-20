package edu.umich.dice3.gamestate;

import java.util.Collection;
import java.util.Random;

public class Die {
    
    public static final int NUM_SIDES = 6;

	// Shared Random insatnce
	private static Random random;
	
	public static void setRandom(Random random)
	{
	    Die.random = random;
	}
	
	private int value;
	private boolean pushed;
	
	public Die()
	{
		roll();
		pushed = false;
	}
	
	public void roll()
	{
		value = random.nextInt(NUM_SIDES) + 1;
	}
	
	public int getValue()
	{
		return value;
	}
	
	public void push()
	{
		pushed = true;
	}
	
	public boolean getPushed()
	{
		return pushed;
	}
	
	@Override
	public String toString() {
		return "" + value + (pushed ? "*" : "");  
	}
	
	public static int[] countDice(Collection<Die> dice)
	{
	    int[] counts = new int[NUM_SIDES];
	    for (Die die : dice)
	    {
	        counts[die.getValue() - 1] += 1;
	    }
	    return counts;
	}
	 
}
