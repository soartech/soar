package edu.umich.robot.laser;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TestUrgBinner
{
    @Test(expected=IllegalArgumentException.class)
    public void testCtor1() throws Exception
    {
        new UrgBinner("URG_RANGE", -1, 0);
    }
    
    @Test(expected=IllegalArgumentException.class)
    public void testCtor2() throws Exception
    {
        new UrgBinner("URG_RANGE", 0, 0);
    }
    
    @Test(expected=IllegalArgumentException.class)
    public void testCtor3() throws Exception
    {
        new UrgBinner("URG_RANGE", 0, -1);
    }
    
    @Test
    public void testBinnedSize()
    {
        for (int i = 1; i < 10; ++i)
        {
            UrgBinner ub1 = new UrgBinner("URG_RANGE", i, Math.PI);
            assertEquals(ub1.getBinned().size(), i);
        }
    }

    @Test
    public void testCtor5()
    {
        new UrgBinner("URG_RANGE", 1, Math.PI);
    }
    
    @Test
    public void testCtor6()
    {
        new UrgBinner("URG_RANGE", 1, Math.PI * 2);
    }
    
    @Test(expected=IllegalArgumentException.class)
    public void testCtor7() throws Exception
    {
        new UrgBinner("URG_RANGE", 1, Math.PI * 2.01);
    }
    
}
