package april.util;

/** Execution time measurement. **/
public class Tic
{
    long initTime;
    long startTime;

    /** Includes an implicit call to tic() **/
    public Tic()
    {
        initTime = System.nanoTime();
        startTime = initTime;
    }

    /** Begin measuring time from now. **/
    public void tic()
    {
        startTime = System.nanoTime();
    }

    /** How much time has passed since the most recent call to tic()? **/
    public double toc()
    {
        long endTime = System.nanoTime();
        double elapsedTime = (endTime-startTime)/1000000000f;

        return elapsedTime;
    }

    /** Equivalent to toc() followed by tic() **/
    public double toctic()
    {
        long endTime = System.nanoTime();
        double elapsedTime = (endTime-startTime)/1000000000f;

        tic();
        return elapsedTime;
    }

    /** How much time has passed since the object was created? **/
    public double totalTime()
    {
        long endTime = System.nanoTime();
        double elapsedTime = (endTime-initTime)/1000000000f;

        return elapsedTime;
    }
}
