package edu.umich.robot.april;

/**
 * A SimObject is created by the Simulator by examining a configuration file. The SimObject has access to all of the simulator state, but otherwise acts like an independent application: it subscribes and publishes LCM messages as though it were a normal app.
 * 
 * They must implement the special constructor (Simulator, Config) so that the Simulator can instantiate them.
 **/
public interface SoarSimObject
{
    // must implement constructor with signature:
    // public SimObject(Simulator sim, String name, Config config);
    
    void dispose();
}
