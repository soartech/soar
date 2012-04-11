package edu.umich.robot.util;

public interface VelocitiesController extends Updatable
{

    void reset();

    void setAngular(double av);

    void setLinear(double lv);

}
