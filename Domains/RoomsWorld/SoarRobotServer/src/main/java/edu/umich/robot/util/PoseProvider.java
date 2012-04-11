package edu.umich.robot.util;

import java.util.ArrayList;
import java.util.List;

import april.lcmtypes.pose_t;

public abstract class PoseProvider
{
    public interface PoseChangedListener
    {
        void onPoseChanged(pose_t pose);
    }
    
    public abstract pose_t getPose();
    
    List<PoseChangedListener> poseChangedListeners = new ArrayList<PoseChangedListener>();
    
    public void addPoseChangedListener(PoseChangedListener listener)
    {
        poseChangedListeners.add(listener);
    }
    
    public void removePoseChangedListener(PoseChangedListener listener)
    {
        poseChangedListeners.remove(listener);
    }
    
    public void poseChanged()
    {
        pose_t pose = getPose();
        for (PoseChangedListener listener : poseChangedListeners)
        {
            listener.onPoseChanged(pose);
        }
    }

}
