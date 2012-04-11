package edu.umich.robot.april;

import java.awt.*;

import april.vis.*;
import april.jmat.*;

public class ModelS
{
    // Wheel axis goes through rear axle.
    public static final double chassisFwd = 0.2;      // how far does chassis extend in front of wheels?
    public static final double chassisBack = 0.6;     // how far does chassis extend behind wheels?
    public static final double chassisWidth = 0.4;    // total width of chassis
    public static final double chassisHeight = 0.3;   // height of chassis.
    public static final double groundClearance = 0.1; // space between ground and chassis
    public static final double cameraFwd = 0;         // x coordinate of camera
    public static final double cameraHeight = chassisHeight + groundClearance;    // z coordinate of camera
    public static final double cameraSize = 0.2;
    public static final double wheelDiameter = 0.3;   // diameter of main drive wheels
    public static final double rearWheelDiameter = groundClearance;
    public static final double rearWheelBack = 0.5;

    public static final Color wheelColor = new Color(.25f, .25f, .25f);
    public static final Color cameraColor = new Color(234, 229, 147);


    public static final HersheyFont hf = HersheyFont.getDefaultFont();

    public static VisObject createRobot(Color color)
    {
        double hfscale = chassisWidth * 0.8 / hf.getMaxCharacterHeight();

        return new VisChain(new VisBox((chassisFwd - chassisBack)/2.0, 0, chassisHeight/2 + groundClearance,
                                       chassisFwd + chassisBack, chassisWidth, chassisHeight,
                                       color),
                            new VisChain(LinAlg.translate(0, chassisWidth/2+0.01, wheelDiameter/2),
                                         LinAlg.rotateX(Math.PI/2),
                                         new VisCylinder(wheelDiameter/2, 0.1, new VisDataFillStyle(wheelColor))),
                            new VisChain(LinAlg.translate(0, -chassisWidth/2-0.01, wheelDiameter/2),
                                         LinAlg.rotateX(Math.PI/2),
                                         new VisCylinder(wheelDiameter/2, 0.1, new VisDataFillStyle(wheelColor))),
                            new VisChain(LinAlg.translate(-rearWheelBack, 0, rearWheelDiameter/2),
                                         LinAlg.rotateX(Math.PI/2),
                                         new VisCylinder(rearWheelDiameter/2, 0.1, new VisDataFillStyle(wheelColor))),
                            //new VisChain(LinAlg.translate(0, 0, chassisHeight + groundClearance),
                            //             LinAlg.scale(hfscale, hfscale, hfscale),
                            //             new VisHersheyText(hf, ""+robotid, new VisDataLineStyle(Color.white, 6))),
                            new VisChain(LinAlg.translate(cameraFwd, 0, cameraHeight),
                                         new VisBox(0,0,cameraSize / 2,cameraSize, cameraSize, cameraSize, cameraColor)));

    }

    private static final double selectionRadius = 0.5;
    private static final double selectionHeight = 0.01;
    public static final Color selectionColor = new Color(.1f, .4f, 1f);

    public static VisObject createSelectionCircle() {
        return new VisCylinder(selectionRadius, selectionHeight, selectionColor);
    }
}