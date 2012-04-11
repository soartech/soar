package edu.umich.robot.april;

import java.util.ArrayList;
import java.awt.*;
import april.vis.*;
import april.jmat.*;
import april.config.*;

import javax.swing.*;


public class Model4 extends VisChain
{
    public static final HersheyFont hf = HersheyFont.getDefaultFont();

    static final double in2m = 2.54/100;  // convert inches to meters

    static MyColorizer colorizer = new MyColorizer();

    public Model4()
    {
        this(null, Color.black, 1);
    }

    public Model4(Config config, Color chassisColor, double opacity)
    {
        this(config, -1, chassisColor, opacity);
    }

    public Model4(Config config, int robotID, Color chassisColor, double opacity)
    {
        // colors
        int alpha = (int) Math.min(255, Math.max(0, (opacity*255)));
        Color gray  = new Color(120, 120, 120, alpha);
        Color cyan  = new Color(  0, 255, 255, alpha);
        Color green = new Color(  0, 255,   0, alpha);
        Color red   = new Color(255,   0,   0, alpha);
        Color black = new Color(  0,   0,   0, alpha);

        if (chassisColor == null)
            chassisColor = new Color(45, 45, 45, alpha);
        else
            chassisColor = new Color(chassisColor.getRed(), chassisColor.getGreen(),
                                     chassisColor.getBlue(), alpha);

        Color wheelColor     = new Color(Color.black.getRed(), Color.black.getGreen(), Color.black.getBlue(), alpha);
        Color absColor       = new Color(230, 215, 170, alpha);
        VisObject wheel      = new VisCylinder(3.9375*in2m, 2*in2m, new VisDataFillStyle(wheelColor));
        VisObject axel       = new VisCylinder(0.5*in2m, 7.05*in2m, new VisDataFillStyle(gray));
        VisObject motorMount = new VisBox(6*in2m, 3*in2m, 3*in2m, new VisDataFillStyle(gray));

        double cfgRadius  = .5;
        double center[] = new double[] {5.21875, 0, 0};

        double hfscale = 8*in2m / hf.getMaxCharacterHeight();

        if (config != null)
        {
            cfgRadius = config.getDouble("robot.geometry.radius", 0.001);
            center[0] = config.getDoubles("robot.geometry.circles_x", new double[] {center[0]})[0];
            center[1] = config.getDoubles("robot.geometry.circles_y", new double[] {center[1]})[0];
        }
        VisCircle circleFwd = new VisCircle(cfgRadius, new VisDataFillStyle(new Color(  0, 255,   0, 128)));
        VisCircle circleBck1 = new VisCircle(cfgRadius, new VisDataFillStyle(new Color(  0,   0,   0, 128)));
        VisCircle circleBck2 = new VisCircle(cfgRadius, new VisDataFillStyle(new Color(  0,   0,   0, 128)));
        circleFwd.setThetaRange( -Math.PI/4,  Math.PI/4);
        circleBck1.setThetaRange( Math.PI/4,    Math.PI);
        circleBck2.setThetaRange(  -Math.PI, -Math.PI/4);

        add(
            // pose origin
            new VisCylinder(0.25*in2m, 4*in2m, new VisDataFillStyle(cyan)),
            new VisChain(LinAlg.translate(center[0]*in2m, center[1]*in2m, center[2]*in2m),
                         new VisCylinder(0.25*in2m, 4*in2m, new VisDataFillStyle(green))),

            // chassis
            new VisChain(LinAlg.translate(0*in2m, center[1]*in2m, 9.75*in2m),
                         new VisBox(16.75*in2m, 14.5*in2m, 6.5*in2m, chassisColor)),
            new VisChain(LinAlg.translate(0*in2m, center[1]*in2m, 4.75*in2m),
                         new VisBox(16.75*in2m, 5.5*in2m, 3.5*in2m, chassisColor)),
            new VisChain(LinAlg.translate(8.375*in2m, 0*in2m, 6.125*in2m),
                         LinAlg.rotateY(Math.PI/4),
                         new VisBox(4.42*in2m, 5.5*in2m, 4.42*in2m, chassisColor)),
            new VisChain(LinAlg.translate(-8.375*in2m, 0*in2m, 6.125*in2m),
                         LinAlg.rotateY(Math.PI/4),
                         new VisBox(4.42*in2m, 5.5*in2m, 4.42*in2m, chassisColor)),

            // wheels
            new VisChain(LinAlg.translate(-center[0]*in2m, 9.125*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         wheel),
            new VisChain(LinAlg.translate(-center[0]*in2m, -9.125*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         wheel),
            new VisChain(LinAlg.translate(center[0]*in2m, 9.125*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         wheel),
            new VisChain(LinAlg.translate(center[0]*in2m, -9.125*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         wheel),

            // axels (i mean, we already knew the wheel center translations...)
            new VisChain(LinAlg.translate(-center[0]*in2m, 6.625*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         axel),
            new VisChain(LinAlg.translate(-center[0]*in2m, -6.625*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         axel),
            new VisChain(LinAlg.translate(center[0]*in2m, 6.625*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         axel),
            new VisChain(LinAlg.translate(center[0]*in2m, -6.625*in2m, 3.9375*in2m),
                         LinAlg.rotateX(Math.PI/2),
                         axel),

            // motor mounts
            new VisChain(LinAlg.translate((-center[0] + 0.59375)*in2m, 5.5*in2m, 4.75*in2m),
                         motorMount),
            new VisChain(LinAlg.translate((-center[0] + 0.59375)*in2m, -5.5*in2m, 4.75*in2m),
                         motorMount),
            new VisChain(LinAlg.translate((center[0] - 0.59375)*in2m, 5.5*in2m, 4.75*in2m),
                         motorMount),
            new VisChain(LinAlg.translate((center[0] - 0.59375)*in2m, -5.5*in2m, 4.75*in2m),
                         motorMount),

            // sensor head
            new VisChain(LinAlg.translate((center[0] - 0.09375)*in2m, 0.5*in2m, 16.5*in2m),
                         new VisBox(6.5*in2m, 6*in2m, 7*in2m, new VisDataFillStyle(chassisColor))),
            new VisChain(LinAlg.translate((center[0] - 0.09375)*in2m, 0.5*in2m, 20.0625*in2m),
                         new VisBox(6.5*in2m, 6*in2m, 0.125*in2m, new VisDataFillStyle(absColor))),


            // estop
            new VisChain(LinAlg.translate((-center[0] + 1.21875)*in2m, -6*in2m, 13.5*in2m),
                         new VisCylinder(0.75*in2m, 1*in2m, new VisDataFillStyle(red))),

            // beacon
            //new VisChain(LinAlg.translate(0*in2m, 0*in2m, 15.5*in2m),
            //             new VisCylinder(1*in2m, 5*in2m, new VisDataFillStyle(black))),
            //new VisChain(LinAlg.translate(0*in2m, 0*in2m, 18.125*in2m),
            //             new VisCylinder(1*in2m, 0.25*in2m, new VisDataFillStyle(absColor))),

            // label
            robotID >=0 ? new VisChain(LinAlg.translate(-3*center[0]*in2m, 0*in2m, 9.75*in2m),
                                       new VisText(new double[3], VisText.ANCHOR.CENTER,""+robotID)) : null,

            // configuration space circles
            new VisChain(LinAlg.translate(0*in2m, 0*in2m, 0.3*in2m), circleBck1, circleBck2, circleFwd)
            );
    }

    // translucent factory methods
    public static VisObject makeTranslucent()
    {
        return new Model4(null, Color.black, 0.5);
    }

    public static VisObject makeTranslucent(Config config)
    {
        return new Model4(config, Color.black, 0.5);
    }

    public static VisObject makeTranslucent(Config config, Color chassisColor)
    {
        return new Model4(config, chassisColor, 0.5);
    }

    // opaque factory methods
    public static VisObject make()
    {
        return new Model4(null, null, 1);
    }

    public static VisObject make(Config config)
    {
        return new Model4(config, null, 1);
    }

    public static VisObject Model4Head(Config config, double camPan, double camTilt, double hok)
    {
        return Model4Head(config, camPan, camTilt, hok, false);
    }

    public static VisObject Model4Head(Config config, double camPan, double camTilt, double hok, boolean laserEn)
    {
        double poseToPan[][] = ConfigUtil.getRigidBodyTransform(config, "cameraCalibration.extrinsics.poseToPan");
        double panToTilt[][] = ConfigUtil.getRigidBodyTransform(config, "cameraCalibration.extrinsics.panToTilt");
        double tiltToCam[][] = ConfigUtil.getRigidBodyTransform(config, "cameraCalibration.extrinsics.tiltToCam");

        VisObject servo  = new VisBox( 0.00000,  0.01325, -0.01875,
                                       0.02600,  0.04850,  0.03750,
                                       new Color(40, 40, 40));

        VisObject camera = new VisChain(new VisBox(0.000, 0.000, 0.000,
                                                   0.043, 0.034, 0.020,
                                                   new Color(40,40,40)),
                                        LinAlg.translate(0.000, 0.000, 0.017),
                                        new VisCylinder(0.0165,
                                                        0.0340,
                                                        new VisDataFillStyle(new Color(40,40,40))));

        VisObject laser  = new VisChain(LinAlg.translate(0.5, 0, 0),
                                        LinAlg.rotateY(Math.PI/2),
                                        new VisCylinder(0.005, 1, Color.red));

        VisObject hokuyo = new VisChain(new VisBox(0.000, 0.000, 0.000,
                                                   0.060, 0.060, 0.038,
                                                   new Color(40, 40, 40)),
                                        LinAlg.translate(0.000, 0.000, 0.0215),
                                        new VisBox(0.000, 0.000, 0.000,
                                                   0.052, 0.052, 0.005,
                                                   new Color(50, 50, 50)),
                                        LinAlg.translate(0.000, 0.000, 0.0135),
                                        new VisCylinder(0.0230, 0.030, new Color(40, 40, 40)),
                                        LinAlg.translate(0.000, 0.000, 0.0195),
                                        new VisBox(0.000, 0.000, 0.000,
                                                   0.045, 0.045, 0.009,
                                                   new Color(235,135,40)),
                                        LinAlg.translate(0.000, 0.000, 0.0090),
                                        new VisBox(0.000, 0.000, 0.000,
                                                   0.045, 0.045, 0.009,
                                                   new Color(40,40,40))
                                        );

        double cam[][] = LinAlg.multiplyMany(poseToPan,
                                             LinAlg.rotateZ(camPan),
                                             panToTilt,
                                             LinAlg.rotateZ(camTilt),
                                             tiltToCam);
        VisChain c = new VisChain();
        // camera setup
        c.add(new VisChain( poseToPan,
                            servo));
                            //new VisAxes()));

        c.add(new VisChain( poseToPan,
                            LinAlg.rotateZ(camPan),
                            panToTilt,
                            servo));
                            //new VisAxes()));

        c.add(new VisChain( poseToPan,
                            LinAlg.rotateZ(camPan),
                            panToTilt,
                            LinAlg.rotateZ(camTilt),
                            tiltToCam,
                            camera));

        // laser
        if (laserEn)
            c.add(new VisChain( poseToPan,
                                LinAlg.rotateZ(camPan),
                                panToTilt,
                                LinAlg.rotateZ(camTilt),
                                tiltToCam,
                                LinAlg.rotateX(Math.PI/2),
                                LinAlg.rotateZ(Math.PI/2),
                                LinAlg.translate(0, 0, 0.022),
                                laser));

        // hokuyo setup
        c.add(new VisChain( poseToPan,
                            LinAlg.translate( 0.0930, 0.0340,-0.0410),
                            LinAlg.rotateZ(Math.PI),
                            LinAlg.rotateX(-Math.PI/2),
                            servo));

        c.add(new VisChain( poseToPan,
                            LinAlg.translate( 0.0930, 0.0340,-0.0410),
                            LinAlg.rotateZ(Math.PI),
                            LinAlg.rotateX(-Math.PI/2),
                            LinAlg.translate( 0.0000, 0.0000, 0.0340),
                            LinAlg.rotateX(Math.PI/2),
                            LinAlg.rotateZ(-Math.PI),
                            LinAlg.translate( 0.0000, 0.0000,-0.0090),
                            LinAlg.rotateY(-hok),
                            hokuyo));
        return c;
    }


    public static VisObject Model4Flag(boolean displayFlag)
    {
        VisChain c = new VisChain();

        if (displayFlag) {
            ArrayList<double[]> points = new ArrayList<double[]>();
            ArrayList<int[]> triangles = new ArrayList<int[]>();

            triangles.add(new int[]{0,1,3});
            triangles.add(new int[]{0,2,3});
            triangles.add(new int[]{0,1,2});
            triangles.add(new int[]{1,2,3});
            points.add(new double[]{0, 0, 0});
            points.add(new double[]{-0.25, 0.075, 0.1});
            points.add(new double[]{-0.25, -0.075, 0.1});
            points.add(new double[]{0, 0, 0.2});

            VisObject pole  = new VisChain(LinAlg.translate(0, 0, (13+18)*in2m),
                                           new VisCylinder(0.005, 36*in2m, Color.lightGray),
                                           LinAlg.translate(0, 0, 18*in2m),
                                           new VisTriangles(colorizer, points, triangles)
                );


            c.add(pole);
        }
        return c;
    }

//    // Scale invariant wedge
//    public static VisObject makeWedge(double pixelRadius, int col)
//    {
//        int cint =  0x00ffffff & col;
//
//        VisChain c = new VisChain();
//        Color bg = new Color(0x88000000 | cint, true);
//        Color edge = new Color(0xff000000 | cint, true);
//
//        c.add(new VisCircle(pixelRadius, new VisDataFillStyle(bg),
//                            new VisDataLineStyle(edge, 2.1, true)));
//
//        // For making the wedge.
//        double rad = 3*Math.PI/4;
//        double wedgeScale = .9;
//        double crad = pixelRadius*Math.cos(rad)*wedgeScale;
//        double srad = pixelRadius*Math.sin(rad)*wedgeScale;
//
//        return new VisChain(LinAlg.translate(.13,0,0),new VisScreenCoordinates(new double[3], c));
//    }


    static class MyColorizer implements april.vis.Colorizer
    {
        public int colorize(double p[])
        {
            return Color.blue.getRGB();
        }
    }
}
