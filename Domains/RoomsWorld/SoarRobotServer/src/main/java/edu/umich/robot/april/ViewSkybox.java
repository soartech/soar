/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.umich.robot.april;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.imageio.ImageIO;

import april.config.Config;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisImage;
import april.vis.VisLighting;
import april.vis.VisTexture;
import april.vis.VisWorld.Buffer;


/**
 *
 * @author miller
 */
public class ViewSkybox implements ViewObject {

    private static final double distance = 32;
    private static final double height = 32;

    public ViewSkybox(Viewer viewer, String name, Config config) {
        String northImagePath = config.getPath("north_image");
        String southImagePath = config.getPath("south_image");
        String eastImagePath = config.getPath("east_image");
        String westImagePath = config.getPath("west_image");
        String upImagePath = config.getPath("up_image");
        String downImagePath = config.getPath("down_image");

        try {
            VisTexture northTexture = new VisTexture(ImageIO.read(new File(northImagePath)));
            VisTexture southTexture = new VisTexture(ImageIO.read(new File(southImagePath)));
            VisTexture eastTexture = new VisTexture(ImageIO.read(new File(eastImagePath)));
            VisTexture westTexture = new VisTexture(ImageIO.read(new File(westImagePath)));
            VisTexture upTexture = new VisTexture(ImageIO.read(new File(upImagePath)));
            VisTexture downTexture = new VisTexture(ImageIO.read(new File(downImagePath)));


            northTexture.setMagFilter(true);
            southTexture.setMagFilter(true);
            eastTexture.setMagFilter(true);
            westTexture.setMagFilter(true);
            upTexture.setMagFilter(true);
            downTexture.setMagFilter(true);

            Buffer vb = viewer.getVisWorld().getBuffer("Skybox");
            double[] xy1 = { -distance, -distance };
            double[] xy2 = { distance, -distance };
            double[] zz = { -height / 2, height };
            vb.addBuffered(new VisLighting(false, new VisVerticalImage(northTexture, xy2, xy1, zz)));
            xy1 = new double[] { distance, -distance };
            xy2 = new double[] { distance, distance };
            vb.addBuffered(new VisLighting(false, new VisVerticalImage(westTexture, xy2, xy1, zz)));
            xy1 = new double[] { distance, distance };
            xy2 = new double[] { -distance, distance };
            vb.addBuffered(new VisLighting(false, new VisVerticalImage(southTexture, xy2, xy1, zz)));
            xy1 = new double[] { -distance, distance };
            xy2 = new double[] { -distance, -distance };
            vb.addBuffered(new VisLighting(false, new VisVerticalImage(eastTexture, xy2, xy1, zz)));

            xy1 = new double[] { -distance, -distance };
            xy2 = new double[] { distance, distance };
            vb.addBuffered(new VisLighting(false, new VisImage(upTexture, xy1, xy2, height, true)));
            vb.addBuffered(new VisLighting(false, new VisImage(downTexture, xy1, xy2, -height / 2, false)));

            vb.switchBuffer();
        } catch (IOException ex) {
            Logger.getLogger(ViewSkybox.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
