package edu.umich.robot.april;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import april.config.Config;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisTexture;
import april.vis.VisWorld;


public class ViewMapImage implements ViewObject
{
    Viewer viewer;
    String name;
    Config config;

    public ViewMapImage(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        this.name = name;
        this.config = config;
        String imagePath = config.getPath("obstacles.image_path");
        System.out.println("obstacles.image_path: " + imagePath);
        if (imagePath != null)
        {
            try
            {
                BufferedImage im = javax.imageio.ImageIO.read(new File(imagePath));
                int width = im.getWidth();
                int height = im.getHeight();
                double origin[] = config.getDoubles("obstacles.image_origin", new double[] { width / 2.0, height / 2.0 });
                double metersPerPixel = config.getDouble("obstacles.meters_per_pixel", .1);
                double xy0[] = new double[] { -origin[0] * metersPerPixel, (origin[1] - height) * metersPerPixel };
                double xy1[] = new double[] { (width - origin[0]) * metersPerPixel, origin[1] * metersPerPixel };
                VisWorld.Buffer vbim = viewer.getVisWorld().getBuffer("MAPIM");
                VisTexture tex = new VisTexture(im);
                //tex.lock();
                //tex.setMipMapping(true);
                //vbim.addBuffered(new VisDepthTest(false, new VisLighting(false, new VisImage(tex, xy0, xy1, true))));
                //vbim.addBuffered(new VisImage(tex, xy0, xy1, true));
                vbim.switchBuffer();
                //vbim.setDrawOrder(-100);
            } catch (IOException ex)
            {
                System.out.println("Couldn't read " + imagePath);
            }
        }
    }
}
