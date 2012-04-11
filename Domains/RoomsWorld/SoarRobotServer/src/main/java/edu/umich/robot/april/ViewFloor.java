/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.umich.robot.april;

import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.io.File;
import java.io.IOException;

import april.config.Config;
import april.util.ImageUtil;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisImage;
import april.vis.VisTexture;
import april.vis.VisWorld;


/**
 * 
 * @author miller
 */
public class ViewFloor implements ViewObject
{
    private final Viewer viewer;

    private final String name;

    private final Config config;

    public ViewFloor(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        this.name = name;
        this.config = config;

        String imagePath = config.getPath("obstacles.image_path");
        String floorTexturePath = config
                .getPath("obstacles.floor_texture_path");
        try
        {
            if (imagePath != null && floorTexturePath != null)
            {
                new Builder(imagePath, floorTexturePath);
            }
        }
        catch (Throwable e)
        {
            e.printStackTrace();
        }
    }

    private class Builder
    {
        int size;

        int[] im_pixels;

        VisTexture texture;

        double metersPerPixel;

        double[] origin;

        VisWorld.Buffer vb;

        Builder(String imagePath, String texturePath)
        {
            try
            {
                BufferedImage tex = javax.imageio.ImageIO.read(new File(
                        texturePath));
                texture = new VisTexture(tex);
                texture.setMipMapping(true);
                texture.setMagFilter(true);

                BufferedImage im = javax.imageio.ImageIO.read(new File(
                        imagePath));
                im = ImageUtil.convertImage(im, BufferedImage.TYPE_INT_RGB);

                DataBuffer db = im.getRaster().getDataBuffer();
                assert db instanceof DataBufferInt;

                im_pixels = ((DataBufferInt) db).getData();
                size = im.getWidth();
                assert size == im.getHeight();

            }
            catch (IOException ex)
            {
                System.out.println("Couldn't read " + imagePath);
                return;
            }

            origin = config.getDoubles("obstacles.image_origin", new double[] {
                    size / 2.0, size / 2.0 });
            metersPerPixel = config.getDouble("obstacles.meters_per_pixel", .1);

            vb = viewer.getVisWorld().getBuffer("Floor");
            /*
             * for (int x = 0; x < size; ++x) { for (int y = 0; y < size; ++y) {
             * process(x, y); } }
             */
            double[] xy1 = pixelToWorld(0, 0);
            double[] xy2 = { xy1[0] + size * metersPerPixel,
                    xy1[1] - size * metersPerPixel };

            VisImage image = new VisImage(texture, xy1, xy2);
            
            // TODO SoarApril
            // image.setRepeat(16);
            
            vb.addBuffered(image);
            vb.switchBuffer();
        }

        private void process(int x, int y)
        {
            if (!sample(x, y))
            {
                double[] xy1 = pixelToWorld(x, y);
                double[] xy2 = { xy1[0] + .2, xy1[1] - .2 };
                vb.addBuffered(new VisImage(texture, xy1, xy2));
            }
        }

        private double[] pixelToWorld(int x, int y)
        {
            // see SimWorld2D.privateSampleRGB
            double[] xy = new double[2];
            xy[0] = (x - origin[0]) * metersPerPixel;
            xy[1] = (y - origin[1]) * -metersPerPixel;
            return xy;
        }

        private boolean sample(int x, int y)
        {
            return (im_pixels[y * size + x] & 0xff) >= 128;
        }
    }
}
