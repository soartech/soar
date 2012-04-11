package edu.umich.robot.april;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.io.File;
import java.io.IOException;

import april.config.Config;
import april.jmat.LinAlg;
import april.util.ImageUtil;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisDataFillStyle;
import april.vis.VisRectangle;
import april.vis.VisTexture;
import april.vis.VisWorld;

public class ViewWalls implements ViewObject
{
    private final Viewer viewer;

    //private final String name;

    private final Config config;

    public ViewWalls(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        //this.name = name;
        this.config = config;

        String imagePath = config.getPath("obstacles.image_path");
        String wallTexturePath = config.getPath("obstacles.wall_texture_path");
        try
        {
            if (imagePath != null && wallTexturePath != null)
                new Builder(imagePath, wallTexturePath);
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

            vb = viewer.getVisWorld().getBuffer("Walls");

            // compare each pixel to the four surrounding it
            // if the pixels are different, create a wall between them
            int from = -1;
            int len = -1;
            for (int x = 0; x < size; ++x)
            {
                for (int y = 0; y < size; ++y)
                {
                    if (sample(x, y))
                    {
                    	if (len == -1)
                    	{
                    	    from = y;
                    	    len = 1;
                    	}
                    	else
                    	    ++len;
                    }
                    else
                    {
                        if (len != -1)
                        {
                            addRect(x, from, len);
                            len = -1;
                        }
                    }
                    //process(x, y, true);
                    //process(x, y, false);
                }
        	    if (len != -1)
        	    {
        	        addRect(x, from, len);
        	        len = -1;
        	    }
            }

            vb.switchBuffer();
        }
        
        private void addRect(int x, int from, int len)
        {
            vb.addBuffered(new VisRectangle(pixelToWorld(x, from), LinAlg.add(pixelToWorld(x, from), new double[] { metersPerPixel, -metersPerPixel * len }), new VisDataFillStyle(Color.black)));
        }

        private void process(int x, int y, boolean xSide)
        {
            boolean current = sample(x, y);


            if (xSide)
            {
                x = x + 1;
                if (x >= size)
                    return;
            }
            else
            {
                y = y + 1;
                if (y >= size)
                    return;
            }
            boolean other = sample(x, y);

            if (current != other)
            {
                double[] xy1 = pixelToWorld(x, y);

                double[] xy2 = { xy1[0], xy1[1] };
                if (xSide)
                {
                    xy2[1] -= metersPerPixel;
                }
                else
                {
                    xy2[0] += metersPerPixel;
                }

                double[] zz = { 0, 0.8 };

                // vb.addBuffered(new VisBox(xy1[0], xy1[1], 0.4, xy2[0],
                // xy2[1], 0.8, texture));
                vb.addBuffered(new VisVerticalImage(texture, xy1, xy2, zz));
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
