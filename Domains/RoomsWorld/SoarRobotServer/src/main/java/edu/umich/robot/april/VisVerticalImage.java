/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.umich.robot.april;

import java.awt.Color;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;

import april.vis.VisContext;
import april.vis.VisObject;
import april.vis.VisTexture;
import april.vis.VisUtil;

/**
 * 
 * @author miller
 */
public class VisVerticalImage implements VisObject {

    VisTexture texture;
    double[] xy1, xy2, zz;
    double repeatWidth = 1;
    double repeatHeight = 1;


    /**
     *
     * @param texture
     * @param xy1 x/y coords for first edge of image.
     * @param xy2 x/y coords for second edge of image.
     * @param zz lower and upper z values of image.
     */
    public VisVerticalImage(VisTexture texture, double[] xy1, double[] xy2, double[] zz) {
        this.texture = texture;
        texture.lock();
        this.xy1 = xy1;
        this.xy2 = xy2;
        this.zz = zz;
    }

    public void setRepeat(double repeat) {
        repeatWidth = repeat;
        repeatHeight = repeat;
    }

    @Override
    public void render(VisContext vc, GL gl, GLU glu) {
        VisUtil.setColor(gl, Color.white);

        VisUtil.pushGLWholeState(gl);

        double width = texture.getWidth() * repeatWidth;
        double height = texture.getHeight() * repeatHeight;

        texture.bindTexture(gl);

        //gl.glEnable(gl.GL_BLEND);
        //gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA);

        gl.glBegin(gl.GL_QUADS);
        gl.glTexCoord2d(0, height);
        gl.glVertex3d(xy1[0], xy1[1], zz[0]);
        gl.glTexCoord2d(width, height);
        gl.glVertex3d(xy2[0], xy2[1], zz[0]);
        gl.glTexCoord2d(width, 0);
        gl.glVertex3d(xy2[0], xy2[1], zz[1]);
        gl.glTexCoord2d(0, 0);
        gl.glVertex3d(xy1[0], xy1[1], zz[1]);
        gl.glEnd();

        texture.unbindTexture(gl);
        VisUtil.popGLWholeState(gl);
    }

}
