package edu.umich.soarrobot.SoarRobotTablet.objects;

import javax.microedition.khronos.opengles.GL10;

import android.graphics.Point;
import android.graphics.PointF;
import edu.umich.soarrobot.SoarRobotTablet.layout.GLUtil;

public class SimWallTop {
    Point start;
    Point size;
    PointF scaledStart;
    PointF scaledSize;
    
    public SimWallTop(Point start, Point size) {
        this.start = start;
        this.size = size;
        this.scaledStart = SimArea.scalePoint(start);
        this.scaledSize = SimArea.scalePoint(size);
    }
    
    public void draw(GL10 gl) {
        GLUtil.drawRect(gl, scaledStart.x, scaledStart.y, -1.0f, scaledSize.x, scaledSize.y, GLUtil.LIGHT_GRAY);
    }
}
