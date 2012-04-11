package edu.umich.soarrobot.SoarRobotTablet.objects;

import javax.microedition.khronos.opengles.GL10;

import edu.umich.soarrobot.SoarRobotTablet.layout.GLUtil;

import android.graphics.Point;
import android.graphics.PointF;

public class SimWall {
	Point start;
	Point end;
	PointF scaledStart;
	PointF scaledEnd;

	public SimWall(Point start, Point end) {
		this.start = start;
		this.end = end;
		this.scaledStart = SimArea.scalePoint(start);
		this.scaledEnd = SimArea.scalePoint(end);		
	}

	public void draw(GL10 gl) {
		GLUtil.drawWall(gl, scaledStart, scaledEnd, 1.0f, GLUtil.GRAY);
	}
}