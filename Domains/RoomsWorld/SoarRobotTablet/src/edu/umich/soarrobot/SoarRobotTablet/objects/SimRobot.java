package edu.umich.soarrobot.SoarRobotTablet.objects;

import java.util.HashMap;

import javax.microedition.khronos.opengles.GL10;

import android.graphics.PointF;
import april.lcmtypes.laser_t;
import edu.umich.robot.lcmtypes.waypoint_list_t;
import edu.umich.robot.lcmtypes.waypoint_t;
import edu.umich.soarrobot.SoarRobotTablet.layout.GLUtil;

public class SimRobot extends SimObject
{
    protected SimObject carrying;
   
    
    private laser_t lidar;
    private float lidarTheta;
    private PointF lidarLocation;

    private laser_t lowresLidar;
    private float lowresLidarTheta;
    private PointF lowresLidarLocation;
    
    private HashMap<String, SimRobot> robots;
    
    private waypoint_list_t waypoints;
    private int follow; // Which robot to follow

    public SimRobot(String type, int id, PointF location)
    {
        super(type, id, location);
        this.carrying = null;
        lidar = null;
        lowresLidar = null;
        waypoints = null;
        robots = new HashMap<String, SimRobot>();
    }
    
    public void draw(GL10 gl, boolean drawRed, boolean drawBlue, boolean drawYellow) {
        if (!visible) {
            return;
        }
        if (drawRed) {
            drawLidar(lidar, lidarLocation, lidarTheta, gl, GLUtil.RED);
        }
        if (drawBlue) {
            drawLidar(lowresLidar, lowresLidarLocation, lowresLidarTheta, gl, GLUtil.BLUE);
        }
        if (drawYellow) {
            drawWaypoints(gl, GLUtil.YELLOW);
        }
        drawRobot(gl);
    }

    private static void drawLidar(laser_t lidar, PointF lidarLocation, float lidarTheta, GL10 gl, String color) {
        if (lidar == null) {
            return;
        }
        //c.save();
        gl.glMatrixMode(GL10.GL_MODELVIEW);
        gl.glPushMatrix();
        //c.translate(lidarLocation.x, lidarLocation.y);
        //c.rotate(-lidarTheta);
        gl.glTranslatef(lidarLocation.x, lidarLocation.y, 0.0f);
        gl.glRotatef(lidarTheta, 0.0f, 0.0f, 1.0f);
        //p.setStyle(Style.FILL);
        for (int i = 0; i < lidar.nranges; ++i) {
            float range = lidar.ranges[i];
            float angle = lidar.rad0 + lidar.radstep * i;
            float dx = (float)Math.cos(angle) * range;
            float dy = (float)Math.sin(angle) * range;
            GLUtil.drawCube(gl, -0.05f + dx, -0.05f + dy, -0.5f, 0.1f, 0.1f, 0.1f, color);
            /*
            c.save();
            c.translate(dx, dy);
            c.drawCircle(0.0f, 0.0f, 0.1f, p);
            c.restore();
            */
        }
        gl.glPopMatrix();
        //c.restore();
    }
    
    private void drawRobot(GL10 gl) {
        GLUtil.drawCube(gl, location.x, location.y, -0.5f, size.x, size.y, 0.3f, GLUtil.BLUE, theta); // Body
        GLUtil.drawCube(gl, location.x, location.y, -0.75f, size.x * 0.4f, size.y * 0.4f, 0.125f, GLUtil.LIGHT_GRAY, theta); // Head
        GLUtil.drawWheel(gl, 
                location.x + 0.4f * (size.x) * (float) Math.cos(Math.toRadians(theta) - Math.PI/4), 
                location.y + 0.6f * (size.y) * (float) Math.sin(Math.toRadians(theta) - Math.PI/4),
                -0.3f, 
                0.15f, 
                0.15f, 
                0.15f, 
                GLUtil.BLACK, 
                theta);
        GLUtil.drawWheel(gl, 
                location.x - 0.4f * (size.x) * (float) Math.cos(Math.toRadians(-theta) - Math.PI/4), 
                location.y + 0.6f * (size.y) * (float) Math.sin(Math.toRadians(-theta) - Math.PI/4),
                -0.3f, 
                0.15f, 
                0.15f, 
                0.15f, 
                GLUtil.BLACK, 
                theta);      
        GLUtil.drawWheel(gl, 
                location.x - 0.4f * (size.x + 0.22f) * (float) Math.cos(Math.toRadians(theta) - Math.PI/3), 
                location.y - 0.6f * (size.y + 0.22f) * (float) Math.sin(Math.toRadians(theta) - Math.PI/3),
                -0.3f, 
                0.15f, 
                0.15f, 
                0.15f, 
                GLUtil.BLACK, 
                theta);
        GLUtil.drawWheel(gl, 
                location.x + 0.4f * (size.x + 0.22f) * (float) Math.cos(Math.toRadians(-theta) - Math.PI/3), 
                location.y - 0.6f * (size.y + 0.22f) * (float) Math.sin(Math.toRadians(-theta) - Math.PI/3),
                -0.3f, 
                0.15f, 
                0.15f, 
                0.15f, 
                GLUtil.BLACK, 
                theta);     
    }
    
    private void drawWaypoints(GL10 gl, String color) {
        if (waypoints == null) {
            return;
        }
        for (waypoint_t w : waypoints.waypoints) {
            GLUtil.drawCube(gl, (float) w.xLocal - 0.05f, (float) w.yLocal - 0.05f, -0.5f, 0.1f, 0.1f, 0.1f, color);
            /*
            c.save();
            c.translate((float)w.xLocal, (float)-w.yLocal);
            c.drawCircle(0.0f, 0.0f, 0.1f, p);
            c.restore();
            */
        }
    }
    
    public void setLidar(laser_t lidar)
    {
       this.lidar = lidar;
       lidarTheta = theta;
       lidarLocation = location;
    }
    
    public void setLowresLidar(laser_t lidar)
    {
       this.lowresLidar = lidar;
       lowresLidarTheta = theta;
       lowresLidarLocation = location;
    }
    
    public void setWaypoints(waypoint_list_t w)
    {
        waypoints = w;
    }
    
    public void setCarrying(SimObject carrying) {
        this.carrying = carrying;
    }
    
    public SimObject getCarrying() {
        return carrying;
    }
}
