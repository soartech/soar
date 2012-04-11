package edu.umich.soarrobot.SoarRobotTablet.layout;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.HashMap;

import javax.microedition.khronos.opengles.GL10;

import android.graphics.Color;
import android.graphics.Point;
import android.graphics.PointF;

public class GLUtil {
	
    private final static int CYLINDER_RESOLUTION = 8;
    
	// X = left / right
	// Y = up / down
	// Z = forward / backward
	
	/*
	 *        6----------7
	 *       /|          |
	 *      / |          |
	 *     2  |      (3) |
	 *     |  |          |
	 *     |  4----------5
	 *     | /          /
	 *     |/          /
	 *     0----------1
	 */
	
	private static float[] cubeCoords = {
		0.0f, 0.0f, 0.0f,
		1.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f,
		1.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 1.0f,
		1.0f, 0.0f, 1.0f,
		0.0f, 1.0f, 1.0f,
		1.0f, 1.0f, 1.0f,
	};

	
	private static short[] cubeIndeces = {
		// Back
		0, 2, 1,
		1, 2, 3,
		
		// Front
		4, 5, 6,
		5, 7, 6,
		
		// Left side
		0, 4, 2,
		4, 6, 2,
		
		// Right side
		1, 3, 5,
		5, 3, 7,
		
		// Top side
		2, 6, 3,
		6, 7, 3,
		
		// Bottom side
		0, 1, 4,
		1, 5, 4,
	};
	
	private static float[] cubeNormals = {
		0.0f, 0.0f, 1.0f,
		0.0f, 0.0f, 1.0f,
		0.0f, 0.0f, -1.0f,
		0.0f, 0.0f, -1.0f,
		-1.0f, 0.0f, 0.0f,
		-1.0f, 0.0f, 0.0f,
		1.0f, 0.0f, 0.0f,
		1.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f,
		0.0f, 1.0f, 0.0f,
		0.0f, -1.0f, 0.0f,
		0.0f, -1.0f, 0.0f,
	};
		
	private static short[] rectIndeces = {
		0, 2, 1,
		1, 2, 3,
	};
	
	private static short[] reverseRectIndeces = {
		0, 1, 2,
		1, 3, 2,
	};
	
	private static FloatBuffer cubeCoordsBuffer;	
	private static ShortBuffer cubeIndecesBuffer, rectIndecesBuffer, reverseRectIndicesBuffer;
	private static FloatBuffer cubeNormalsBuffer, rectNormalsBuffer;
	
	// Member variables for cylinder
	private static float[] cylinderCoords = new float[CYLINDER_RESOLUTION * 6 + 6];
	    
	private static short[] cylinderIndeces = new short[CYLINDER_RESOLUTION * 12]; 
	    
	private static float[] cylinderNormals = new float[CYLINDER_RESOLUTION * 12];
	
	private static FloatBuffer cylinderCoordsBuffer;
	private static ShortBuffer cylinderIndecesBuffer;
	private static FloatBuffer cylinderNormalsBuffer;
	
	public static final String RED =        "rgb(1.0f,0.0f,0.0f)";
	public static final String GREEN =      "rgb(0.0f,1.0f,0.0f)";
	public static final String BLUE =       "rgb(0.0f,0.0f,1.0f)";
	public static final String DARK_BLUE =  "rgb(0.0f,0.0f,0.8f)";
	public static final String YELLOW =     "rgb(1.0f,1.0f,0.0f)";
	public static final String WHITE =      "rgb(1.0f,1.0f,1.0f)";
	public static final String BLACK =      "rgb(0.0f,0.0f,0.0f)";
	public static final String GRAY =       "rgb(0.5f,0.5f,0.5f)";
	public static final String LIGHT_GRAY = "rgb(0.8f,0.8f,0.8f)";
	public static final String DARK_GRAY =  "rgb(0.4f,0.4f,0.4f)";
	
	static {
        // float has 4 bytes, coordinate * 4 bytes
        ByteBuffer bb = ByteBuffer.allocateDirect(cubeCoords.length * 4);
        bb.order(ByteOrder.nativeOrder());
        cubeCoordsBuffer = bb.asFloatBuffer();
     
        // short has 2 bytes, indices * 2 bytes
        bb = ByteBuffer.allocateDirect(cubeIndeces.length * 2);
        bb.order(ByteOrder.nativeOrder());
        cubeIndecesBuffer = bb.asShortBuffer();
        
        // short has 2 bytes, indices * 2 bytes
        bb = ByteBuffer.allocateDirect(rectIndeces.length * 2);
        bb.order(ByteOrder.nativeOrder());
        rectIndecesBuffer = bb.asShortBuffer();
        
        bb = ByteBuffer.allocateDirect(reverseRectIndeces.length * 2);
        bb.order(ByteOrder.nativeOrder());
        reverseRectIndicesBuffer = bb.asShortBuffer();
     
        cubeCoordsBuffer.put(cubeCoords);
        cubeIndecesBuffer.put(cubeIndeces);
        rectIndecesBuffer.put(rectIndeces);
        reverseRectIndicesBuffer.put(reverseRectIndeces);
     
        cubeCoordsBuffer.position(0);
        cubeIndecesBuffer.position(0);
        rectIndecesBuffer.position(0);
        reverseRectIndicesBuffer.position(0);
        
        bb = ByteBuffer.allocateDirect(cubeNormals.length * 4);
        bb.order(ByteOrder.nativeOrder());
        cubeNormalsBuffer = bb.asFloatBuffer();
        cubeNormalsBuffer.put(cubeNormals);
        cubeNormalsBuffer.position(0);
        
        // Initializing buffers for cylinder
        for (int i = 0; i < CYLINDER_RESOLUTION + 1; i++) {
            PointF coords = (i == 0) ? new PointF(0.0f, 0.0f) : getCylinderXY(i);
            cylinderCoords[3*i] = coords.x;
            cylinderCoords[3*i + 1] = coords.y;
            cylinderCoords[3*i + 2] = 0.0f;
            
            cylinderCoords[3*i + (CYLINDER_RESOLUTION+1) * 3] = coords.x;
            cylinderCoords[3*i + 1 + (CYLINDER_RESOLUTION+1) * 3] = coords.y;
            cylinderCoords[3*i + 2 + (CYLINDER_RESOLUTION+1) * 3] = 1.0f;
        }
        // Filling in Indices of unit cylinder
        for (int i = 0; i < CYLINDER_RESOLUTION; i++) {
            cylinderIndeces[3*i] = 0;
            cylinderIndeces[3*i+1] = (i == CYLINDER_RESOLUTION - 1) ? 1 : (short) (i+2);
            cylinderIndeces[3*i+2] = (short) (i+1);

            cylinderIndeces[3*i + CYLINDER_RESOLUTION*3] = (short) (i+1);
            cylinderIndeces[3*i + 1 + CYLINDER_RESOLUTION*3] = (i == CYLINDER_RESOLUTION - 1) ? 1 : (short) (i+2);
            cylinderIndeces[3*i + 2 + CYLINDER_RESOLUTION*3] = (short) (i+2+CYLINDER_RESOLUTION);
            
            cylinderIndeces[3*i + CYLINDER_RESOLUTION*6] = (i == CYLINDER_RESOLUTION - 1) ? 1 : (short) (i+2);
            cylinderIndeces[3*i + 1 + CYLINDER_RESOLUTION*6] = (short) ((i == CYLINDER_RESOLUTION - 1) ? (2+CYLINDER_RESOLUTION) : (i+3+CYLINDER_RESOLUTION));
            cylinderIndeces[3*i + 2 + CYLINDER_RESOLUTION*6] = (short) (i+2+CYLINDER_RESOLUTION);
            
            cylinderIndeces[3*i + CYLINDER_RESOLUTION*9] = (short) (CYLINDER_RESOLUTION+1);
            cylinderIndeces[3*i + 1 + CYLINDER_RESOLUTION*9] = (short) (i+2+CYLINDER_RESOLUTION);
            cylinderIndeces[3*i + 2 + CYLINDER_RESOLUTION*9] = (short) ((i == CYLINDER_RESOLUTION - 1) ? (2+CYLINDER_RESOLUTION) : (i+3+CYLINDER_RESOLUTION));
        }
        // Filling in the normal vectors for the unit cylinder
        for (int i = 0; i < CYLINDER_RESOLUTION; i++) {
            //PointF firstPoint = (i == 0) ? new PointF(0.0f, 0.0f) : getCylinderXY(i);
            //PointF secondPoint = (i == CYLINDER_RESOLUTION) ? new PointF(0.0f, 0.0f) : getCylinderXY(i+1);
            PointF firstPoint = getCylinderXY(i+1);
            PointF secondPoint = (i == CYLINDER_RESOLUTION-1) ? getCylinderXY(1) : getCylinderXY(i+2);
            PointF midPoint = new PointF((firstPoint.x + secondPoint.x) / 2.0f, (firstPoint.y + secondPoint.y)/2.0f);
            
            cylinderNormals[3*i] = 0.0f;
            cylinderNormals[3*i + 1] = 0.0f;
            cylinderNormals[3*i + 2] = -1.0f;
            
            cylinderNormals[3*i + CYLINDER_RESOLUTION*3] = midPoint.x;
            cylinderNormals[3*i + 1 + CYLINDER_RESOLUTION*3] = midPoint.y;
            cylinderNormals[3*i + 2 + CYLINDER_RESOLUTION*3] = 0.0f;
            
            cylinderNormals[3*i + CYLINDER_RESOLUTION*6] = midPoint.x;
            cylinderNormals[3*i + 1 + CYLINDER_RESOLUTION*6] = midPoint.y;
            cylinderNormals[3*i + 2 + CYLINDER_RESOLUTION*6] = 0.0f;
            
            cylinderNormals[3*i + CYLINDER_RESOLUTION*9] = 0.0f;
            cylinderNormals[3*i + 1 + CYLINDER_RESOLUTION*9] = 0.0f;
            cylinderNormals[3*i + 2 + CYLINDER_RESOLUTION*9] = 1.0f;
        }
        
        bb = ByteBuffer.allocateDirect(cylinderCoords.length * 4);
        bb.order(ByteOrder.nativeOrder());
        cylinderCoordsBuffer = bb.asFloatBuffer();
        
        bb = ByteBuffer.allocateDirect(cylinderIndeces.length * 2);
        bb.order(ByteOrder.nativeOrder());
        cylinderIndecesBuffer = bb.asShortBuffer();
        
        bb = ByteBuffer.allocateDirect(cylinderNormals.length * 4);
        bb.order(ByteOrder.nativeOrder());
        cylinderNormalsBuffer = bb.asFloatBuffer();
        
        cylinderCoordsBuffer.put(cylinderCoords);
        cylinderIndecesBuffer.put(cylinderIndeces);
        cylinderNormalsBuffer.put(cylinderNormals);
        
        cylinderCoordsBuffer.position(0);
        cylinderIndecesBuffer.position(0);
        cylinderNormalsBuffer.position(0);
	}
	
	public static PointF getCylinderXY(int index) {
	    double degrees = index * (360/CYLINDER_RESOLUTION);
	    PointF point = new PointF((float) Math.cos(Math.toRadians(degrees)), (float) Math.sin(Math.toRadians(degrees)));
	    return point;
	}
	
	private final static HashMap<String, FloatBuffer> colorBufferMap = new HashMap<String, FloatBuffer>();
	
	public static String colorString(float r, float g, float b)
	{
		return "rgb(" + r + "," + g + "," + b + ")";
	}
	
	private static FloatBuffer getColorBuffer(String color)
	{
		if (colorBufferMap.containsKey(color))
		{
			return colorBufferMap.get(color);
		};
		
		int l_paren = color.indexOf('(');
		int r_paren = color.indexOf(')');
		if (!color.startsWith("rgb") || l_paren == -1 || r_paren == -1)
		{
			return null;
		}
		String rgb = color.substring(l_paren + 1, r_paren);
		String[] colors = rgb.split(",");
		if (colors.length != 3)
		{
			return null;
		}
		try {
			float r = Float.parseFloat(colors[0]);
			float g = Float.parseFloat(colors[1]);
			float b = Float.parseFloat(colors[2]);
			FloatBuffer ret = makeColorBuffer(r, g, b);
			colorBufferMap.put(color, ret);
			return ret;
		}
		catch (NumberFormatException e) {
			return null;
		}
	}
	
	private static FloatBuffer makeColorBuffer(float r, float g, float b) {
        float[] colors = new float[8 * (CYLINDER_RESOLUTION + 1)];
        assignColors(colors, r, g, b);
		ByteBuffer cbb = ByteBuffer.allocateDirect(colors.length * 4);
        cbb.order(ByteOrder.nativeOrder());
        FloatBuffer buffer = cbb.asFloatBuffer();
        buffer.put(colors);
        buffer.position(0);
        return buffer;
	}
	
	private static void assignColors(float[] ar, float r, float g, float b) {
		for (int i = 0; i < ar.length / 4; ++i) {
			ar[i * 4] = r;
			ar[i * 4 + 1] = g;
			ar[i * 4 + 2] = b;
			ar[i * 4 + 3] = 1.0f;
		}
	}
	
	/**
	 * Current matrix should be MODELVIEW
	 * @param gl
	 * @param x
	 * @param y
	 * @param r
	 * @param g
	 * @param b
	 */
	public static void drawCube(GL10 gl, float x, float y, float z, float w, float h, float d, String color, float theta)
	{
        gl.glPushMatrix();

        gl.glTranslatef(x, y, z);
        gl.glRotatef(theta, 0.0f, 0.0f, 1.0f);
        gl.glScalef(w, h, d);
        gl.glTranslatef(-0.5f, -0.5f, -0.5f);
        
        gl.glVertexPointer(3, GL10.GL_FLOAT, 0, cubeCoordsBuffer);
        gl.glColorPointer(4, GL10.GL_FLOAT, 0, getColorBuffer(color));
        gl.glNormalPointer(GL10.GL_FLOAT, 0, cubeNormalsBuffer);
        gl.glDrawElements(GL10.GL_TRIANGLES, 12 * 3, GL10.GL_UNSIGNED_SHORT, cubeIndecesBuffer);
        
        gl.glPopMatrix();
	}
	
	public static void drawCube(GL10 gl, float x, float y, float z, float w, float h, float d, String color)
	{
		drawCube(gl, x, y, z, w, h, d, color, 0.0f);
	}

	
	/*************
	 * Rectangle *
	 *************/
	
	public static void drawRect(GL10 gl, float x, float y, float z, float w, float h, String color)
	{        
        gl.glPushMatrix();
        
        gl.glTranslatef(x, y, z);
        gl.glScalef(w, h, 1.0f);

        gl.glVertexPointer(3, GL10.GL_FLOAT, 0, cubeCoordsBuffer);
        gl.glColorPointer(4, GL10.GL_FLOAT, 0, getColorBuffer(color));
        gl.glNormalPointer(GL10.GL_FLOAT, 0, cubeNormalsBuffer);
        gl.glDrawElements(GL10.GL_TRIANGLES, 2 * 3, GL10.GL_UNSIGNED_SHORT, rectIndecesBuffer);
        
        gl.glPopMatrix();
	}
	
	public static void drawRectLidar(GL10 gl, float x, float y, float z, float w, float h, String color)
	{        
	    gl.glPushMatrix();
	        
	    gl.glTranslatef(x, y, z);
	    gl.glScalef(w, h, 1.0f);

	    gl.glVertexPointer(3, GL10.GL_FLOAT, 0, cubeCoordsBuffer);
	    gl.glColorPointer(4, GL10.GL_FLOAT, 0, getColorBuffer(color));
	    gl.glNormalPointer(GL10.GL_FLOAT, 0, cubeNormalsBuffer);
	    gl.glDrawElements(GL10.GL_TRIANGLES, 2 * 3, GL10.GL_UNSIGNED_SHORT, rectIndecesBuffer);
	        
	    gl.glPopMatrix();
	 }
	
	/************
	 * Cylinder *
	 ************/
	
	 public static void drawCylinder(GL10 gl, float x, float y, float z, float w, float h, float d, String color, float theta) {
	     gl.glPushMatrix();
	     
	     gl.glTranslatef(x, y, z);
	     gl.glScalef(w, h, d);
	     gl.glRotatef(theta, 0.0f, 1.0f, 0.0f);
	     
	     gl.glVertexPointer(3, GL10.GL_FLOAT, 0, cylinderCoordsBuffer);
	     gl.glColorPointer(4, GL10.GL_FLOAT, 0, getColorBuffer(color));
	     gl.glNormalPointer(GL10.GL_FLOAT, 0, cylinderNormalsBuffer);
	     gl.glDrawElements(GL10.GL_TRIANGLES, 12 * CYLINDER_RESOLUTION, GL10.GL_UNSIGNED_SHORT, cylinderIndecesBuffer);	    
	     
	     gl.glPopMatrix();
	 }
	 
	 public static void drawWheel(GL10 gl, float x, float y, float z, float w, float h, float d, String color, float theta) {
	         gl.glPushMatrix();
	         
	         gl.glTranslatef(x, y, z);
	         gl.glScalef(w, h, d);
	         gl.glRotatef(90.0f, 1.0f, 0.0f, 0.0f); // To orient the cylinder like a wheel
	         gl.glRotatef(theta, 0.0f, 1.0f, 0.0f);
	         
	         gl.glVertexPointer(3, GL10.GL_FLOAT, 0, cylinderCoordsBuffer);
	         gl.glColorPointer(4, GL10.GL_FLOAT, 0, getColorBuffer(color));
	         gl.glNormalPointer(GL10.GL_FLOAT, 0, cylinderNormalsBuffer);
	         gl.glDrawElements(GL10.GL_TRIANGLES, 12 * CYLINDER_RESOLUTION, GL10.GL_UNSIGNED_SHORT, cylinderIndecesBuffer);     
	         
	         gl.glPopMatrix();
	     }

	 /* * * * * * * * * * * *
	  * Wall related stuff  *
	  * * * * * * * * * * * **/
	 
	public static void drawWall(GL10 gl, PointF start, PointF end, float height, String color) {
		float dx = end.x - start.x;
		float dy = end.y - start.y;
		float theta = (float) Math.toDegrees(Math.atan2(dy, dx));
		float scale = (float) Math.sqrt(dx * dx + dy * dy);
		
		gl.glPushMatrix();
		gl.glTranslatef(start.x, start.y, 0.0f);
		gl.glRotatef(90.0f, -1.0f, 0.0f, 0.0f);
		gl.glRotatef(theta, 0.0f, -1.0f, 0.0f);
		gl.glScalef(scale, 1.0f, 1.0f);
        gl.glVertexPointer(3, GL10.GL_FLOAT, 0, cubeCoordsBuffer);
        gl.glColorPointer(4, GL10.GL_FLOAT, 0, getColorBuffer(color));
        gl.glNormalPointer(GL10.GL_FLOAT, 0, cubeNormalsBuffer);
        gl.glDrawElements(GL10.GL_TRIANGLES, 2 * 3, GL10.GL_UNSIGNED_SHORT, rectIndecesBuffer);
        gl.glDrawElements(GL10.GL_TRIANGLES, 2 * 3, GL10.GL_UNSIGNED_SHORT, reverseRectIndicesBuffer);
        gl.glPopMatrix();
	}
	 
}
