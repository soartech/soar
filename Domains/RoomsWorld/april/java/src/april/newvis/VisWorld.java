package april.newvis;

import java.util.*;
import java.awt.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.util.*;

public class VisWorld
{
    // contents of temporaryObjects, lights, and buffers are protected by
    // synchronzing on their respective lists.
    ArrayList<TemporaryObject> temporaryObjects = new ArrayList<TemporaryObject>();
    ArrayList<VisLight> lights = new ArrayList<VisLight>();
    ArrayList<Buffer> buffers = new ArrayList<Buffer>();

    // bufferMap is protected by synchronizing on 'buffers'.
    HashMap<String, Buffer> bufferMap = new HashMap<String, Buffer>();

    // acquisition order: acquire buffers before acquiring a lock on
    // any individual buffer.
    Timer timer = new Timer();

    static boolean debug = EnvUtil.getProperty("vis.debug", false);

    static class TemporaryObject implements Comparable<TemporaryObject>
    {
        VisObject vo;
        long      expire_mtime;

        TemporaryObject(VisObject vo, long expire_mtime)
	    {
            this.vo = vo;
            this.expire_mtime = expire_mtime;
	    }

        public int compareTo(TemporaryObject b)
        {
            return (int) (expire_mtime - b.expire_mtime);
        }
    }

    public class Buffer implements Comparable<Buffer>
    {
        // contents of 'front' and 'back' are protected by synchronizing on the buffer.
        protected ArrayList<VisObject> back  = new ArrayList<VisObject>();
        protected ArrayList<VisObject> front = new ArrayList<VisObject>();

        int drawOrder = -1;
        String name;

        Buffer(String name)
        {
            this.name = name;
        }

        public synchronized void addBuffered(VisObject vo)
        {
            back.add(vo);
        }

        public synchronized void clear()
        {
            back.clear();
            front.clear();
        }

        public synchronized void addFront(VisObject vo)
        {
            front.add(vo);
        }

        public synchronized void switchBuffer()
        {
            front = back;
            // don't recycle: a previous front buffer
            // could still have a reference somewhere.
            back = new ArrayList<VisObject>();
        }

        public int compareTo(Buffer b)
        {
            return drawOrder - b.drawOrder;
        }

        public void setDrawOrder(int order)
        {
            if (order != this.drawOrder) {
                this.drawOrder = order;
                synchronized(buffers) {
                    Collections.sort(buffers);
                }
            }
        }
    }

    public VisWorld()
    {
        lights.add(new VisLight(new float[] { 100f, 150f, 120f, 1.0f },
                                new float[] { .4f, .4f, .4f, 1.0f},
                                new float[] { .8f, .8f, .8f, 1.0f },
                                new float[] { .5f, .5f, .5f, 1.0f}));


        lights.add(new VisLight(new float[] { -100f, -150f, 120f, 1.0f },
                                new float[] { .1f, .1f, .1f, 1.0f},
                                new float[] { .1f, .1f, .1f, 1.0f },
                                new float[] { .5f, .5f, .5f, 1.0f}));
    }

    public void addTemporary(VisObject vo, double seconds)
    {
        synchronized (temporaryObjects) {
            temporaryObjects.add(new TemporaryObject(vo,
                                                     (long) (System.currentTimeMillis() + seconds * 1000)));
        }
    }

    public void removeTemporary(VisObject vo)
    {
        synchronized (temporaryObjects) {
            TemporaryObject foundto = null;

            for (TemporaryObject to : temporaryObjects) {
                if (to.vo == vo) {
                    foundto = to;
                    break;
                }
            }

            if (foundto != null)
                temporaryObjects.remove(foundto);
        }
    }

    public Buffer getBuffer(String name)
    {
        Buffer b = bufferMap.get(name);
        if (b == null) {
            b = new Buffer(name);
            synchronized(buffers) {
                bufferMap.put(name, b);
                buffers.add(b);
                Collections.sort(buffers);
            }
        }

        return b;
    }

    private void renderObject(VisCanvas vc, GL gl, GLU glu, VisObject vo)
    {
        vo.render(vc, gl, glu);

        if (debug) {
            int err = gl.glGetError();
            if (err != gl.GL_NO_ERROR) {
                System.out.println("GL Error while rendering "+vo+": "+glu.gluErrorString(err));
            }
        }
    }


    // called by VisCanvas (only)
    void render(VisCanvas vc, GL gl, GLU glu)
    {
        synchronized(buffers) {
            for (Buffer b : buffers) {

                synchronized(b) {
//                if (!vc.getViewManager().isBufferEnabled(b.name))
//                    continue;

                    for (VisObject vo : b.front) {
                        if (vo != null)
                            renderObject(vc, gl, glu, vo);
                    }
                }
            }
        }

        // draw temporary objects last (highest priority)
        // remove stale temporary objects
        long mtime = System.currentTimeMillis();

        long min_mtime = Long.MAX_VALUE;

        synchronized (temporaryObjects) {
            for (int i = 0; i < temporaryObjects.size(); i++) {
                TemporaryObject to = temporaryObjects.get(i);

                if (mtime > to.expire_mtime) {
                    // delete this object by shuffling the last element into this position.
                    temporaryObjects.set(i, temporaryObjects.get(temporaryObjects.size()-1));
                    temporaryObjects.remove(temporaryObjects.size()-1);
                    i--;
                } else {
                    renderObject(vc, gl, glu, to.vo);

                    min_mtime = Math.min(min_mtime, to.expire_mtime);
                }
            }
        }

        if (min_mtime < Long.MAX_VALUE)
            timer.schedule(new RedrawTask(vc), min_mtime - mtime);
    }

    // used to force a redraw when temporary items change.
    class RedrawTask extends TimerTask
    {
        VisCanvas vc;

        RedrawTask(VisCanvas vc)
        {
            this.vc = vc;
        }

        public void run()
        {
            vc.draw();
        }
    }
}
