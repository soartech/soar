package april.vis;

import java.util.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.util.*;

/** Contains the VisObjects in a world. **/
public class VisWorld
{
    ArrayList<TemporaryObject> temporaryObjects = new ArrayList<TemporaryObject>();
    ArrayList<VisLight> lights = new ArrayList<VisLight>();

    HashMap<String, Buffer> bufferMap = new HashMap<String, Buffer>();
    ArrayList<Buffer> buffers = new ArrayList<Buffer>();

    ArrayList<VisWorldListener> listeners = new ArrayList<VisWorldListener>();
    Timer timer = new Timer();

    static final String UNBUFFERED = "__UNBUFFERED";
    static final String DEFAULT = "__DEFAULT";

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
        private ArrayList<VisObject> back  = new ArrayList<VisObject>();
        private ArrayList<VisObject> front = new ArrayList<VisObject>();
        // we will generally want to ensure that other buffers draw on
        // top; consequently, make the default that we draw very
        // early.
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
            notifyListeners();
        }

        public void addFront(VisObject vo)
        {
            synchronized(buffers) {
                front.add(vo);
                notifyListeners();
            }
        }

        public void  switchBuffer()
        {
            synchronized (buffers) {
                front = back;
                // don't recycle: a previous front buffer
                // could still have a reference somewhere.
                back = new ArrayList<VisObject>();

                notifyListeners();
            }
        }

        ArrayList<VisObject> getFront()
        {
            return front;
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

    public synchronized void clear()
    {
        synchronized(temporaryObjects) {
            temporaryObjects.clear();
        }
        synchronized(buffers) {
            bufferMap.clear();
            buffers.clear();
        }
        notifyListeners();
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

    protected void notifyListeners()
    {
        for (VisWorldListener listener : listeners)
            listener.worldChanged(this);
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
            notifyListeners();
        }

        return b;
    }

    public void addBuffered(String name, VisObject vo)
    {
        Buffer b = getBuffer(name);
        b.addBuffered(vo);
    }

    public void switchBuffer(String name)
    {
        Buffer b = getBuffer(name);
        b.switchBuffer();
    }

    public void addBuffered(VisObject vo)
    {
        addBuffered(DEFAULT, vo);
    }

    public void switchBuffer()
    {
        switchBuffer(DEFAULT);
    }

    public void addListener(VisWorldListener l)
    {
        listeners.add(l);
    }

    public void removeListener(VisWorldListener l)
    {
        if (listeners.contains(l))
            listeners.remove(l);
    }

    private void renderPreserveState(VisContext vc, GL gl, GLU glu, VisObject vo)
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
    void render(VisContext vc, GL gl, GLU glu)
    {
        synchronized(buffers) {
            for (Buffer b : buffers) {

                if (!vc.getViewManager().isBufferEnabled(b.name))
                    continue;

                ArrayList<VisObject> obs = b.getFront();

                for (VisObject vo : obs) {
                    if (vo != null)
                        renderPreserveState(vc, gl, glu, vo);
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
                    renderPreserveState(vc, gl, glu, to.vo);

                    min_mtime = Math.min(min_mtime, to.expire_mtime);
                }
            }
        }

        if (min_mtime < Long.MAX_VALUE)
            timer.schedule(new RedrawTask(vc), min_mtime - mtime);
    }

    class RedrawTask extends TimerTask
    {
        VisContext vc;

        RedrawTask(VisContext vc)
        {
            this.vc = vc;
        }

        public void run()
        {
            vc.draw();
        }
    }
}
