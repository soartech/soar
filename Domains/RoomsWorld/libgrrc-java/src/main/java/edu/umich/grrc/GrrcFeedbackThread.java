/*
 * Copyright (c) 2011, Regents of the University of Michigan
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package edu.umich.grrc;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * <p>
 * Starts a thread and listens for GrrcFeedbackPackets. Interested parties
 * should register GrrcFeedbackHandler objects.
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public class GrrcFeedbackThread
{
    private final ExecutorService exec = Executors.newSingleThreadExecutor();

    private final DatagramSocket socket;

    private final Future<?> task;

    private final int BUFFER_SIZE = 8192;

    private final List<GrrcFeedbackHandler> handlers = new ArrayList<GrrcFeedbackHandler>();

    public GrrcFeedbackThread(DatagramSocket socket)
    {
        this.socket = socket;
        task = exec.submit(listener);
    }

    private final Runnable listener = new Runnable()
    {
        public void run()
        {
            DatagramPacket packet;
            ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);
            try
            {
                while (!Thread.interrupted())
                {
                    packet = new DatagramPacket(buffer.array(), buffer
                            .capacity());
                    socket.receive(packet);

                    if (!handlers.isEmpty())
                    {
                        // GrrcFeedbackPacket is immutable
                        GrrcFeedbackPacket fbp = GrrcFeedbackPacket
                                .valueOf(packet);
                        // System.out.println(fbp);
                        for (GrrcFeedbackHandler handler : handlers)
                        {
                            handler.packetReceived(fbp);
                        }
                    }
                }
            }
            catch (IOException e)
            {
                if (!Thread.interrupted())
                    e.printStackTrace();
            }
        }
    };

    public void addHandler(GrrcFeedbackHandler handler)
    {
        handlers.add(handler);
    }

    public void removeHandler(GrrcFeedbackHandler handler)
    {
        handlers.remove(handler);
    }

    public boolean isRunning()
    {
        return !task.isDone();
    }

    public void shutdown()
    {
        task.cancel(true);
        exec.shutdown();
    }
}
