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
import java.net.DatagramSocket;
import java.net.InetAddress;

import org.junit.Test;

/**
 * Simple tests.
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 *
 */
public class TestGrrcDrivePacket
{

    @Test
    public void testSimpleSend() throws IOException
    {
        InetAddress ia = InetAddress.getByName("localhost");
        int port = 15432;

        GrrcPacket gp = new GrrcDrivePacket(ia, port).setBrake(1.0f)
                .setSpeedMps(2.0f).setTurnRateRps(3.0f);

        DatagramSocket s = new DatagramSocket();
        s.send(gp.getPacket());
    }
}
