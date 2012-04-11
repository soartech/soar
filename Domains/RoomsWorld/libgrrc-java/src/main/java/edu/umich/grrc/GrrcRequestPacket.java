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

import java.net.InetAddress;

/**
 * <p>
 * This class represents the format_2_request packet as defined in grrc-com.h:
 * 
 * <pre>
 * struct {
 *         float packet_type;
 *         uint32_t id;
 *         uint32_t size;
 *         grrc_feedback_tag fields[MAX_REQUEST_TAGS];
 * } format_2_request;
 * </pre>
 * 
 * <p>
 * Documentation from README:
 * 
 * <blockquote> This packet asks for a single feedback packet with the requested
 * fields. </blockquote>
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public class GrrcRequestPacket extends GrrcRequestBase
{
    private static final float PACKET_TYPE = 2.0f;

    public GrrcRequestPacket(long id, InetAddress address, int port)
    {
        super(id, address, port);
    }

    @Override
    protected float getPacketType()
    {
        return PACKET_TYPE;
    }

    @Override
    public String toString()
    {
        return "GrrcRequestPacket(" + super.toString() + ")";
    }
}
