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

import java.net.DatagramPacket;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * <pre>
 * struct grrc_feedback_packet {
 * 	uint32_t id;
 * 	uint32_t size;
 * 	datum values[MAX_REQUEST_TAGS];
 * };
 * </pre>
 * 
 * <p>
 * Documentation from README:
 * 
 * <blockquote>Where the content of fields is determined by the tags given in
 * the corresponding request. </blockquote>
 * 
 * <p>
 * Immutable.
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public class GrrcFeedbackPacket
{
    public static GrrcFeedbackPacket valueOf(DatagramPacket packet)
    {
        return new GrrcFeedbackPacket(packet);
    }

    private static final int ID_OFFSET = 0;

    private static final int SIZE_OFFSET = 4;

    private static final int VALUES_OFFSET = 8;

    private final long id;

    private final List<Float> values;

    public GrrcFeedbackPacket(DatagramPacket packet)
    {
        ByteBuffer buffer = ByteBuffer.allocate(packet.getLength());
        buffer.put(packet.getData(), 0, packet.getLength());

        id = Unsigned.getUnsignedInt(buffer, ID_OFFSET);
        int size = (int) Unsigned.getUnsignedInt(buffer, SIZE_OFFSET);
        values = new ArrayList<Float>(size);

        for (int i = 0; i < size; ++i)
        {
            float value = buffer.getFloat(VALUES_OFFSET + (4 * i));
            values.add(Float.valueOf(value));
        }

    }

    public long getId()
    {
        return id;
    }

    public List<Float> getValues()
    {
        return Collections.unmodifiableList(values);
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder("GrrcFeedbackPacket(");
        sb.append(id);
        for (Float value : values)
            sb.append(String.format(",%3.3f", value));
        sb.append(")");
        return sb.toString();
    }
}
