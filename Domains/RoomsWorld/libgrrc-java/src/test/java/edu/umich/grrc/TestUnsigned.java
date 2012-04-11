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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

import java.nio.ByteBuffer;

import org.junit.Test;
import org.junit.Before;

import edu.umich.grrc.Unsigned;

/**
 * Some really simple tests for Unsigned class.
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 *
 */
public class TestUnsigned
{
    private ByteBuffer buffer;

    @Before
    public void before()
    {
        buffer = ByteBuffer.allocate(16);
    }

    @Test
    public void testIsUnsignedInt()
    {
        for (long i = 0; i <= 0xFFFFFFFFL; ++i)
        {
            assertTrue(Unsigned.isUnsignedInt(i));
        }

        assertFalse(Unsigned.isUnsignedInt(-1));
        assertFalse(Unsigned.isUnsignedInt(0x1FFFFFFFFL));
    }

    @Test
    public void testUnsignedInt()
    {
        Unsigned.putUnsignedInt(buffer, 0);
        Unsigned.putUnsignedInt(buffer, 0xFFFFFFFFL);
        Unsigned.putUnsignedInt(buffer, 20);
        Unsigned.putUnsignedInt(buffer, 4000);

        buffer.flip();

        assertEquals(Unsigned.getUnsignedInt(buffer), 0);
        assertEquals(Unsigned.getUnsignedInt(buffer), 0xFFFFFFFFL);
        assertEquals(Unsigned.getUnsignedInt(buffer), 20);
        assertEquals(Unsigned.getUnsignedInt(buffer), 4000);
    }

    @Test
    public void testUnsignedIntPosition()
    {
        Unsigned.putUnsignedInt(buffer, 0, 12);
        Unsigned.putUnsignedInt(buffer, 0xFFFFFFFFL, 8);
        Unsigned.putUnsignedInt(buffer, 20, 4);
        Unsigned.putUnsignedInt(buffer, 4000, 0);

        assertEquals(Unsigned.getUnsignedInt(buffer, 12), 0);
        assertEquals(Unsigned.getUnsignedInt(buffer, 8), 0xFFFFFFFFL);
        assertEquals(Unsigned.getUnsignedInt(buffer, 4), 20);
        assertEquals(Unsigned.getUnsignedInt(buffer, 0), 4000);
    }
}
