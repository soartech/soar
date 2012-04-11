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
package edu.umich.robot.soar;

import sml.Identifier;

/**
 * Utility for parsing parameters off of Soar wmes.
 * 
 * @author voigtjr@gmail.com
 */
public class CommandParameters
{
    public static int requireInteger(Identifier id, String attribute)
            throws SoarCommandError
    {
        String idString = id.GetParameterValue(attribute);
        try
        {
            return Integer.parseInt(idString);
        }
        catch (NullPointerException e)
        {
            throw new SoarCommandError("No " + attribute + " on command");
        }
        catch (NumberFormatException e)
        {
            throw new SoarCommandError("Error parsing integer " + attribute
                    + ": " + idString);
        }
    }

    public static long requireLong(Identifier id, String attribute)
            throws SoarCommandError
    {
        String idString = id.GetParameterValue(attribute);
        try
        {
            return Long.parseLong(idString);
        }
        catch (NullPointerException e)
        {
            throw new SoarCommandError("No " + attribute + " on command");
        }
        catch (NumberFormatException e)
        {
            throw new SoarCommandError("Error parsing integer " + attribute
                    + ": " + idString);
        }
    }

    public static double requireDouble(Identifier id, String attribute)
            throws SoarCommandError
    {
        String idString = id.GetParameterValue(attribute);
        try
        {
            return Double.parseDouble(idString);
        }
        catch (NullPointerException e)
        {
            throw new SoarCommandError("No " + attribute + " on command");
        }
        catch (NumberFormatException e)
        {
            throw new SoarCommandError("Error parsing double " + attribute
                    + ": " + idString);
        }
    }

    public static String requireString(Identifier id, String attribute)
            throws SoarCommandError
    {
        String idString = id.GetParameterValue(attribute);
        if (idString == null)
            throw new SoarCommandError("No " + attribute + " on command");
        return idString;
    }
}
