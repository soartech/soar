/*
 * Note from Brian Tanner.
 * I stole this code from here:
 * http://www.di.unipi.it/~tini/labalg/Slides/ConsoleReader.java
 * 
 * It was the first hit for "ConsoleReader" and I wanted to be done with this
 * in a hurry.
 * 
 * Thanks for putting your code up.  There was no license stated.  I'm sure it's
 * fine ;)
 */
package org.rlcommunity.rlglue.codec.installer;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;

/** 
A class to read strings and numbers from an input stream.
This class is suitable for beginning Java programmers.
It constructs the necessary buffered reader, 
handles I/O exceptions, and converts strings to numbers.
 */
public class ConsoleReader {
//To make non-instantiable
    private ConsoleReader(){
        
    }
    static {
        reader = new BufferedReader(new InputStreamReader(System.in));
    }

    /**
    Reads a line of input and converts it into an integer.
    The input line must contain nothing but an integer.
    Not even added white space is allowed.
    @return the integer that the user typed
     */
    public static int readInt() {
        String inputString = readLine();

        int n = Integer.parseInt(inputString);
        return n;
    }

    /**
    Reads a line of input and converts it into a floating-
    point number. The input line must contain nothing but 
    a nunber. Not even added white space is allowed.
    @return the number that the user typed
     */
    public static double readDouble() {
        String inputString = readLine();
        double x = Double.parseDouble(inputString);
        return x;
    }

    /**
    Reads a line of input. In the (unlikely) event
    of an IOException, the program terminates. 
    @return the line of input that the user typed, null
    at the end of input
     */
    public static String readLine() {
        String inputLine = "";

        try {
            inputLine = reader.readLine();
        } catch (IOException e) {
            System.out.println(e);
           System.exit(1);
        }

        return inputLine;
    }
    private static BufferedReader reader;
}
