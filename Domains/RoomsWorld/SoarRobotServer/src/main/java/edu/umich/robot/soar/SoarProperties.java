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

import java.io.File;

import edu.umich.robot.soar.SoarDataCollector.DataCollectionMode;
import edu.umich.robot.util.properties.PropertyKey;

/**
 * @author voigtjr@gmail.com
 */
public class SoarProperties
{
    public static PropertyKey<Boolean> SPAWN_DEBUGGERS = 
        PropertyKey.builder("spawn-debuggers", Boolean.class)
        .defaultValue(true)
        .build();

    public static PropertyKey<DataCollectionMode> DATA_COLLECTION_MODE = 
        PropertyKey.builder("data-collection-mode", DataCollectionMode.class)
        .defaultValue(DataCollectionMode.DECISION_CYCLES)
        .build();

    public static PropertyKey<Integer> PERIOD_CYCLES = 
        PropertyKey.builder("period-cycles", Integer.class)
        .defaultValue(5000)
        .build();

    public static PropertyKey<Integer> PERIOD_MILLIS = 
        PropertyKey.builder("period-millis", Integer.class)
        .defaultValue(15000)
        .build();

    public static PropertyKey<File> DATA_FILE = 
        PropertyKey.builder("data-file", File.class)
        .defaultValue(null)
        .build();

}
