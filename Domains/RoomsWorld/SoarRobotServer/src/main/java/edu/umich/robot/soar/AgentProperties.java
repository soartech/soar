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

import edu.umich.robot.util.AngSpeedUnit;
import edu.umich.robot.util.AngleResolution;
import edu.umich.robot.util.AngleUnit;
import edu.umich.robot.util.ImmutablePose;
import edu.umich.robot.util.LengthUnit;
import edu.umich.robot.util.LinSpeedUnit;
import edu.umich.robot.util.Misc;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.properties.PropertyKey;

/**
 * @author voigtjr@gmail.com
 */
public class AgentProperties
{
    public static PropertyKey<Integer> OBJECT_LINGER_SECONDS = 
        PropertyKey.builder("object-linger-seconds", Integer.class)
        .defaultValue(2)
        .build();

    public static PropertyKey<AngSpeedUnit> ANGULAR_SPEED_UNIT = 
        PropertyKey.builder("angular-speed-unit", AngSpeedUnit.class) 
        .defaultValue(AngSpeedUnit.DEGREES_PER_SEC) 
        .build();
    
    public static final PropertyKey<LengthUnit> LENGTH_UNIT = 
        PropertyKey.builder("length-unit", LengthUnit.class) 
        .defaultValue(LengthUnit.METERS) 
        .build();

    public static final PropertyKey<LinSpeedUnit> LINEAR_SPEED_UNIT = 
        PropertyKey.builder("linear-speed-unit", LinSpeedUnit.class) 
        .defaultValue(LinSpeedUnit.METERS_PER_SEC) 
        .build();

    public static final PropertyKey<AngleUnit> ANGLE_UNIT = 
        PropertyKey.builder("angle-unit", AngleUnit.class) 
        .defaultValue(AngleUnit.DEGREES) 
        .build();

    public static final PropertyKey<AngleResolution> ANGLE_RESOLUTION = 
        PropertyKey.builder("angle-resolution", AngleResolution.class) 
        .defaultValue(AngleResolution.INT) 
        .build();

    public static final PropertyKey<Pose> TRANSLATION = 
        PropertyKey.builder("translation", Pose.class) 
        .defaultValue(ImmutablePose.new2DInstance(0, 0)) 
        .build();

    public static enum LearnSetting 
    { 
        OFF("learn --off"), ON("learn --on"), ONLY("learn --only");
        
        private final String commandLine;
        
        LearnSetting(String commandLine)
        {
            this.commandLine = commandLine;
        }

        public String toCommandLine()
        {
            return commandLine;
        } 
    };
    
    public static final PropertyKey<LearnSetting> LEARN = 
        PropertyKey.builder("learn", LearnSetting.class) 
        .defaultValue(LearnSetting.OFF) 
        .build();

    public static final PropertyKey<Boolean> EPMEM_LEARNING = 
        PropertyKey.builder("epmem-learning", Boolean.class) 
        .defaultValue(Boolean.FALSE) 
        .build();

    public static final PropertyKey<Boolean> SMEM_LEARNING = 
        PropertyKey.builder("smem-learning", Boolean.class) 
        .defaultValue(Boolean.FALSE) 
        .build();

    public static final PropertyKey<String[]> EPMEM_EXCLUSIONS = 
        PropertyKey.builder("epmem-exclusions", String[].class) 
        .defaultValue(new String[] {  "epmem", "smem" }) 
        .build();

    public static final PropertyKey<String> DEFAULT_STORAGE_AREA_ID = 
        PropertyKey.builder("default-storage-area-id", String.class) 
        .defaultValue("8") 
        .build();

    public static final PropertyKey<String> AREAS_HELD_IN = 
        PropertyKey.builder("areas-held-in", String.class) 
        .defaultValue("wm") 
        .build();

    public static final PropertyKey<String> OBJECTS_HELD_IN = 
        PropertyKey.builder("objects-held-in", String.class) 
        .defaultValue("wm") 
        .build();

    public static final PropertyKey<String> LOOK_AHEAD_PLANNING = 
        PropertyKey.builder("look-ahead-planning", String.class) 
        .defaultValue("yes") 
        .build();

    public static final PropertyKey<String> SEARCH_CONTROL_GO_TO_GATEWAY = 
        PropertyKey.builder("search-control-go-to-gateway", String.class) 
        .defaultValue("yes") 
        .build();

    public static final PropertyKey<String> DELETE_OLD_AREAS = 
        PropertyKey.builder("delete-old-areas", String.class) 
        .defaultValue("false") 
        .build();
    
    public static enum Mission 
    {
        CLEAN_ROOMS, RANDOM_WALK, PATROL, TOUCH_OBJECTS;
        
        @Override
        public String toString() 
        {
            return Misc.constantCaseToLowerDashes(this.name());
        }
    }

    public static final PropertyKey<Mission> MISSION = 
        PropertyKey.builder("mission", Mission.class) 
        .defaultValue(Mission.CLEAN_ROOMS) 
        .build();

    public static final PropertyKey<String[]> MISC_COMMANDS = 
        PropertyKey.builder("misc-commands", String[].class) 
        .defaultValue(new String[] { "smem --set timers one", "epmem --set timers one", "waitsnc --disable" }) 
        .build();


}
