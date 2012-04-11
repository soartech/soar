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

/**
 * <p>
 * Copy paste from grrc-com.h and then auto-formatted. Careful if 0xFFFFFFFF is
 * ever indicated, might need to add leading zeroes to avoid sign-extension.
 * 
 * @author Jonathan Voigt <voigtjr@gmail.com>
 * 
 */
public enum GrrcFeedbackTag
{
    GRRC_FB_VERSION(0x00001), // Version number
    GRRC_FB_SEQ(0x00002), // Global sequence number
    GRRC_FB_TIME(0x00003), // Time packet was sent (milliseconds)
    GRRC_FB_BATTERY_PERCENTAGE(0x00004), // Current battery percentage
    GRRC_FB_SEQ_PACKET(0x00005), // Packet-specific sequence number

    GRRC_FB_ODOM_X(0x00010), // X position of robot), per odometry (meters)
    GRRC_FB_ODOM_Y(0x00011), // Y position of robot), per odometry (meters)
    GRRC_FB_THETA(0x00012), // Heading of robot (radians)
    GRRC_FB_VELOCITY(0x00013), // Velocity of robot (meters per second)
    GRRC_FB_TURNRATE(0x00014), // Rate of turn of robot (radians per second)
    GRRC_FB_BRAKE(0x00015), // State of brake of robot
    GRRC_FB_FLIPPER_RATE(0x00016), // Rate of turn of flipper of robot (radians
    // per second)
    GRRC_FB_FLIPPER_ANGLE(0x00017), // Angle of flipper of robot (radians)
    GRRC_FB_FLIPPER_TORQUE(0x00018), // Torque of flipper of robot (Newton
    // meters)

    GRRC_FB_COMMANDED_VELOCITY(0x00023), // Currently commanded velocity of
    // robot (furlongs per fortnight)
    GRRC_FB_COMMANDED_TURNRATE(0x00024), // Currently commanded rate of turn of
    // robot (radians per second)
    GRRC_FB_COMMANDED_BRAKE(0x00025), // Currently commanded state of brake of
    // robot
    GRRC_FB_COMMANDED_FLIPPER_RATE(0x00026), // Currently commanded rate of turn
    // of flipper of robot (radians per
    // second)

    GRRC_FB_BATTERY_VOLTAGE0(0x00030), // Battery Voltage
    GRRC_FB_BATTERY_VOLTAGE1(0x00031), // Battery Voltage
    GRRC_FB_BATTERY_VOLTAGE2(0x00032), // Battery Voltage
    GRRC_FB_BATTERY_VOLTAGE3(0x00033), // Battery Voltage
    GRRC_FB_BATTERY_VOLTAGE4(0x00034), // Battery Voltage
    GRRC_FB_BATTERY_VOLTAGE5(0x00035), // Battery Voltage
    GRRC_FB_BATTERY_VOLTAGE6(0x00036), // Battery Voltage
    GRRC_FB_BATTERY_VOLTAGE7(0x00037), // Battery Voltage

    GRRC_FB_TEMPERATURE0(0x00040), // Robot Temperature (degrees Celsius)
    GRRC_FB_TEMPERATURE1(0x00041), // Robot Temperature (degrees Celsius)
    GRRC_FB_TEMPERATURE2(0x00042), // Robot Temperature (degrees Celsius)
    GRRC_FB_TEMPERATURE3(0x00043), // Robot Temperature (degrees Celsius)
    GRRC_FB_TEMPERATURE4(0x00044), // Robot Temperature (degrees Celsius)
    GRRC_FB_TEMPERATURE5(0x00045), // Robot Temperature (degrees Celsius)
    GRRC_FB_TEMPERATURE6(0x00046), // Robot Temperature (degrees Celsius)
    GRRC_FB_TEMPERATURE7(0x00047), // Robot Temperature (degrees Celsius)

    GRRC_FB_MOTOR_CURRENT0(0x00050), // Robot Motor Current (amps)
    GRRC_FB_MOTOR_CURRENT1(0x00051), // Robot Motor Current (amps)
    GRRC_FB_MOTOR_CURRENT2(0x00052), // Robot Motor Current (amps)
    GRRC_FB_MOTOR_CURRENT3(0x00053), // Robot Motor Current (amps)
    GRRC_FB_MOTOR_CURRENT4(0x00054), // Robot Motor Current (amps)
    GRRC_FB_MOTOR_CURRENT5(0x00055), // Robot Motor Current (amps)
    GRRC_FB_MOTOR_CURRENT6(0x00056), // Robot Motor Current (amps)
    GRRC_FB_MOTOR_CURRENT7(0x00057), // Robot Motor Current (amps)

    GRRC_FB_ENCODER0(0x00060), // Robot Motor Encoders (travelled distance in
    // meters)
    GRRC_FB_ENCODER1(0x00061), // Robot Motor Encoders (travelled distance in
    // meters)
    GRRC_FB_ENCODER2(0x00062), // Robot Motor Encoders (travelled distance in
    // meters)
    GRRC_FB_ENCODER3(0x00063), // Robot Motor Encoders (travelled distance in
    // meters)
    GRRC_FB_ENCODER4(0x00064), // Robot Motor Encoders (travelled distance in
    // meters)
    GRRC_FB_ENCODER5(0x00065), // Robot Motor Encoders (travelled distance in
    // meters)
    GRRC_FB_ENCODER6(0x00066), // Robot Motor Encoders (travelled distance in
    // meters)
    GRRC_FB_ENCODER7(0x00067), // Robot Motor Encoders (travelled distance in
    // meters)

    GRRC_GEOM_ROBOT_ID(0x00100), // Robot identifier
    GRRC_GEOM_TRACK_SPACING(0x00101), // Robot track spacing (meters)
    GRRC_GEOM_PALLET_HEIGHT(0x00102), // Robot pallet height (meters)
    GRRC_GEOM_MAX_HEIGHT(0x00103), // Maximum height of robot (meters)
    GRRC_GEOM_MAX_DIMENSION(0x00104), // Maximum dimension of robot (meters)

    ;

    private final long id;

    private GrrcFeedbackTag(long id)
    {
        if (!Unsigned.isUnsignedInt(id))
            throw new IllegalArgumentException("id not unsigned int");
        this.id = id;
    }

    public long getId()
    {
        return id;
    }
}
