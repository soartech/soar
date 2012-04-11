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

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import sml.Identifier;

import com.google.common.collect.Lists;

import edu.umich.robot.RobotOutput;
import edu.umich.robot.metamap.AreaDescription;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.properties.PropertyManager;
import edu.umich.robot.util.soar.AngSpeedWme;
import edu.umich.robot.util.soar.DistanceWme;
import edu.umich.robot.util.soar.LinSpeedWme;
import edu.umich.robot.util.soar.YawWme;
import edu.umich.soar.FloatWme;
import edu.umich.soar.IntWme;
import edu.umich.soar.StringWme;

/**
 * @author voigtjr@gmail.com
 */
public class SelfIL extends InputLinkElement
{
    private static final Log logger = LogFactory.getLog(SelfIL.class);

    private final RobotOutput output;

    private final IntWme areaId;

    private IntWme carryId;

    private final List<DistanceWme> xyz = Lists.newArrayList();

    private final List<LinSpeedWme> xyzVelocity = Lists.newArrayList();

    private YawWme yaw;

    private AngSpeedWme yawVelocity;
    
    private final StringWme headlight;
    
    private final FloatWme battery;
    
    private final PropertyManager properties;

    SelfIL(SoarAgent agent)
    {
        super(agent, IOConstants.SELF, agent.getSoarAgent().GetInputLink());
        this.output = agent.getRobotOutput();
        properties = agent.getProperties();

        areaId = IntWme.newInstance(getRoot(), IOConstants.AREA);
        headlight = StringWme.newInstance(getRoot(), IOConstants.HEADLIGHT);
        battery = FloatWme.newInstance(getRoot(), IOConstants.BATTERY);

        Identifier pose = getRoot().CreateIdWME(IOConstants.POSE);
        xyz.add(DistanceWme.newInstance(pose, IOConstants.X, properties));
        xyz.add(DistanceWme.newInstance(pose, IOConstants.Y, properties));
        xyz.add(DistanceWme.newInstance(pose, IOConstants.Z, properties));
        yaw = YawWme.newInstance(pose, IOConstants.YAW, properties);
        xyzVelocity.add(LinSpeedWme.newInstance(pose, IOConstants.X_VELOCITY, properties));
        xyzVelocity.add(LinSpeedWme.newInstance(pose, IOConstants.Y_VELOCITY, properties));
        xyzVelocity.add(LinSpeedWme.newInstance(pose, IOConstants.Z_VELOCITY, properties));
        yawVelocity = AngSpeedWme.newInstance(pose, IOConstants.YAW_VELOCITY, properties);
        
        update();
    }

    @Override
    public void update()
    {
        if (output.getCarriedObject() == null)
        {
            if (carryId != null)
            {
                carryId.destroy();
                carryId = null;
            }
        }
        else
        {
            if (carryId == null)
                carryId = IntWme.newInstance(getRoot(), IOConstants.CARRY);
            
            carryId.update(output.getCarriedObject().getId());
            logger.trace(carryId);
        }

        AreaDescription a = output.getAreaDescription();
        areaId.update((a != null) ? a.getId() : -1);
        headlight.update(output.isHeadlightOn() ? IOConstants.ON : IOConstants.OFF);
        battery.update(output.getBatteryLife() * 100);
        logger.trace(areaId);

        Pose p = output.getPose();
        p.translate(properties.get(AgentProperties.TRANSLATION));
        for (int i = 0; i < xyz.size(); ++i)
        {
            xyz.get(i).update(p.getPos().get(i));
            logger.trace(xyz.get(i));
            
            xyzVelocity.get(i).update(p.getVel().get(i));
            logger.trace(xyzVelocity.get(i));
        }
        
        yaw.update(p.getYaw());
        logger.trace(yaw);
        
        yawVelocity.update(p.getRotationRate().get(2));
        logger.trace(yawVelocity);
    }

}
