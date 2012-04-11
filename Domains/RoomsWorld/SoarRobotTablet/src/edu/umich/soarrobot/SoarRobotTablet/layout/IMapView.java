package edu.umich.soarrobot.SoarRobotTablet.layout;

import edu.umich.soarrobot.SoarRobotTablet.SoarRobotTablet;
import edu.umich.soarrobot.SoarRobotTablet.objects.SimObject;
import edu.umich.soarrobot.SoarRobotTablet.objects.SimRobot;

public interface IMapView {

	void setNextClass(CharSequence charSequence);

	void zoomOut();
	void zoomIn();

	void setActivity(SoarRobotTablet soarRobotTablet);

	void draw();

	void addObject(SimObject sim);
	void addRobot(SimRobot robot);
	void pickUpObject(String robotName, int id);

	void deserializeMap(String substring);

	SimRobot getRobot(String string);

	void dropObject(String name);

	SimObject getSimObject(int id);

	void doorClose(int id);

	void doorOpen(int id);

	void roomLight(int id, boolean on);

}
