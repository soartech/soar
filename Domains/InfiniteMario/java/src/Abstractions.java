package edu.umich;

import sml.FloatElement;
import sml.Identifier;
import sml.IntElement;
import sml.StringElement;


public class Abstractions{
	static class Monster {
		double x;
		double y;
		double sx;
		double sy;
		int type;			
		String typeName;
		boolean winged;
	}
	static class MonsterWME{
		double x;
		double y;
		double sx;
		double sy;
		int type;			
		String typeName;
		boolean winged;
		Identifier monsterWME;
		StringElement monster_type;
		StringElement monster_winged;
		FloatElement monster_xd;
		FloatElement monster_yd;

		//For discretization of space around monsters. uncomment 
		//		FloatElement monster_x;
		//	FloatElement monster_y;
		
	    IntElement monster_x;
		IntElement monster_y;
		
		FloatElement monster_sx;
		FloatElement monster_sy;
		boolean flag;
	}
}
