/* Parameter Holder for Passing Parameters Around
* Copyright (C) 2007, Brian Tanner brian@tannerpages.com (http://brian.tannerpages.com/)
* 
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */

//Ok, so Parameter holder could make use of templates, should tell Andrew.

#ifndef ParameterHolder_H
#define ParameterHolder_H

#include <vector>
#include <map>
#include <string>

enum PHTypes{
	intParam=0,
	doubleParam=1,
	boolParam=2,
	stringParam=3
};


class ParameterHolder{
private:
	typedef std::map<std::string, int> TStrIntMap;
	typedef std::map<std::string, double> TStrdoubleMap;
	typedef std::map<std::string, bool> TStrBoolMap;
	typedef std::map<std::string, std::string> TStrStrMap;

	typedef std::vector<std::string> TStrVec;
	typedef std::vector<int> TIntVec;
	typedef std::vector<PHTypes> TPHTypesVec;
	
	TStrIntMap intParams;
	TStrdoubleMap doubleParams;
	TStrBoolMap boolParams;
	TStrStrMap stringParams;
	
	TStrIntMap allParams;
	//we'll let everything be an alias to itself, and then we'll always just look up aliases
	mutable TStrStrMap aliases;

	TStrVec allParamNames;
	TPHTypesVec allParamTypes;
	TStrVec allAliases;
	
public:
	ParameterHolder(const std::string stringRep);
	
	ParameterHolder();
	virtual ~ParameterHolder();

	std::string stringSerialize();

	virtual std::string getAlias(std::string alias);
	bool supportsParam(std::string alias);

	virtual void setAlias(std::string alias, std::string original);

	virtual void setIntegerParam(std::string alias, int value);
	virtual void setDoubleParam(std::string alias, double value);
	virtual void setBoolParam(std::string alias, bool value);
	virtual void setStringParam(std::string alias, std::string value);

	virtual void addIntegerParam(std::string alias);
	virtual void addDoubleParam(std::string alias);
	virtual void addBoolParam(std::string alias);
	virtual void addStringParam(std::string alias);

//Should have done this a while ago
	virtual void addIntegerParam(std::string alias, int defaultValue);
	virtual void addDoubleParam(std::string alias, double defaultValue);
	virtual void addBoolParam(std::string alias, bool defaultValue);
	virtual void addStringParam(std::string alias, std::string defaultValue);


	virtual int getIntegerParam(std::string alias);
	virtual double getDoubleParam(std::string alias);
	virtual bool getBoolParam(std::string alias);
	virtual std::string getStringParamEncoded(std::string alias);
	virtual std::string getStringParam(std::string alias);
	
	virtual int getParamCount();
	virtual std::string getParamName(int which);
	virtual PHTypes getParamType(int which);
	
	
};

#endif

