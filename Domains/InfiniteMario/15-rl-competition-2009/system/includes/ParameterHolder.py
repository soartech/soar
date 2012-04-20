# Parameter Holder for Passing Parameters Around
# Copyright (C) 2007, Brian Tanner brian@tannerpages.com (http://brian.tannerpages.com/)
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

INT_PARAM=0
DOUBLE_PARAM=1
BOOL_PARAM=2
STRING_PARAM=3

class ParameterHolder:
	
	
	def __init__(self,stringRep=None):
		self.intParams = {}
		self.doubleParams = {}
		self.boolParams = {}
		self.stringParams = {}

		self.allParams = {}
		#we'll let everything be an alias to itself, and then we'll always just look up aliases
		self.aliases = {}

		self.allParamNames = []
		self.allParamTypes = []
		self.allAliases = []
		if stringRep == None:
			return
		arrayRep = stringRep.split('_')
		pointerType = arrayRep.pop(0)
		if pointerType == "NULL":
			return
		numParams = arrayRep.pop(0)
		numParams = int(numParams)
		for i in range(numParams):
			thisParamName = arrayRep.pop(0)
			thisParamType = int(arrayRep.pop(0))
			if thisParamType == INT_PARAM:
				iParamValue = int(arrayRep.pop(0))
				self.addIntegerParamWithDefault(thisParamName,iParamValue)
			elif thisParamType == DOUBLE_PARAM:
				fParamValue = float(arrayRep.pop(0))
				self.addDoubleParamWithDefault(thisParamName,fParamValue)
			elif thisParamType == BOOL_PARAM:
				bParamValue = arrayRep.pop(0)
				if bParamValue == "true":
					self.addBoolParamWithDefault(thisParamName,True)
				else:
					self.addBoolParamWithDefault(thisParamName,False)
			elif thisParamType == STRING_PARAM:
				sParamValue = arrayRep.pop(0)
				self.addStringParamWithDefault(thisParamName,sParamValue)
		
		numAliases = arrayRep.pop(0)
		numAliases = int(numAliases)
		for i in range(numAliases):
			thisAlias = arrayRep.pop(0)
			thisTarget = arrayRep.pop(0)
			self.setAlias(thisAlias,thisTarget)


	def stringSerialize(self): #return string
		outString = "PARAMHOLDER_%d_" % (len(self.allParamNames))
		for i in range(len(self.allParamNames)):
			paramType = self.allParamTypes[i]
			outString = outString + "%s_%d_" % (self.allParamNames[i],paramType)
			if paramType == INT_PARAM:
				outString = outString + "%d_" % (self.getIntegerParam(self.allParamNames[i]))
			elif paramType == DOUBLE_PARAM:
				outString = outString + "%f_" % (self.getDoubleParam(self.allParamNames[i]))
			elif paramType == BOOL_PARAM:
				if (self.getBoolParam(self.allParamNames[i])):
					outString = outString + "true_"
				else:
					outString = outString + "false_"
			else:
				outString = outString + self.getStringParamEncoded(self.allParamNames[i]) + "_"
				
			
		outString = outString + "%d_" % (len(self.allAliases))
		for i in range(len(self.allAliases)):
			outString = outString + "%s_%s_" % (self.allAliases[i],self.getAlias(self.allAliases[i]))
		
		return outString


	def getAlias(self,alias): #return string
		if not self.aliases.has_key(alias):
			sys.stderr.write("You wanted to look up original for alias: %s, but that alias hasn't been set\n" % (alias))
			sys.exit(-1)
		return self.aliases[alias]
	
	def supportsParam(self,alias): #return bool
		return self.aliases.has_key(alias)

	def setAlias(self,alias,original): #void
		if not self.allParams.has_key(original):
			sys.stderr.write("C++ Parameter Holder::Careful, you are setting an alias of:%s to: %s but: %s isn't in the parameter set\n" % (alias,original,original))
			sys.exit(1)
		self.aliases[alias] = original
		self.allAliases.append(alias)
		

	def setIntegerParam(self,alias,value): #void
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are setting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
		self.intParams[name] = value
	
	def setDoubleParam(self,alias,value): #void
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are setting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
		self.doubleParams[name] = value
	
	def setBoolParam(self,alias,value): #void
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are setting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
		self.boolParams[name] = value
	
	def setStringParam(self,alias,value): #void
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are setting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
		value = value.replace(":","!!COLON!!")
		value = value.replace("_","!!UNDERSCORE!!")
		self.stringParams[name] = value

	def addIntegerParam(self,alias): #void
		self.allParams[alias] = INT_PARAM
		self.allParamNames.append(alias)
		self.allParamTypes.append(INT_PARAM)
		self.setAlias(alias,alias)
	
	def addDoubleParam(self,alias): #void
		self.allParams[alias] = DOUBLE_PARAM
		self.allParamNames.append(alias)
		self.allParamTypes.append(DOUBLE_PARAM)
		self.setAlias(alias,alias)
	
	def addBoolParam(self,alias): #void
		self.allParams[alias] = BOOL_PARAM
		self.allParamNames.append(alias)
		self.allParamTypes.append(BOOL_PARAM)
		self.setAlias(alias,alias)
	
	def addStringParam(self,alias): #void
		self.allParams[alias] = STRING_PARAM
		self.allParamNames.append(alias)
		self.allParamTypes.append(STRING_PARAM)
		self.setAlias(alias,alias)

	#Should have done this a while ago
	def addIntegerParamWithDefault(self,alias,defaultValue): #void
		self.addIntegerParam(alias)
		self.setIntegerParam(alias, defaultValue)
	
	def addDoubleParamWithDefault(self,alias,defaultValue): #void
		self.addDoubleParam(alias)
		self.setDoubleParam(alias, defaultValue)
	
	def addBoolParamWithDefault(self,alias,defaultValue): #void
		self.addBoolParam(alias)
		self.setBoolParam(alias, defaultValue)
	
	def addStringParamWithDefault(self,alias,defaultValue): #void
		self.addStringParam(alias)
		self.setStringParam(alias, defaultValue)

	def getIntegerParam(self,alias): #int
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
			sys.exit(1)
		if not self.intParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter isn't an int parameter...\n" % (name))
			sys.exit(1)
		return self.intParams[name]
	
	def getDoubleParam(self,alias): #double
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
			sys.exit(1)
		if not self.doubleParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter isn't a double parameter...\n" % (name))
			sys.exit(1)
		return self.doubleParams[name]
	
	def getBoolParam(self,alias): #bool
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
			sys.exit(1)
		if not self.boolParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter isn't a boolean parameter...\n" % (name))
			sys.exit(1)
		return self.boolParams[name]
	
	def getStringParamEncoded(self,alias): #string
		name = self.getAlias(alias)
		if not self.allParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter hasn't been added...\n" % (name))
			sys.exit(1)
		if not self.stringParams.has_key(name):
			sys.stderr.write("Careful, you are getting the value of parameter: %s but the parameter isn't a string parameter...\n" % (name))
			sys.exit(1)
		return self.stringParams[name]
	
	def getStringParam(self,alias):
		encodedVersion = self.getStringParamEncoded(alias)
		fixedVersion = encodedVersion.replace("!!COLON!!",":")
		fixedVersion = fixedVersion.replace("!!UNDERSCORE!!","_")
		return fixedVersion

	def getParamCount(self): #int
		return len(self.allParams)
	
	def getParamName(self,which): #string
		return self.allParamNames[which]
	
	def getParamType(self,which): #PHTypes
		return self.allParamTypes[which]



