%  Copyright 2008 Brian Tanner
%  http://rl-glue-ext.googlecode.com/
%  brian@tannerpages.com
%  http://research.tannerpages.com
%  
%   Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%  
%       http://www.apache.org/licenses/LICENSE-2.0
%  
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%  
%   $Revision: 637 $
%   $Date: 2009-02-07 16:02:45 -0500 (Sat, 07 Feb 2009) $
%   $Author: brian@tannerpages.com $
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_1_experiment.m $
%
function failures=test_1_experiment()
        failures=0;
        totalTests=0;
        
        RL_init();
        

	RL_start();

    roat=RL_step();

	
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('one|1.|one',RL_env_message('one')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('one|1.|one',RL_agent_message('one')),failures,totalTests);
 
 	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	

	roat=RL_step();

    [failures,totalTests]=check_fail(~strcmp('two|2.2.|two',RL_env_message('two')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('two|2.2.|two',RL_agent_message('two')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(1)~=1,failures,totalTests);

	roat=RL_step();

    [failures,totalTests]=check_fail(~strcmp('three||three',RL_env_message('three')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('three||three',RL_agent_message('three')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(1)~=2,failures,totalTests);
    
	roat=RL_step();
    
    
    [failures,totalTests]=check_fail(~strcmp('four|4.|four',RL_env_message('four')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('four|4.|four',RL_agent_message('four')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(1)~=3,failures,totalTests);

	roat=RL_step();
    [failures,totalTests]=check_fail(~strcmp('five|5.5.|five',RL_env_message('five')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('five|4.|five',RL_agent_message('five')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal==0,failures,totalTests);
        

%Gabor has made it so this environment will step past terminal.  This is
%not something we want to do in general at all.
%But, in order to keep the other tests all working, I'll allow it
   	roat=RL_step();
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=5,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=5,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=5,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(1)~=173,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(2)~=-173,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(3)~=2147483647,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(4)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(5)~=-2147483648,failures,totalTests);

    [failures,totalTests]=check_fail(roat.o.doubleArray(1)~=0.0078125,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.doubleArray(2)~=-0.0078125,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.doubleArray(3)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.doubleArray(4)~=0.0078125e150,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.doubleArray(5)~=-0.0078125e150,failures,totalTests);

    [failures,totalTests]=check_fail(roat.o.charArray(1)~='g',failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.charArray(2)~='F',failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.charArray(3)~='?',failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.charArray(4)~=' ',failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.charArray(5)~='&',failures,totalTests);

            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_1_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_1_experiment\n',totalTests);
        end
        disconnectGlue();
end


function [failures, totalTests]=check_fail(theCondition, failures, totalTests)
    totalTests=totalTests+1;
    if(theCondition)
        fprintf(1,'Failed test %d\n',totalTests);
        failures=failures+1;
    end
end