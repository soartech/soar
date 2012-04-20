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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_empty_experiment.m $
%
function failures=test_empty_experiment()
        failures=0;
        totalTests=0;
        
        RL_init();
        

        for whichEpisode = [1:1:4]
            startTuple=RL_start();
            
            if mod(whichEpisode,2)==0
                [failures,totalTests]=check_fail(length(startTuple.a.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.charArray)~=0,failures,totalTests);

                [failures,totalTests]=check_fail(length(startTuple.o.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.charArray)~=0,failures,totalTests);
            else
                [failures,totalTests]=check_fail(length(startTuple.a.intArray)~=7,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.doubleArray)~=3,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.charArray)~=1,failures,totalTests);

                [failures,totalTests]=check_fail(length(startTuple.o.intArray)~=2,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.doubleArray)~=4,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.charArray)~=5,failures,totalTests);
            end
        for whichStep=0:1:4
    		stepTuple=RL_step();
            [failures,totalTests]=check_fail(stepTuple.terminal~=0,failures,totalTests);
            [failures,totalTests]=check_fail(stepTuple.terminal~=0,failures,totalTests);

            if mod(whichEpisode,2)==0
                [failures,totalTests]=check_fail(length(stepTuple.a.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.charArray)~=0,failures,totalTests);

                [failures,totalTests]=check_fail(length(stepTuple.o.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.charArray)~=0,failures,totalTests);
            else
                [failures,totalTests]=check_fail(length(stepTuple.a.intArray)~=7,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.doubleArray)~=3,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.charArray)~=1,failures,totalTests);

                [failures,totalTests]=check_fail(length(stepTuple.o.intArray)~=2,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.doubleArray)~=4,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.charArray)~=5,failures,totalTests);
            end
        end

	
	

        end

        
        
        
        
        
        

            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_empty_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_empty_experiment\n',totalTests);
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