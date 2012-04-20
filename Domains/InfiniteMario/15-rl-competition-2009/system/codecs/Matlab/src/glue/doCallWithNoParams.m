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
%   $Revision: 798 $
%   $Date: 2009-02-26 21:11:35 -0500 (Thu, 26 Feb 2009) $
%   $Author: brian@tannerpages.com $
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/glue/doCallWithNoParams.m $
%
function doCallWithNoParams(state)
    global p__rlglueStruct;
    try
		p__rlglueStruct.network.clearSendBuffer();
		p__rlglueStruct.network.putInt(state);
		p__rlglueStruct.network.putInt(0);
		p__rlglueStruct.network.flipSendBuffer();
		p__rlglueStruct.network.send();
    catch
        theError=lasterror();
        fprintf(1,'Java threw an exception when trying to do your Experiment command.\n');
        fprintf(1,'Maybe you forgot to call disconnectGlue() after your last Experiment crashed?\n');
        fprintf(1,'I have taken the liberty to call it for you.  Try your experiment again.\n');
%        fprintf(1,'The type of exception was: %s Message was %s\n',theError.identifier,theError.message);
        disconnectGlue();
    end
end
