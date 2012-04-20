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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/glue/forceConnection.m $
%
%
%NOTE: Because checkForJavaCodec might call javaddpath, which calls
%clear('java'), we have to declare p__rlglueStruct as global BEFORE and
%AFTER checkForJavaCodec.  Sometimes I hate matlab.  Well, usually.
function forceConnection()
    global p__rlglueStruct;
    
    needsSetup=false;
    
    if ~isfield(p__rlglueStruct,'network')
        needsSetup=true;
    else
       if ~p__rlglueStruct.network.isConnected()
           needsSetup=true;
       end
    end
    
           
    if needsSetup
		%NOTE: If the java package isn't in your path already, this WILL delete all your globals
		%because it calls javaddpath which calls clear('java')
	    checkForJavaCodec();
        global p__rlglueStruct;
		global p__rlglueSettings;
        p__rlglueStruct.network = org.rlcommunity.rlglue.codec.network.Network;


%Set defaults for host and port
        host=char(org.rlcommunity.rlglue.codec.network.Network.kDefaultHost);
        port=org.rlcommunity.rlglue.codec.network.Network.kDefaultPort;
        timeout=org.rlcommunity.rlglue.codec.network.Network.kRetryTimeout;
        %This is new as of version 1.02, it will allow us to more easily
        %abort and experiment with CTRL-C while connecting.
        blocking=false;
        

%Pick up user specifications if there are any
		if isfield(p__rlglueSettings,'port')
			port=p__rlglueSettings.port;
		end
		if isfield(p__rlglueSettings,'host')
			host=p__rlglueSettings.host;
		end

        fprintf(1,'RL-Glue Matlab Experiment Codec Version: %s (%s)\n',RL_get_codec_version(),RL_get_svn_version());
        fprintf(1,'\tConnecting to rl_glue at host: %s on port %d\n', char(host), port);

        % Connect
        actuallyConnected=p__rlglueStruct.network.connect(host,port,timeout,blocking);
        
        % Since we are non-blocking, we need to be a bit careful and be
        % sure that we are actually connected.  By doing this loop part in
        % Matlab it means we can do CTRL-C
        
        while ~actuallyConnected
            actuallyConnected=p__rlglueStruct.network.ensureConnected();
            pause(1/2);
        end

        fprintf(1,'\tExperiment Codec Connected\n');
        p__rlglueStruct.network.clearSendBuffer();
        p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kExperimentConnection);
        p__rlglueStruct.network.putInt(0);
        p__rlglueStruct.network.flipSendBuffer();

        p__rlglueStruct.network.send();
    end
end
