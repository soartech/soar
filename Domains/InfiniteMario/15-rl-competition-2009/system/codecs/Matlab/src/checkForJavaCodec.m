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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/checkForJavaCodec.m $
%
function [alreadyFound,foundAtAll]=checkForJavaCodec()
alreadyFound=false;
foundAtAll=false;
%Check if Matlab can find the RL-Glue classes
rlglueClassesFound=exist('org.rlcommunity.rlglue.codec.RLGlue','class');

%8 is the return code for Java class
if rlglueClassesFound==8
    alreadyFound=true;
    foundAtAll=true;
    return;
end

foundCodec=tryToFindJavaCodec();

if ~foundCodec
    error('Could not find the Java RL-Glue Codec. Please do: javaddpath(''/path/to/JavaRLGlueCodec.jar'' before using the Matlab RL-Glue codec.');
else
    foundAtAll=true;
end

end

function foundCodec=tryToFindJavaCodec()
    foundCodec=false;
%Find out where the codec is installed.
    fileToFind='checkForJavaCodec.m';
    ftfLen=length(fileToFind);
    
    codecSrcFilePath=which('checkForJavaCodec.m');
    pathLength=length(codecSrcFilePath);
    
    justPath=codecSrcFilePath(1:pathLength-ftfLen);
    
    javaCodecJarPath=strcat(justPath,'../libs/JavaRLGlueCodec.jar');
    javaaddpath(javaCodecJarPath);

    rlglueClassesFound=exist('org.rlcommunity.rlglue.codec.RLGlue','class');
    %8 is the return code for Java class
    if rlglueClassesFound==8
        foundCodec=true;
    end
end

%This is the old code before we had an installer.  Yay installer!
%Look in the default location which is the matlab location and then
%../Java/products/JavaRLGlueCodec.jar
% function foundCodec=tryToFindJavaCodec()
%     currentDir=pwd;
%     foundCodec=false;
%     %Look for /Matlab in the current directory, hoping we're in
%     %somthing/to/codecs/Matlab/..  because if that's true maybe
%     %/something/to/codecs/Java exists.
%     matlabIndices=strfind(currentDir,'codecs/Matlab');
%     
%     for thisMatlabIndex=matlabIndices
%         thisSubString=currentDir(1:thisMatlabIndex-1);
%         possibleJavaLocation=sprintf('%scodecs/Java/products/JavaRLGlueCodec.jar',thisSubString);
%         existString=sprintf('exist(''%s'',''file'')',possibleJavaLocation);
%         
%         thatJarExists=eval(existString);
%         if thatJarExists == 2
%            fprintf(1,'found java jar at %s\n', possibleJavaLocation);
%            javaaddpath(possibleJavaLocation);
%            foundCodec=true;
%            return;
%         end
%     end
% end
