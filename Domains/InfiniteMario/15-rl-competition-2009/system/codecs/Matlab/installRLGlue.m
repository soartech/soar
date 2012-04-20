%Install the .m files and the Java codec somewhere, and add them to the path.
function installRLGlue(installLocation)
%This relies on the JVM running, but that's ok so does the whole codec.
pathSeparator=char(java.io.File.separator);
    usingWindows=false;
    if pathSeparator=='\'
        usingWindows=true;
    end

    suggestPath=false;
    
    if nargin==0
        suggestPath=true;
    else
        if isempty(installLocation)
            suggestPath=true;
        end
    end
    
    if suggestPath
        userdir=getuserdir();
        installLocation=strcat(userdir,pathSeparator,'rl-glue',pathSeparator,'codecs',pathSeparator,'matlab');
        fprintf(1,'No install path specified.  Suggesting one...\n');
    end
    
    thePathQuestion=sprintf('Planning to install the matlab rl-glue codec to: \n\t%s\n\nAfter that, we are going to add this (and subdirectories) to your Matlab path, and then save it.\n-- Is that ok',installLocation);

    if ~promptUserYN(thePathQuestion)
        abortInstall('You did not approve of the install location or procedure.');
        return;
    end
    
    createSuccess=makeNewDir(installLocation);
    if(~createSuccess)
        fprintf(1,'Aborting because directory creation failed\n');
        return;
    end
    
    mainLocation=strcat(installLocation,pathSeparator,'src');
    agentLocation=strcat(mainLocation,pathSeparator,'agent');
    envLocation=strcat(mainLocation,pathSeparator,'environment');
    glueLocation=strcat(mainLocation,pathSeparator,'glue');
    
    fprintf(1,'Copying files...\n');
    
    mainSrcFiles=strcat('src',pathSeparator,'*.m');
    [installSuccess,installMessage]=copyfile(mainSrcFiles,mainLocation);
    fprintf(1,'\tsrc ==> %s\n',mainLocation);
    if(~installSuccess)
        fprintf(1,'Aborting because copying some files failed. %s\n',installMessage);
        return;
    end

    agentSrcFiles=strcat('src',pathSeparator,'agent',pathSeparator,'*.m');
    [installSuccess,installMessage]=copyfile(agentSrcFiles,agentLocation);
    fprintf(1,'\tsrc/agent ==> %s\n',agentLocation);
    if(~installSuccess)
        fprintf(1,'Aborting because copying some files failed. %s\n',installMessage);
        return;
    end

    envSrcFiles=strcat('src',pathSeparator,'environment',pathSeparator,'*.m');
    [installSuccess,installMessage]=copyfile(envSrcFiles,envLocation);
    fprintf(1,'\tsrc/environment ==> %s\n',envLocation);
    if(~installSuccess)
        fprintf(1,'Aborting because copying some files failed. %s\n',installMessage);
        return;
    end

    glueSrcFiles=strcat('src',pathSeparator,'glue',pathSeparator,'*.m');
    [installSuccess,installMessage]=copyfile(glueSrcFiles,glueLocation);
    fprintf(1,'\tsrc/glue ==> %s\n',glueLocation);
    if(~installSuccess)
        fprintf(1,'Aborting because copying some files failed. %s\n',installMessage);
        return;
    end

    fprintf(1,'All .m files successfully installed!\n');
    
    fprintf(1,'Adding paths...\n');
    fprintf(1,'\t%s\n',mainLocation);
    addpath(mainLocation);
    fprintf(1,'\t%s\n',agentLocation);
    addpath(agentLocation);
    fprintf(1,'\t%s\n',envLocation);
    addpath(envLocation);
    fprintf(1,'\t%s\n',glueLocation);
    addpath(glueLocation);
    
    saveSuccess=savepath();
    
    if saveSuccess==1
        fprintf(1,'Problem saving the path.  You may have to add them manually...\n');
    else
        fprintf(1,'All paths added and saved successfully!\n');
    end

    
    JarLocation=strcat(installLocation,pathSeparator,'libs');
    
    fprintf(1,'Installing the Java Codec...\n');
    
    javaCodecJar=strcat('libs',pathSeparator,'*.jar');
    [installSuccess,installMessage]=copyfile(javaCodecJar,JarLocation);
    fprintf(1,'\tlibs ==> %s\n',JarLocation);
    if(~installSuccess)
        fprintf(1,'Aborting because copying some files failed. %s\n',installMessage);
        return;
    end
    
    fprintf(1,'Java Codec jar installed successfully.\n');
    fprintf(1,'  --- Congratulations.  RL-Glue Matlab codec has been installed! ---\n');
    return;
end


function createSuccess=makeNewDir(newDirectoryLocation)
    createSuccess=false;
    
    fprintf(1,'Creating the directory %s  ...',newDirectoryLocation);
    theJFile=java.io.File(newDirectoryLocation);
    
    createSuccess=theJFile.isDirectory();
    
    if createSuccess
        fprintf(1,'success (already existed)!\n');
    else
        createSuccess=theJFile.mkdirs();
        if createSuccess
            fprintf(1,'success (created)!\n');
        else
            fprintf(1,'failed.  Not sure why.\n');
        end
    end
end

function shouldProceed=promptUserYN(theString)
    shouldProceed=false;
    
    thePrompt=strcat(theString,' yes/no [no]: ');
    %Windows is picky and eats the separator char in the input function
    if isWindows()
       thePrompt=strrep(thePrompt,'\','\\');
    end
    reply = input(thePrompt,'s');
    if ~isempty(reply)
        if strcmpi(reply,'y') || strcmpi(reply,'yes')
            shouldProceed=true;
        end
    end
end

function abortInstall(whyMessage)
    fprintf(1,'Installation of RL-Glue Matlab Codec cancelled for reason:\n\t%s\n',whyMessage);
end

%Thanks to sven probst.
%http://www.mathworks.com/matlabcentral/fileexchange/15885
function userDir = getuserdir()
    %GETUSERDIR   return the user home directory.
    %   USERDIR = GETUSERDIR returns the user home directory using the registry
    %   on windows systems and using Java on non windows systems as a string
    %
    %   Example:
    %      getuserdir() returns on windows
    %           C:\Documents and Settings\MyName\Eigene Dateien

    if ispc
        userDir = winqueryreg('HKEY_CURRENT_USER',...
            ['Software\Microsoft\Windows\CurrentVersion\' ...
             'Explorer\Shell Folders'],'Personal');
    else
        userDir = char(java.lang.System.getProperty('user.home'));
    end
end

function isWin= isWindows()
    isWin=false;
    if char(java.io.File.separator)=='\'
        isWin=true;
    end
end