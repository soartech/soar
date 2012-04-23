===========================
Soar Shuffler Documentation
===========================
This python script creates zip files of the all the downloads that we post to the Soar web site.  It reads in a data file (Soar_Projects_Filelist.py) that contains a declarative representation of how the script should put together the zip file.  

It can do a variety of different things based on the contents of that data file:
  - Create a zip
  - Just copy files from one location on the computer to another.  (good for documentation files that we both include in zips and post directly to the web site)
  - Create arbitrary directory structures within the zip (the repository directory structure does not have to mirror the release structure, files can be re-used across projects, etc.)
  - Move the zip to a particular release directory (to make it easy to upload the files to the web site and keep URLs correct)
  - Create a multi-platform release.  This will create five different platform-specific versions of a zip file, automatically copying platform-specific version of dll, launch scripts, etc. based on variables in the project description.

To run the script, simply type:
  import soar_shuffler
  soar_shuffler.doit(False)

Note:  If you don't include "False", it will only execute projects that have changed files, but that features is still a bit buggy.

===================
Directory Structure
===================
This script does not have any parameters and assumes a fixed directory structure. There are two root directories you can change at the top of soar_shuffler.py, Repository_Dir and Output_Dir, for example:

Repository_Dir = "C:\\Soar\\repository\\trunk\\"
Output_Dir = "C:\\Soar\\output\\"

The Output_Dir is where the script will place all the final files (usually zip files).  The Repository_Dir is where it pulls all the files from and should be an svn checkout of trunk.

In addition to those two hard-coded paths, the project descriptions data file specifies exactly where it expects to find files.  Furthermore, the way that the script automates building multi-platform zips (five zips based on a single project description, not a single zip for multiple platforms) places a few other constraints.  

Here is a summary of the directory structure you need to get the script to work with the included project description file:

+ Output Directory (specified in variable described above)
+ Repository directory (specified in variable described above.  Check out trunk into this directory.  Most of the following folders will be created by the checkout.)
    +---AgentDevelopmentTools
    +---Agents
    +---Compiled (This is where you put the various binaries you compiled manually.  You must create these folders.  They are not in the repository and will not be created when you check out trunk.)
    |   +---linux32
    |   |   +---out (out directory produced by building SoarSuite on 32-bit Linux)
    |   |   \---soar_robot_server (release directory produced by building RoomsWorld on 32-bit Linux)
    |   +---linux64
    |   |   +---out (out directory produced by building SoarSuite on 64-bit Linux)
    |   |   \---soar_robot_server (release directory produced by building RoomsWorld on 64-bit Linux)
    |   +---mac64
    |   |   +---out (out directory produced by building SoarSuite on OSX)
    |   |   \---soar_robot_server (release directory produced by building RoomsWorld on OSX)
    |   +---Soar2D (contains Eaters_TankSoar.jar)
    |   +---VisualSoar (contains VisualSoar.jar)
    |   +---windows32
    |   |   +---out (out directory produced by building SoarSuite on 32-bit Windows)
    |   |   \---soar_robot_server (release directory produced by building RoomsWorld on 32-bit Windows)
    |   \---windows64
    |       +---out (out directory produced by building SoarSuite on 64-bit Windows)
    |       \---soar_robot_server (release directory produced by building RoomsWorld on 64-bit Windows)
    +---Deprecated
    +---Documentation
    +---Domains
    +---ExampleDomainDevelopmentCode
    +---ExperimentalModules
    +---GeneratingTools
    +---Release (various readme, etc.)
    |   +---linux (Linux launch scripts, .sh files)
    |   +---osx (Mac launch scripts, .command files)
    |   \---win (Window's launch scripts, .bat files)
    +---SoarSuite
    \---Unsupported

===================
Data File Structure
===================
This file specifies how projects are zipped up and files are moved around.  Currently, a single data file contains descriptions of every project.  This data file is called "Soar_Projects_Filelist.py" and should be in the same directory as "soar_shuffler.py".

- The first line of each project contains only the project name and delimits one project from another.  Any line that does NOT contain an equal sign "=" is considered a new project name.
- Other lines can either specify a parameter describing that project or how to move or organize files within the project
  - Parameters:  There are two parameters you can set for a project:  type and out
      - Type 
          - 'zip': zip up the files described in the project
          - 'copy' : copy files from one directory to another as descirbed in the project
          - 'multiplatform-zip': zip up the files described in the project but copy different version of platform-specific items like libraries and launch scripts.  Project descriptions of this type contain variables telling it which files need special version and where it can find them.
          - For example, "type=multiplatform-zip" will create five zips for a particular project description
      - Out
          - This parameter specified where in the output direct it places the final zip or copied files.
          - For example, "out=SoarSuite\" will move the final results into Output Directory\SoarSuite
  - File organization desciptors:
      - Each entry follows the following structure
          - (path relative to Repository_Dir)=(destination directory)
          - For example, Compiled\Soar2D\Eaters_TankSoar.jar=bin would put "Eaters_TankSoar.jar" into a top-level folder called "bin" while "Agents\MyAgent=Agents\default" would zip/copy all of the contents of the MyAgent directory into a new directoy called default.
          - The path can specify either a directory, in which case it will copy the entire directory, or a specific file.  Similarly, the destination can specify either a target directory or a specific target filename, which would also result in a rename.
          - You can specify "top" instead of a destination directory to tell the script to put the source items at the top level of the zip or destination directory.
          - Note that if you specify a directory as the source, it will not copy the top level folder.  If you want to keep that folder as well, specify it in the destination name, for example "Agents\default=Agents\default".
          - Spaces are fine in file names.
      - Multi-platform zips can also contain variables in the file organization descriptors to tell it how to specialize a release for a particular platform.  There are four different variables you can use:
          - RELEASE_DIR:  will instantiate as "win" for windows, "linux" for linux, and "osx" for mac.  This allows the utility to pull from different folders that are platform-specific but not 32 or 64 bit specific, for example launch scripts or build instructions.
          - COMPILE_DIR: will instantiate as "windows64", "windows32", "linux64", "linux32", "mac64" depending on the platform.  This allows the utility to pull files from different folders that are specific to a particular architecture, most notably the compiled Soar binaries.
          - LAUNCH_EXTENSION: will instantiate as .bat, .sh or .command files inside a zip based on the platform in question.  This allows the utility to name the launch scripts appropriately.
              - Note: The script will automatically give those files executable privileges within the zip, which can be a bit tricky since how that works is different on each platform.
          - DLL_EXTENSION: Will instantiate as .dll, .so or .dylib based on the platform in question.
              - Note: The OSX releases have two special cases, that are handled separately:  libJava_sml_ClientInterface.DLL_EXTENSION will be instantiated as libJava_sml_ClientInterface.jnilib while _Python_sml_ClientInterface.DLL_EXTENSION will become _Python_sml_ClientInterface.so.  All other cases on the mac will becomes .dylib's.
              - Note: The windows substitution will also strip out any "lib" from the beginning of the filename because Mac and Linux both use the convention that all libraries should start their name with lib.  Make sure to use the linux convention to ensure it works on all platforms, for example, "Compiled\COMPILE_DIR\out\libSoar.DLL_EXTENSION=bin"
- Comments can be included in the data file by starting a line with #.

============
Known Issues
============
- When launching, the script will empty out the output directory.  Sometimes it doesn't wait long enough for the the output directory to be deleted.  If you get an error related to that, just run it again.
- There is code to only re-zip files whose contents have changed.  It doesn't seem to always work properly, but the whole script works fast enough that fixing it is not a high priority.