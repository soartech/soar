This is the Rooms World project, formerly known as Sproom.

To build SoarRobotServer from source:

Requires:
Soar 9.3.2
Ant
Java 6

1) Move the following files from the Soar release into the correct native/ folder:

OSX:
libJava_sml_ClientInterface.jnilib
libSoar.dylib
libTestExternalLibraryLib.dylib
soar-debugger.jar
soar-debugger-api.jar

Linux:
livJava_sml_ClientInterface.so
libSoar.so
libTestExternalLibrary.so

Windows:
Java_sml_ClientInterface.dll
Soar.dll
TastExternalLibraryLib.dll
soar-debugger.jar
soar-debugger-api.jar

2) Also put the following files form your Soar release into the correct native folder:

soar-debugger.jar
soar-debugger-api.jar
sml.jar
soar-smljava.jar
swt.jar

3) On a command line, navigate to the SoarRobotServer directory and execute "ant" to build the project.
4) Windows and OSX: In the release directory, in the sub-folder for your platform, double-click on SoarRobotServer.jar. Alternatively, run "java -jar SoarRobotServer.jar"
Linux: On a command line, in the release directory, in the sub-folder for your platform, run "./SoarRobotServer.sh"
5) Select a config file, for example, config/basic/3x3.txt. The simulator should start running and a Soar Debugger should appear.

To run: open release/[platform]/SoarRobotServer.jar
To distribute: release/[platform].zip

NOTE:

You can safely remove the release directory or the SoarRobotServer/build directories to save space. These should be in svnignore.txt files anyway.

TODO:

Make sure all the right LICENCE.txt files are in place before this source code goes public.
Test Soar agents to make sure they work the way we want.
Add documentation to the release so end users will know what to do with it.
Deal with SoarRobotTablet, which is pretty much ignored for now.
Maybe: Change Ant build process to look for requirements in $SOAR_HOME instead of requiring files to be copied manually into the right native directory.

There are three main parts of the project that are Soar-related:

SoarRobotServer: The main robot simulator.
SoarRobotTablet: An android client for interacting with the simulation over a network.
SoarRobotLibrary: Shared code between the server and the Android project.

Then there are dependencies that we will keep here because it's more convenient for the build process, plus some of them are old versions that we aren't currently planning on migrating:

april:

This is our fork of the April project from about summer 2011. We made some changes that we rely on, and it's more trouble than it's worth to migrate over to the newer versions of April, so we're just keeping this around and using it.

jogl:

Contains jogl version 1.1.1 for various platforms. We're using version 1.1.1 for some reason and that's not easy to find online so we're keeping our own copies around.

It looks like jogl 1.1.1 is online at: http://download.java.net/media/jogl/builds/archive/jsr-231-1.1.1/

junit_lib:

Unit testing -- it looks like this is required by libgrrc.

libgrrc-java:

Code for communicating with real, live robots.

Then there are folders that are organized to help with the build process.

native:

Contains platform-specific binary files.

release:

Folder for putting platform-specific, release-ready jars and native dependencies in.

robot_lib:

Various robotics-related jar files. This used to be called ROBOT in Eclipse.
