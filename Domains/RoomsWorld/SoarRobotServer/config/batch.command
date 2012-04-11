#!/bin/bash
#export SOAR_HOME="$(dirname "$0")"
export SOAR_HOME="$HOME/sandbox"
export DYLD_LIBRARY_PATH="$SOAR_HOME/lib"
java -cp \
$SOAR_HOME/share/java/april-mega.jar:\
$SOAR_HOME/share/java/commons-logging-1.1.1.jar:\
$SOAR_HOME/share/java/gluegen-rt.jar:\
$SOAR_HOME/share/java/grrc.jar:\
$SOAR_HOME/share/java/guava-license.txt:\
$SOAR_HOME/share/java/guava-r06.jar:\
$SOAR_HOME/share/java/ini4j-0.5.1.jar:\
$SOAR_HOME/share/java/jcommon-1.0.10.jar:\
$SOAR_HOME/share/java/jfreechart-1.0.6.jar:\
$SOAR_HOME/share/java/jfreechart-1.0.6-swt.jar:\
$SOAR_HOME/share/java/jgoodies-common-0.9.9.jar:\
$SOAR_HOME/share/java/jgoodies-forms-1.4.0b1.jar:\
$SOAR_HOME/share/java/jinput.jar:\
$SOAR_HOME/share/java/jogl.jar:\
$SOAR_HOME/share/java/jzlib.jar:\
$SOAR_HOME/share/java/lcm.jar:\
$SOAR_HOME/share/java/log4j-1.2.15.jar:\
$SOAR_HOME/share/java/orc.jar:\
$SOAR_HOME/share/java/sml.jar:\
$SOAR_HOME/share/java/soar-smljava-9.3.0.jar:\
$SOAR_HOME/share/java/soar-visualsoar-9.3.0.jar:\
$SOAR_HOME/share/java/sqlite-jdbc-3.6.20.1.jar:\
$SOAR_HOME/share/java/stopwatch-0.4-with-deps.jar:\
$SOAR_HOME/share/java/swingx-1.6.1.jar:\
$SOAR_HOME/share/java/swingx-1.6.1-license.txt:\
$SOAR_HOME/share/java/Soar-Robot.jar \
edu.umich.robot.Application --config config/data1/data1.runs

