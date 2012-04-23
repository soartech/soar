#!/bin/bash
unset DYLD_LIBRARY_PATH
cd $(dirname "$0")
java -jar SoarRobotServer.jar

