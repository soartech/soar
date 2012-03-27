#!/bin/bash
export SOAR_HOME="$(dirname "$0")"
export DYLD_LIBRARY_PATH="$SOAR_HOME/bin"
cd $SOAR_HOME
java -XstartOnFirstThread -jar "$SOAR_HOME/Eaters_TankSoar.jar" ../config/eaters.cnf &

