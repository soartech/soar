#!/bin/bash
export SOAR_HOME="$(dirname "$0")/bin"
export DYLD_LIBRARY_PATH="$SOAR_HOME"
cd $(dirname "$0")
java -XstartOnFirstThread -jar "$SOAR_HOME/Eaters_TankSoar.jar" config/eaters.cnf &

