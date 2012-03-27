#!/bin/bash
export SOAR_HOME="$(dirname "$0")"
export LD_LIBRARY_PATH="$SOAR_HOME/bin"
cd $SOAR_HOME
java -jar "$SOAR_HOME/VisualSoar.jar"

