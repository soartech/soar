#!/bin/bash
export SOAR_HOME="$(dirname "$0")/bin"
export DYLD_LIBRARY_PATH="$SOAR_HOME"
cd $(dirname "$0")
java -jar "$SOAR_HOME/VisualSoar.jar" &

