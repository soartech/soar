#!/bin/bash
export SOAR_HOME="$(pwd)/bin"
export LD_LIBRARY_PATH="$SOAR_HOME"
java -jar "$SOAR_HOME/VisualSoar.jar"

