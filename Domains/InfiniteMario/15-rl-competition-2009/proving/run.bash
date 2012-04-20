#!/bin/bash


#Variables
basePath=../
systemPath=$basePath/system
libPath=$systemPath/libraries
provingPath=$systemPath/proving

compLib=$libPath/RLVizLib.jar

echo "Starting up proving software..."
export LC_ALL=en_US
macAboutNameCommand=-Dcom.apple.mrj.application.apple.menu.about.name=RLCompetitionProvingApp
java -Xmx128M  -Duser.language=en $macAboutNameCommand -jar ./bin/Proving.jar
echo "-- Proving software finished"
