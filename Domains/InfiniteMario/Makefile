
*.class: ./java/src/*.java
	javac -source 5 -sourcepath ./java/src/ -cp $(SOAR_HOME)/share/java/sml.jar:$(COMP_HOME)/system/libraries/rl-viz/RLVizLib.jar -d ./java/bin/ ./java/src/*.java 

clean:
	rm -Rf ./java/bin/*
