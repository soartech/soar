=========================
RL-Glue Distribution
=========================
This project is licensed under the Apache 2 license. Check LICENSE-2.0.txt for more info. To paraphrase, you can use this code how you see fit.

----------------------------
Documentation
----------------------------

You should probably look at the overview and technical manuals for this project. They are in:
docs/Glue-Overview.pdf
docs/TechnicalManual.pdf


For the full RL-Glue Documentation, please visit RL-Glue's public home:
http://glue.rl-community.org

----------------------------
Super short introduction to RL-Glue
----------------------------
Each run with RL-Glue will use four components: the glue, an environment program, an agent program, and an experiment program.  In the most general case, each of these components is a separate process and they all communicate through the glue using network sockets.

To make this work, you're going to need some sort of C compiler.

This code is tested on Linux, Mac OS X, and Cygwin.

----------------------------
Getting Started
----------------------------
This project is built with GNU autotools, so you should just need to do:
	$>./configure
	$>make
	$>sudo make install
	
This will install the necessary libraries, binaries, and headers to:
	/usr/local/lib
	/usr/local/bin
	/usr/local/includes/rlglue
	
If you don't have sudo priviledges or you would like to install them somewhere else (Ex: your home directory):
	$>./configure --prefix=/path/to/your/home/directory         EG: /home/Users/btanner
	$>make
	$>make install



