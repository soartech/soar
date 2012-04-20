=========================
C/C++ Codec for RL-Glue
=========================
This project is licensed under the Apache 2 license. Check LICENSE-2.0.txt for more info. To paraphrase, you can use this code how you see fit.

----------------------------
Documentation
----------------------------
You should probably check out the instructions in:
docs/C-Codec.pdf

For the full RL-Glue Codec Documentation, please visit:
http://glue.rl-community.org/Home/Extensions/c-c-codec

-------------------------------------
Super short introduction to the Codec
-------------------------------------
This code is (will be) tested on Linux, Mac OS X, and Cygwin.

----------------------------
Getting Started
----------------------------
This project is built with GNU autotools, so you should just need to do:
	$>./configure
	$>make
	$>sudo make install
	
This will install the necessary libraries and headers to:
	/usr/local/lib
	/usr/local/includes/rlglue
	
If you don't have sudo priviledges check the docs for additional information.
