GDL Translator Readme
---------------------
This directory contains a Python program that translates game definitions
from Game Description Language (GDL; usually stored in files with .kif
extension) into self-contained Soar agents that simulate the mechanics
of the game in working memory and productions. See

http://games.stanford.edu/language/language.html

for more information on GDL.

Run the translator as follows:

python translate.py < input.kif > output.soar

where "<" redirects the input file to the program's standard input and
">" redirects the program's standard output to the output file. For
Windows users not familiar with this convention please see

http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/redirection.mspx

The generated agent contains productions that initialize the game
state, proposal rules that propose legal moves, and application rules
that update the game state when legal moves are executed. It does not
contain any other knowledge, such as which moves to choose or what kinds
of heuristics to use.


Content Descriptions
--------------------
translate.py    - The main Python script to run.
common.soar     - Productions that need to be included by all generated agents.
kifs/           - Directory containing example games.
trans/          - Directory containing Python code that does the actual work.


Notes
-----
The generated agent will reference the file "common.soar", so please make
sure it's in the same directory or edit the first line of the generated
agent file.

Generated agents will create two WMEs on the state - (<S1> ^facts <F1>)
and (<S1> ^gs <G1>). All static relations in the game are stored under
the facts WME, while dynamic relations are stored under the gs (game
state) WME.

A GDL relation (on A B) will take the following form in working memory:

(<G1> ^on <O1>)
(<O1> ^p1 A ^p2 B)

Most of the kif files on games.stanford.edu were written for
backward-chaining systems. This means that when deducing the truths of
relations based on the game rules, many unnecessary intermediate relations
are automatically ignored. On the other hand, the generated Soar agents
calculate truth values in a forward-chaining style, preventing them from
pruning unnecessary intermediate relations. Many of the agents generated
from these kif files will experience explosions in working memory
and partial production match sizes, and will slow to a crawl or crash
completely after running out of system memory. If you experience these
symptoms, you need to do two things:

1. Modify the GDL rules so that each inference is as constrained as
   possible.

2. Use the multi-attributes command to tell Soar about relations that
   still have a large number of instances so that the RETE delays matching
   on them until other constraints are in place.

We've included an example of how to modify the mummy maze game to run
tractably in the kifs/ directory.
