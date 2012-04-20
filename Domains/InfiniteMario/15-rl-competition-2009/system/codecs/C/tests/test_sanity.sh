#!/bin/bash

# *  $Revision: 322 $
# *  $Date: 2008-10-08 12:56:23 -0400 (Wed, 08 Oct 2008) $
# *  $Author: brian@tannerpages.com $
# *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_sanity.sh $


killall rl_glue 2>/dev/null
killall test_1_agent 2>/dev/null
killall test_1_environment 2>/dev/null
killall test_sanity_experiment 2>/dev/null
sleep 1
rl_glue &
sleep 1
./test_1_agent &
sleep 1
./test_1_environment &
sleep 1
./test_sanity_experiment

test_outcome=$?
exit $test_outcome