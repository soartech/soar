#!/bin/sh
#
# Runs a specified cross functional test with two C codec components and
# one Lisp codec component. The result is printed to the standard output.
#
# For running cross tests the C codec has to be compiled!
#
# $Revision: 485 $
# $Date: 2009-01-31 08:57:54 -0500 (Sat, 31 Jan 2009) $

if [ ${#} -ne 3 ]; then
    echo "Usage: ${0} <lisp-implementation> <testcase-name> <lisp-component>"
    echo "Example: ${0} sbcl test-empty environment"
    exit -1
fi

tooldir="`dirname ${0}`"
source "${tooldir}/common.sh"

lispimpl="${1}"
testname="${2}"
lispcomp="${3}"

check_test_component_name ${lispcomp}

###############################################################################

load_lisp_config ${lispimpl}
load_functional_test_config ${testname}

c_test_dir="${basedir}/../C/tests"

###############################################################################

execute_rl_glue

if [ ${lispcomp} = "agent" ]; then
{
${LISP} <<- EOF
  `lisp_init`
  `lisp_load_asdf :rl-glue-tests`
  (rl-glue-tests:start-${AGENT})
  `lisp_quit`
EOF
} &
else
    cd ${c_test_dir} &&
    make ${C_AGENT} &&
    ${c_test_dir}/${C_AGENT} &
fi

if [ ${lispcomp} = "environment" ]; then
{
${LISP} <<- EOF
  `lisp_init`
  `lisp_load_asdf :rl-glue-tests`
  (rl-glue-tests:start-${ENVIRONMENT})
  `lisp_quit`
EOF
} &
else
    cd ${c_test_dir} &&
    make ${C_ENVIRONMENT} &&
    ${c_test_dir}/${C_ENVIRONMENT} &
fi

if [ ${lispcomp} = "experiment" ]; then
{
${LISP} <<- EOF
  `lisp_init`
  `lisp_load_asdf :rl-glue-tests`
  (rl-glue-tests:start-${EXPERIMENT})
  `lisp_quit`
EOF
}
else
    cd ${c_test_dir} &&
    make ${C_EXPERIMENT} &&
    ${c_test_dir}/${C_EXPERIMENT}
fi

exit 0

