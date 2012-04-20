#!/bin/sh
#
# Runs the unit tests of the specified package with a selected Lisp
# implementation and prints the result to the standard output.
#
# $Revision: 485 $
# $Date: 2009-01-31 08:57:54 -0500 (Sat, 31 Jan 2009) $

if [ ${#} -ne 2 ]; then
    echo "Usage: ${0} <lisp-implementation> <package>"
    echo "Example: ${0} sbcl rl-glue-utils"
    exit -1
fi

tooldir="`dirname ${0}`"
source "${tooldir}/common.sh"

lispimpl="${1}"
package="${2}"

###############################################################################

load_lisp_config ${lispimpl}

###############################################################################

${LISP} <<- EOF
  `lisp_init`
  (handler-bind ((condition #'(lambda (condition)
                                (continue))))
    (asdf:oos 'asdf:load-op :${package} :verbose nil)
    (asdf:oos 'asdf:load-op :${package}-tests :verbose nil))
  (fiveam:run! '${package}::main-suite)
  `lisp_quit`
EOF

exit 0

