#!/bin/bash
#
# Creating an ASDF tarball of a source directory in the rl-glue Lisp codec
# src path by omitting the .svn directories.
#
# $Revision: 410 $
# $Date: 2008-12-25 10:15:44 -0500 (Thu, 25 Dec 2008) $

if [ ${#} != 1 ]; then
    echo "Usage: ${0} <asdf-system-name>"
    echo "Example: ${0} rl-glue-codec"
    exit -1
fi

tooldir="`dirname ${0}`"
package="${1}"

###############################################################################

function make_asdf_package {
    local system=${1}
    echo -n "Creating package ${system}.tar.gz ... "
    tar -zcf ../${system}.tar.gz --exclude '.svn/*' --exclude '.svn' ${system}
    if [ ${?} = 0 ]; then
        echo "done"
    else
        echo "failed (${?})"
    fi
}

###############################################################################

cd ${tooldir}/../src
make_asdf_package ${package}
cd - >/dev/null

exit 0

