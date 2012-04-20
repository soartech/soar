#/bin/sh
#
# Runs a random cross test of each functional test type with the selected lisp
# implementation. If it is not specified, the full round is run with all the
# supported and available ones. It prints the result and a summary to the
# standard output and into a log file as well.
#
# $Revision: 484 $
# $Date: 2009-01-31 08:39:52 -0500 (Sat, 31 Jan 2009) $

if [ ${#} -gt 1 ]; then
    echo "Usage: ${0} [<lisp-implementation>]"
    exit -1
fi

tooldir="`dirname ${0}`"
source "${tooldir}/common.sh"

lispimpl=""
if [ ${#} -eq 1 ]; then
    lispimpl="${1}"
fi

###############################################################################

LOGFILE=`open_log_file "run-random-cross-tests.log"`

{
    for c in `ls "${tooldir}/config/lisp-${lispimpl}"*`; do
        l=`basename ${c} | cut -d'-' -f2-`
        echo -e "\nFT> ---- CONFIG : ${l}\n"
        source "${c}"

        if [ -z "`which ${LISPBIN} 2>/dev/null`" ]; then
            echo "FT> Lisp binary is not available: ${LISPBIN}."
            continue
        fi

        for t in `ls -d "${tooldir}/../test/functional/test-"*`; do
            if [ -d "${t}" ]; then
                lispcomp=
                let r="${RANDOM} % 3"
                case ${r} in
                    0 ) lispcomp="agent"; ;;
                    1 ) lispcomp="environment"; ;;
                    2 ) lispcomp="experiment"; ;;
                esac

                echo " ---- TEST : ${t} (${lispcomp})"
                ${tooldir}/run-cross-test.sh ${l} `basename ${t}` ${lispcomp}
            fi
        done
    done
} 2>&1 | tee -a ${LOGFILE}

###############################################################################

TMPLOGFILE="${LOGFILE}.tmp"
cp ${LOGFILE} ${TMPLOGFILE}
{
    echo -en "\n----------------------------------------"
    echo -en "----------------------------------------\n"
    grep -P '(FT>|Passed|Failed|elapsed time)' ${TMPLOGFILE}
    echo -en "----------------------------------------"
    echo -en "----------------------------------------\n\n"
} 2>&1 | tee -a ${LOGFILE}
rm ${TMPLOGFILE}

exit 0

