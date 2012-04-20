AC_INIT(C-Codec,2.0)
AC_CONFIG_SRCDIR(src/RL_client_agent.c)
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE([1.9 tar-ustar])

AC_MSG_CHECKING(if custom rl-glue path was specified)
AC_ARG_WITH(rl-glue,
    [  --with-rl-glue=PATH     look for rl-glue in PATH (should be an absolute path) ],
    [LDFLAGS="$LDFLAGS -L$withval/lib"
	CFLAGS="$CFLAGS -I$withval/include"
     CPPFLAGS="$CPPFLAGS -I$withval/include"
	AC_MSG_RESULT(yes)
	],[AC_MSG_RESULT(no)])


AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AM_PROG_LIBTOOL
AM_MAINTAINER_MODE


if test "${TERM}" = "cygwin"; then
dnl Cygwin will not let us link rlgluenetdev without rlutils so we will do a lesser check
	AC_CHECK_LIB([rlutils], [clearRLStruct],[], AC_MSG_ERROR(RL-Glue utils library not found.  This probably means you do not have RL-Glue installed.  You must have RL-Glue installed to use this codec. If you have not downloaded it please see http://glue.rl-community.org/ .  If you do have it installed in a non-standard location you may need to use the --with-rl-glue=/path/to/rlglue command line switch to specify where the rl-glue root is located.))
else
dnl Check to make sure we can find the rlglue libraries and headers.  If no, then tell them how to tell us where they are.
	AC_CHECK_LIB([rlgluenetdev], [rlConnect],[], AC_MSG_ERROR(RL-Glue library not found. You must have RL-Glue installed to use this codec. If you have not downloaded it please see http://glue.rl-community.org/ .  If you do have it installed in a non-standard location you may need to use the --with-rl-glue=/path/to/rlglue command line switch to specify where the rl-glue root is located.),-lrlutils)
fi


AC_CHECK_HEADERS([rlglue/RL_common.h rlglue/Agent_common.h rlglue/Environment_common.h rlglue/RL_glue.h rlglue/network/RL_network.h],[] ,AC_MSG_ERROR(RL-Glue library not found. You must have RL-Glue installed to use this codec. If you have not downloaded it please see http://glue.rl-community.org/ .  If you do have it installed in a non-standard location, you may need to use the --with-rl-glue=/path/to/rlglue command line switch to specify where the rl-glue root is located.),[[#ifdef HAVE_RLGLUE_RL_COMMON_H
# include <rlglue/RL_common.h>
#endif
]])

AC_CHECK_HEADERS([ieeefp.h, sys/time.h])
AC_CHECK_FUNCS([isnan isinf gettimeofday gethostbyname inet_ntoa])


dnl For GCC 4.3, apparently we need to do something a bit fancy
AC_CHECK_MEMBERS([struct hostent.h_addr_list],,,[#include <netdb.h>])

AC_CONFIG_HEADERS(config/config.h)




AC_OUTPUT(Makefile src/Makefile tests/Makefile)
