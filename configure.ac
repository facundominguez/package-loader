#
# Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)
#

# SUBSTs go into config.mk.in (TOP,PREFIX,GHC,INSTAlL)
# AC_DEFINES go into config.h
# only AC_DEFINES are visible in Haskell src

# sanity test
AC_INIT(src/System/Loader/Package.hs)

# Prepare to generate the config.h.in file
AC_CONFIG_HEADERS([config.h])

# untested on earlier than 2.52, but it won't work anyway
AC_PREREQ(2.53)

# Find out what type of system we're running on
AC_CANONICAL_BUILD

Platform="$build_cpu-$build_vendor-$build_os"

# This isn't right for msys mingw.
PREFIX="$prefix"
if test "$prefix" = "NONE"
then
  if test "$build_os" = "cygwin"
  then
    PREFIX="c:/cygwin$ac_default_prefix"
  else
    PREFIX="$ac_default_prefix"
  fi
fi
AC_SUBST(PREFIX)

case $Platform in
*-apple-darwin*)
        MACOSX=yes
        AC_DEFINE_UNQUOTED(MACOSX, [1], [Defined if compiling on the mac])
        LEADING_UNDERSCORE=1
	;;
*)
        MACOSX=no
        LEADING_UNDERSCORE=0
	;;
esac

if test "$build_os" = "mingw32"
then
  AC_DEFINE(__MINGW32__,[1],[Defined if compiling with mingw])
  LEADING_UNDERSCORE=1
fi

if test "$build_os" = "cygwin"
then
  AC_DEFINE(CYGWIN,[1],[Defined if compiling with mingw])
  LEADING_UNDERSCORE=1
fi

AC_DEFINE_UNQUOTED(LEADING_UNDERSCORE, $LEADING_UNDERSCORE, 
                [Whether symbols are prefixed with a leading underscore])

# Allow a debugging version of package-loader to be built
AC_ARG_ENABLE(debug,
   [  --enable-debug          Enable a debug version of hs-plugins to be built],
   [ 
     AC_DEFINE_UNQUOTED(DEBUG, [1], [Defined if a debugging version is to be built]) ],
   [ : ])

AC_PROG_INSTALL

# System.Process uses cmd or command.com shell on Windows, doen't use Cygwin or MSYS shell
AC_OUTPUT

