#!/bin/bash

#
# DEVEL - original R sources taken from r-cran
# BASE  - source code which Ivory is based on
# IVORY - Ivory source code
#
DEVEL="R-devel";
BASE="R-devel-frozen";
IVORY="R";

#TOOLS_DIR=`echo ${0} | sed 's,/[^/][^/]*$,,'`
#if test ${TOOLS_DIR} = ${0} ; then
#  TOOLS_DIR="."
#fi

#cd "${TOOLS_DIR}"/..


if test ! -d $DEVEL; then exit 1; fi;
if test ! -d $BASE;  then exit 1; fi;
if test ! -d $IVORY; then exit 1; fi;

# kolory
txtrst=$(tput sgr0)    # Text reset
txtred=$(tput setaf 1) # Red
txtgrn=$(tput setaf 2) # Green
txtylw=$(tput setaf 3) # Yellow
txtblu=$(tput setaf 4) # Blue
txtpur=$(tput setaf 5) # Purple
txtcyn=$(tput setaf 6) # Cyan
txtwht=$(tput setaf 7) # White
txtrst=$(tput sgr0)    # Text reset.

RECDEVEL="R-recommended";
RECBASE="R-recommended-frozen";
RECIVORY="Recommended";

if test ! -d $RECDEVEL; then exit 1; fi;
if test ! -d $RECBASE;  then exit 1; fi;
if test ! -d $RECIVORY; then exit 1; fi;

RECPKGS="boot class cluster codetools foreign KernSmooth lattice MASS Matrix mgcv nlme nnet rpart spatial survival"

if test `echo $1 | grep "$DEVEL\/"`; then

  X=$DEVEL
  Y=$BASE
  Z=$IVORY

elif test `echo $1 | grep "$RECDEVEL\/"`; then

  X=$RECDEVEL
  Y=$RECBASE
  Z=$RECIVORY

else
 echo "Error!" && exit 1
fi;

  filedev=$1;
  filefroz=`echo $1 | sed -e "s/$X/$Y/"`;
  filemine=`echo $1 | sed -e "s/$X/$Z/"`;
