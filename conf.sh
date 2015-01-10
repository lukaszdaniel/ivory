#!/bin/bash

source conf-names.sh

if test ! -d $DEVEL; then exit 1; fi;
if test ! -d $BASE;  then exit 1; fi;
if test ! -d $IVORY; then exit 1; fi;


if test ! -d $RECDEVEL; then exit 1; fi;
if test ! -d $RECBASE;  then exit 1; fi;
if test ! -d $RECIVORY; then exit 1; fi;


if test `echo $1 | grep "$DEVEL\/"`; then

  X=$DEVEL
  Y=$BASE
  Z=$IVORY

elif test `echo $1 | grep "$RECDEVEL\/"`; then

  X=$RECDEVEL
  Y=$RECBASE
  Z=$RECIVORY

else
 echo "Path should start with '"$DEVEL"' or '"$RECDEVEL"'" && exit 1
fi;

  filedev=$1;
  filefroz=`echo $1 | sed -e "s/$X/$Y/"`;
  filemine=`echo $1 | sed -e "s/$X/$Z/"`;
