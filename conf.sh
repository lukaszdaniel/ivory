#!/bin/bash

source conf-names.sh

if test ! -d $DEVEL; then exit 1; fi;
if test ! -d $BASE;  then exit 1; fi;
if test ! -d $IVORY; then exit 1; fi;


if test ! -d $RECDEVEL; then exit 1; fi;
if test ! -d $RECBASE;  then exit 1; fi;
if test ! -d $RECIVORY; then exit 1; fi;

filedev=""
filefroz=""
filemine=""

for ix in $@; do
 if test `echo $ix | grep "$DEVEL\/"` || test `echo $ix | grep "$BASE\/"`; then

  X=$DEVEL
  Y=$BASE
  Z=$IVORY

 elif test `echo $ix | grep "$RECDEVEL\/"` || test `echo $ix | grep "$RECBASE\/"`; then

  X=$RECDEVEL
  Y=$RECBASE
  Z=$RECIVORY

 else
  echo "Path should start with '"$DEVEL"' or '"$RECDEVEL"' or '"$BASE"' or '"$RECBASE"'" && exit 1
 fi;

 filedev+=" "
 filefroz+=" "
 filemine+=" "

  filedev+=$ix;
  filefroz+=`echo $ix | sed -e "s/[^\/]*\//$Y\//"`;
  filemine+=`echo $ix | sed -e "s/[^\/]*\//$Z\//"`;
done;

 filedev=(${filedev// / })
 filefroz=(${filefroz// / })
 filemine=(${filemine// / })
