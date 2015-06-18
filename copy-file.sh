#!/bin/bash

source conf.sh

for ix in ${!filedev[@]}
do
  cp -rf $filedev $filefroz
  cp -rf $filedev $filemine

  cmd=`svn status $filemine | sed -n -e "/^?/p"`

  if test -n "$cmd"; then
   svn add $filemine
  fi;
done;
