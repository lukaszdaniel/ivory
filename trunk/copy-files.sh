#!/bin/bash

source conf.sh


  cp -f $filedev $filefroz
  cp -f $filedev $filemine

  cmd=`svn status $filemine | sed -n -e "/^?/p"`

  if test -n "$cmd"; then
   svn add $filemine
  fi;
