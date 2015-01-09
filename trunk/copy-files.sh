#!/bin/bash

source conf.sh


  cp -f $filedev $filefroz
  cp -f $filedev $filemine

  if test `svn status $filemine | awk '{ print $1 }'` = "?"; then
   svn add $filemine
  fi;
