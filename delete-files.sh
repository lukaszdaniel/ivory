#!/bin/bash

source conf.sh

  rm -rf $filefroz
  if test `echo $filemine | grep "@"`; then
   filemine=`echo $filemine | sed  -e 's/$/@/'`
  fi;
  svn --force delete $filemine
