#!/bin/bash

source conf.sh

  rm -f $filefroz
  if test `echo $filemine | grep "@"`; then
   filemine=`echo $filemine | sed  -e 's/$/@/'`
  fi;
  svn delete $filemine
