#!/bin/bash

source conf-names.sh


  case $1 in
  $DEVEL)    W=$DEVEL ;;
  $IVORY)    W=$IVORY ;;
  $RECIVORY) W=$RECIVORY     ;;
  *) echo "Invalid argument" && exit 1 ;;
  esac

  cd $W

  svn status --no-ignore | egrep '^[?I]' | cut -c9- | xargs -d \\n rm -r &> /dev/null
  if test $W = $DEVEL; then
   tools/rsync-recommended
  fi;
  cd ..
  svn update $W
