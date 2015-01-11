#!/bin/bash

source conf-names.sh

  tidy=`rm tests/?? &> /dev/null;`

  case $1 in
  $DEVEL)    X=$DEVEL; $tidy ;;
  $IVORY)    X=$IVORY; $tidy ;;
  $RECIVORY) X=$RECIVORY     ;;
  *) echo "Invalid argument" && exit 1 ;;
  esac

  cd $X

  svn status --no-ignore | egrep '^[?I]' | cut -c9- | xargs -d \\n rm -r &> /dev/null
  if test $X = $DEVEL; then
   tools/rsync-recommended
  fi;
  cd ..
  svn update $X
