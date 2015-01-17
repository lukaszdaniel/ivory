#!/bin/bash

source conf-names.sh



source svn-cleanup.sh $RECIVORY
cd $RECIVORY
svn status --no-ignore | egrep '^[?I]' | cut -c9- | xargs -d \\n rm -r &> /dev/null

if [ "$1" != "" ]; then
 RECPKGS=$1
fi;

 for pkg in $RECPKGS; do
  cd $pkg
  rm MD5
  md5deep -r -l * > MD5
  cd ..
  package=`echo $pkg | sed -e 's/\///g'`;
  targz=`find ../$IVORY/src/library/Recommended/$package*.tar.gz`
  tar=`echo $targz | sed -e 's/\.gz//'`
  echo "Updating" $targz
  gunzip -f $targz
  tar --delete -f $tar $pkg
  tar rf $tar $pkg
  gzip $tar
 done
cd ..
 source $IVORY/tools/link-recommended


