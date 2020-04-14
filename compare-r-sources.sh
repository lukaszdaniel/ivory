#!/bin/bash


source conf-names.sh

X=$RECDEVEL
Y=$RECBASE

while getopts ":cs" opt; do
  case $opt in
    c)
      X=$DEVEL
      Y=$BASE
      ;;
    s) simple="1";;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done


if test $X = $DEVEL; then
   source svn-cleanup.sh $X
else
   rm -r $RECDEVEL/* &> /dev/null
   for file in `ls $DEVEL/src/library/Recommended/*.tar.gz`; do
     tar -xf $file -C $RECDEVEL
   done
fi;

if test "${simple}" != "1"; then
LC_ALL=C diff -qr --exclude=".svn" --exclude=".git" --exclude="translations" $X $Y | sort > $RANDSTRING
if test `wc -l $RANDSTRING | cut -d" " -f1` -ne 0; then
   sed -n -e "s/Files \(.*\)* .*/o \1/p" $RANDSTRING | cut -d" " -f1,2
   sed -n -e "s/Only in $X\/\([^: ]*\): \(.*\)/+ $X\/\1\/\2/p" $RANDSTRING | cut -d" " -f1,2
   sed -n -e "s/Only in $Y\/\([^: ]*\): \(.*\)/- $Y\/\1\/\2/p" $RANDSTRING | cut -d" " -f1,2
else
   echo "'"$X"' and '"$Y"' are identical"
fi;
   rm $RANDSTRING
else
  LC_ALL=C diff -qr --exclude=".svn" --exclude=".git" --exclude="translations" $X $Y | sort | sed -n -e "s/Files \(.*\)* and .*/ \1\\\\/p"
fi;
