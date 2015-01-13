#!/bin/bash


source conf-names.sh

X=$RECDEVEL
Y=$RECBASE

while getopts ":c" opt; do
  case $opt in
    c)
      X=$DEVEL
      Y=$BASE
      ;;
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


LC_ALL=C diff -qr --exclude=".svn" --exclude="translations" $X $Y | sort > $RANDSTRING
if test `wc -l $RANDSTRING | cut -d" " -f1` -ne 0; then
   sed -n -e "s/Files \(.*\)* .*/o \1/p" $RANDSTRING | cut -d" " -f1,2
   sed -n -e "s/Only in $X\([^: ]*\): \(.*\)/+ $X\1\/\2/p" $RANDSTRING | cut -d" " -f1,2
   sed -n -e "s/Only in $Y\([^: ]*\): \(.*\)/+ $Y\1\/\2/p" $RANDSTRING | cut -d" " -f1,2
   rm $RANDSTRING
else
   echo "'"$X"' and '" $Y"' are identical"
fi;
