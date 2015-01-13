#!/bin/bash


source conf-names.sh

X=$RECDEVEL
Y=$RECIVORY


while getopts ":cp:" opt; do
  case $opt in
    c)
      X=$DEVEL
      Y=$IVORY
      ;;
    p)
      RECPKGS=$OPTARG;
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done


if [[ $X == $DEVEL ]]; then
   source svn-cleanup.sh $X
   source svn-cleanup.sh $Y
else
   rm -r $X/*
   for file in `ls $DEVEL/src/library/Recommended/*.tar.gz`; do
     tar -xf $file -C $X
   done
fi;


if [[ $X == $DEVEL ]]; then
 LC_ALL=C diff -rEZbwB '--exclude=*.[lo,d,pdf,mo,po,tgz,gz,save]*' '--exclude=MD5' '--exclude=Makefile*' '--exclude=all.R' '--exclude=ChangeLog' $X $Y &> $Y/ChangeLog
else
 for pkg in $RECPKGS; do
 echo "  Package" $pkg "..."
 LC_ALL=C diff -rEZbwB '--exclude=*.[lo,d,pdf,mo,po,tgz,gz,save]*' '--exclude=MD5' '--exclude=Makefile*' '--exclude=all.R' '--exclude=Changes' $X/$pkg $Y/$pkg &> $Y/$pkg/Changes
 done;
fi;
