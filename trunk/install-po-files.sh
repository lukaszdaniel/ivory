#!/bin/bash

source conf-names.sh

while getopts ":c" opt; do
  case $opt in
    c)
      core=1
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done


if test -v core; then

 cd $IVORY/src/library

  for pkg in $BASEPKGS; do
   echo "Installing" $pkg;
    if test $pkg = "base" && test -f $pkg/po/RGui-pl.po; then
 	  msgfmt -c -o translations/inst/pl/LC_MESSAGES/RGui.mo $pkg/po/RGui-pl.po
    fi;
    if test -f $pkg/po/R-pl.po; then
 	  msgfmt -c -o translations/inst/pl/LC_MESSAGES/R-$pkg.mo $pkg/po/R-pl.po
    fi;
    if test $pkg = "base" && test -f $pkg/po/pl.po; then
 	  msgfmt -c -o translations/inst/pl/LC_MESSAGES/R.mo $pkg/po/pl.po;
    elif test -f $pkg/po/pl.po; then
 	  msgfmt -c -o translations/inst/pl/LC_MESSAGES/$pkg.mo $pkg/po/pl.po
    fi;
  done
  cp -u translations/inst/pl/LC_MESSAGES/*.mo ../../library/translations/pl/LC_MESSAGES/.
 cd ../../../

else

 cd $RECIVORY/

  for pkg in $RECPKGS; do
   echo "Installing" $pkg;
    if test -f $pkg/po/R-pl.po; then
 	  msgfmt -c -o $pkg/inst/po/pl/LC_MESSAGES/R-$pkg.mo $pkg/po/R-pl.po
    fi;
    if test -f $pkg/po/pl.po; then
 	  msgfmt -c -o $pkg/inst/po/pl/LC_MESSAGES/$pkg.mo $pkg/po/pl.po
    fi;
  cp -u $pkg/inst/po/pl/LC_MESSAGES/*.mo ../R/library/$pkg/po/pl/LC_MESSAGES/.
  done
fi;
