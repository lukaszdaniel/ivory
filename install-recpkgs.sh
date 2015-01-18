#!/bin/bash

source conf-names.sh



source svn-cleanup.sh $RECIVORY
cd $RECIVORY
svn status --no-ignore | egrep '^[?I]' | cut -c9- | xargs -d \\n rm -r &> /dev/null

if test -n "$1"; then
 RECPKGS=$1
fi;

 for pkg in $RECPKGS; do
  cd $pkg
  version=`sed -n -e '/Version:/p' DESCRIPTION | cut -d" " -f2`
  rm MD5
  md5deep -r -l * > MD5
  cd ..
  if test -f ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz; then
   echo "Updating package '"${pkg}"' version" ${version}
   tar -czf ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz ${pkg}
  else
   echo "Adding package '"${pkg}"' version" ${version}
   svn delete ../${IVORY}/src/library/Recommended/${pkg}_*.tar.gz
   tar -czf ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz ${pkg}
   svn add ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz
  fi;
 done

 cd ../${IVORY}/src/library/Recommended
  for i in ${RECPKGS} ; do
    ln -sf $i*.tar.gz ${i}.tgz
  done
