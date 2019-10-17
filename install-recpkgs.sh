#!/bin/bash

source conf-names.sh



git clean -fdx $RECIVORY

cd $RECIVORY

if test -n "$1"; then
 RECPKGS=$1
fi;

 for pkg in $RECPKGS; do
  cd $pkg
  version=`sed -n -e '/Version:/p' DESCRIPTION | cut -d" " -f2`
  rm MD5
  #md5deep -r -l * > MD5
  find -type f -print0 | xargs -0 md5sum | sed -e 's/ .\//*/' > ../MD5
  mv ../MD5 .
  cd ..
  if test -f ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz; then
   echo "Updating package '"${pkg}"' version" ${version}
   tar -czf ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz ${pkg}
  else
   echo "Adding package '"${pkg}"' version" ${version}
   git rm ../${IVORY}/src/library/Recommended/${pkg}_*.tar.gz
   git rm ../${IVORY}/src/library/Recommended/${pkg}.tgz
   tar -czf ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz ${pkg}
   git add ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz
   ln -sf ../${IVORY}/src/library/Recommended/${pkg}_${version}.tar.gz ../${IVORY}/src/library/Recommended/${pkg}.tgz
   git add ../${IVORY}/src/library/Recommended/${pkg}.tgz
  fi;
 done

 cd ../${IVORY}/src/library/Recommended
  for i in ${RECPKGS} ; do
    ln -sf $i*.tar.gz ${i}.tgz
  done
