#!/bin/bash

source conf-names.sh

cd $IVORY

echo "# List of source files containing translatable strings." > po/POTFILES
echo "# The last line must not be a comment." >> po/POTFILES
echo "" >> po/POTFILES

filelist=`ls --hide=library -R src | awk '/:$/&&f{s=$0; f=0}/:$/&& !f{ sub(/:$/,""); s=$0; f=1; next} NF && f{ print s"/"$0 }' | egrep 'c$|h$|y$'`;
# przykladowy wynik: src/main/edit.c
for file in $filelist; do

 if test -f $file; then
  scan=`cat $file | egrep -v '__\(' | egrep 'R_Suicide\(|_\("|n_\(|gettext\("' | wc -l`
  if test $scan -gt 0; then
    echo $file >> po/POTFILES
  fi;
 fi;

done;
