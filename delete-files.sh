#!/bin/bash

source conf.sh

for ix in ${!filedev[@]}
do
  rm -rf ${filefroz[$ix]}
  if test `echo ${filemine[$ix]} | grep "@"`; then
   filemine[$ix]=`echo ${filemine[$ix]} | sed  -e 's/$/@/'`
  fi;
  git rm --force ${filemine[$ix]}
done;
