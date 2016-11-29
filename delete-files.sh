#!/bin/bash

source conf.sh

for ix in ${!filedev[@]}
do
  rm -rf ${filefroz[$ix]}
  if test `echo ${filemine[$ix]} | grep "@"`; then
   filemine[$ix]=`echo ${filemine[$ix]} | sed  -e 's/$/@/'`
  fi;
  svn --force delete ${filemine[$ix]}
done;
