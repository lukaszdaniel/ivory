#!/bin/bash

source conf.sh

for ix in ${!filedev[@]}
do
 thesame=`diff -rEZbwB ${filefroz[$ix]} ${filemine[$ix]} | wc -l`
 if [[ ${thesame} == 0 ]]; then
  cp -rf ${filedev[$ix]} ${filefroz[$ix]}
  cp -rf ${filedev[$ix]} ${filemine[$ix]}

  cmd=`git status --porcelain ${filemine[$ix]} | sed -n -e "/^?/p"`

  if test -n "$cmd"; then
   git add ${filemine[$ix]}
  fi;
 fi;
done;
