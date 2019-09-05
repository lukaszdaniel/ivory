#!/bin/bash

source conf.sh

for ix in ${!filedev[@]}
do
  cp -rf ${filedev[$ix]} ${filefroz[$ix]}
  cp -rf ${filedev[$ix]} ${filemine[$ix]}

  cmd=`git status --porcelain ${filemine[$ix]} | sed -n -e "/^?/p"`

  if test -n "$cmd"; then
   git add ${filemine[$ix]}
  fi;
done;
