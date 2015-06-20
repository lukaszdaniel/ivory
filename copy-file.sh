#!/bin/bash

source conf.sh

for ix in ${!filedev[@]}
do
  cp -rf ${filedev[$ix]} ${filefroz[$ix]}
  cp -rf ${filedev[$ix]} ${filemine[$ix]}

  cmd=`svn status ${filemine[$ix]} | sed -n -e "/^?/p"`

  if test -n "$cmd"; then
   svn add ${filemine[$ix]}
  fi;
done;
