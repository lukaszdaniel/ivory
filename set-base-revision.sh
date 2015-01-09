#!/bin/bash

#echo "Revision:" $1 > R/BASE-REVISION
#echo "Last Changed Date:" `date +%F` >> R/BASE-REVISION
DEVEL="R-devel"
IVORY="R"

if [[ `LC_ALL=C TZ=GMT svn info $DEVEL | sed -n -e '/^Revision/p'  | cut -d' ' -f2` == $1 ]]; then
LC_ALL=C TZ=GMT svn info $DEVEL | sed -n -e '/^Revision/p' -e '/^Last Changed Date/'p  | cut -d' ' -f1,2,3,4 > $IVORY/BASE-REVISION
else echo "Numer rewizji nie zgadza sie"
fi;
