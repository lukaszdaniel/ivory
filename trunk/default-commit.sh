#!/bin/bash

source conf-names.sh

LC_ALL=C TZ=GMT 
rev=`cat $IVORY/BASE-REVISION | sed -n -e '/^Revision/p'  | cut -d' ' -f2`
message="Updated to R-devel revision $rev."

cd ..
svn commit -m "$message" ivory
svn update ivory
