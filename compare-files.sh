#!/bin/bash

source conf.sh

for ix in ${!filedev[@]}
do
  meld ${filedev[$ix]} ${filefroz[$ix]} ${filemine[$ix]}
done;
