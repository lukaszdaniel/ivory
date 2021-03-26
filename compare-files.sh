#!/bin/bash

source conf.sh

converted=('appl' 'gnuwin32' 'library' 'main' 'modules' 'nmath' 'unix')

for ix in ${!filedev[@]}
do
    cnvtd=0;

    for ic in ${converted[@]}; do
        if [[ "${filedev[$ix]}" =~ ${ic} ]]; then
            cnvtd=1;
        fi;
    done;

    if [[ ${filedev[$ix]##*\.} == "c" ]] && [[ "${cnvtd}" == "1" ]]; then
        meld ${filedev[$ix]} ${filefroz[$ix]} ${filemine[$ix]}"pp"
    else
        meld ${filedev[$ix]} ${filefroz[$ix]} ${filemine[$ix]}
    fi;
done;
