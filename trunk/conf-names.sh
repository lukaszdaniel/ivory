#!/bin/bash

#
# DEVEL - original R sources taken from r-cran
# BASE  - source code which Ivory is based on
# IVORY - Ivory source code
#
DEVEL="R-devel";
BASE="R-devel-frozen";
IVORY="R";

RECDEVEL="R-recommended";
RECBASE="R-recommended-frozen";
RECIVORY="Recommended";

BASEPKGS="base compiler graphics grDevices grid methods parallel splines stats stats4 tcltk tools utils"
RECPKGS="boot class cluster codetools foreign KernSmooth lattice MASS Matrix mgcv nlme nnet rpart spatial survival"

# terminal colors
txtrst=$(tput sgr0)    # Text reset
txtred=$(tput setaf 1) # Red
txtgrn=$(tput setaf 2) # Green
txtylw=$(tput setaf 3) # Yellow
txtblu=$(tput setaf 4) # Blue
txtpur=$(tput setaf 5) # Purple
txtcyn=$(tput setaf 6) # Cyan
txtwht=$(tput setaf 7) # White
txtrst=$(tput sgr0)    # Text reset.

RANDSTRING=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 6 | head -n 1)
