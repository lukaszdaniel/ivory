### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat(gettext("Time elapsed: ", domain = "R-base"), proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
