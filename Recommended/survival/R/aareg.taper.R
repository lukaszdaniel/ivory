#
# Do running averages of an information matrix
# 
aareg.taper <- function(taper, imat, nevent) {
    dd <- dim(imat)
    if (length(taper)==0 || !is.numeric(taper) || any(taper <=0)) 
        stop(gettextf("invalid '%s' argument", "taper"))

    ntaper <- length(taper)
    ntime <- dd[3]
    if (ntaper > ntime) {
        taper <- taper[seq_len(ntime)]
        ntaper <- ntime
        }

    #
    # Turn imat into an array: 1 row per coef, one col per time
    #  and then scale it by the number of events to get a variance
    # (coxph.detail returns imat = var(X) * nevents)
    #
    imat <- matrix(as.vector(imat), ncol=dd[3])
    imat <- imat / rep(nevent, rep(dd[1]*dd[2], dd[3]))

    if (ntaper >1) {
        smoother <- matrix(0., ntime, ntime)
        tsum <- cumsum(rev(taper))
        for (i in seq_len(ntaper)) 
            smoother[seq_len(i), i] <- taper[seq(to=ntaper, length=i)]/tsum[i]
        if (ntaper < ntime) {
            for (i in (ntaper+1):ntime)
                smoother[seq(to=i, length=ntaper),i] <- taper/tsum[ntaper]
            }
        imat <- imat %*% smoother
        }
    array(imat, dim=dd)
    }


