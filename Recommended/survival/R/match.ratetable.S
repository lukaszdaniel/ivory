# Do a set of error checks on whether any categorical vars match the
#   level set of the actual ratetable.  If so they are mapped to the levels
#   found in the ratetable.  Dates need to match dates, and others are set
#   to simple numerics with unclass().  A matrix is returned.
# This is called by pyears and survexp, but not by users
#
# The categoricals are turned into integer subscripts
#
match.ratetable <- function(R, ratetable) {
    if (!is.ratetable(ratetable)) stop("invalid rate table")
    dimid <- attr(ratetable, 'dimid')

    if (is.matrix(R)) {  # older style call
        nd <- ncol(R)
        attR <- attributes(R)
        attributes(R) <- attR['dim']     #other attrs get in the way later
        Rnames <- attR$dimnames[[2]]
        isDate <- attR[["isDate"]]
        levlist <- attR[['levlist']]
        }
    else {  # newer style is a dataframe
        nd <- length(R)
        Rnames <- names(R)
        isDate <- rep(FALSE, nd)
        levlist<- lapply(R, levels)
        for (i in seq_len(nd)) {
            temp <- ratetableDate(R[[i]])
            if (!is.null(temp)) {
                isDate[i] <- TRUE
                R[[i]] <- temp
                }
            }
        }
        
    ord <- match(dimid, Rnames)

    # This should not arise
    if (any(is.na(ord)))
       stop(gettextf("'%s' argument needed by the ratetable was not found in the data", dimid[is.na(ord)]))
    # Neither should this -- two argments matched one of the dimids -- since
    #  I demand an exact match
    if (any(duplicated(ord)))
        stop("'ratetable' argument appears twice in the data")
    
    R <- R[,ord,drop=FALSE]  #put cols in same order as the ratetable
    isDate <- isDate[ord]
    levlist <- levlist[ord]

    dtemp <-dimnames(ratetable)
    rtype  <- attr(ratetable, 'type') # 1= class, 2=cont, 3=date, 4=US yr
    if (is.null(rtype)) { #old style ratetable, be backwards compatable
        temp <- attr(ratetable, 'factor')
        # we map 'old continuous' to 'new date'; since it might be a date
        rtype <- 1*(temp==1) + 3*(temp==0) + 4*(temp >1)  
        }

    # Now, go through the dimensions of the ratetable 1 by 1, and
    #  verify that the user's variable is compatable
    #  with the rate table's dimensions
    #
    if (any(rtype<3 & isDate)) {
        indx <- which(rtype<1 & isDate)
        stop(gettextf("data has a date type variable, but the reference ratetable is not a date for variable %s", dimid[indx]))
        }
    for (i in seq_len(nd)) {
	if (length(levlist[[i]]) >0) {  #factor or character variable
	    if (rtype[i]!=1) stop(gettextf("In 'ratetable()', %s must be a continuous variable", dimid[i]))
	    temp <- charmatch(casefold(levlist[[i]]), casefold(dtemp[[i]]))
	    if (any(is.na(temp)))
		stop(gettextf("levels do not match for 'ratetable()' variable %s", dimid[i]))
            if (any(temp==0)) 
                stop(gettextf("non-unique ratetable match for variable %s", dimid[i]))
	    R[,i] <- temp[as.numeric(R[,i])]
	    }

	else {   # user's data isn't a factor or date
            R[,i] <- unclass(R[,i])  # get rid of difftimes & other such
	    if (rtype[i]==1) {   #ratetable is a factor: ok if data is integer
		temp <- R[,i]
		if (any(floor(temp)!=temp) || any(temp<=0) || max(temp) > length(dtemp[[i]]))
		stop(gettextf("the variable %s is out of range", dimid[i]))
		}
	    }
	}
    R <- as.matrix(R)

    summ <- attr(ratetable, 'summary')
    if (is.null(summ))
	 list(R= R)
    else list(R= R, summ=summ(R))
    }
