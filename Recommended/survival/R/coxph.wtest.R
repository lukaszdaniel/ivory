#
# A Wald test routine, used by the Cox model
#  Why not just do  sum(b * solve(var, b))? -- because the solve
#  function chokes on singular matrices.
#
coxph.wtest <- function(var, b, toler.chol=1e-9) {
    if (is.matrix(b)) {
        nvar <- nrow(b)
        ntest <- ncol(b)
        }
    else {
        nvar <- length(b)
        ntest <- 1
        }
    
    if (length(var) == 0) { #special case added by Tom Lumley
	if (nvar == 0) return(list(test=numeric(0), df=0, solve=0))
	else stop(gettextf("'%s' and '%s' arguments are of different lengths", "var", "b"))
	}

    if (length(var) == 1) {
        if (nvar == 1) return(list(test=b*b/var, df=1, solve=b/var))
        else stop(gettextf("'%s' and '%s' arguments are of different lengths", "var", "b"))
        }

    if (!is.matrix(var) || (nrow(var) != ncol(var)))
            stop("'var' argument is not a square matrix")
    if (nrow(var) != nvar) stop(gettextf("'%s' and '%s' arguments are of different lengths", "var", "b"))

    if (any(!is.finite(b)) || any(!is.finite(var))) {
        cat(gettext("not finite in coxph.wtest", domain = "R-survival"), "\n", sep = "")
        print(b)
        print(var)
        }
    temp <- .C(Ccoxph_wtest, df=as.integer(nvar),
                              as.integer(ntest),
                              as.double(var),
                              tests= as.double(b),
                              solve= double(nvar*ntest),
	                      as.double(toler.chol))
    if (ntest == 1) list(test=temp$tests[1], df=temp$df, solve=temp$solve)
    else          list(test=temp$tests[seq_len(ntest)], df=temp$df, 
                       solve=matrix(temp$solve, nvar, ntest))
    }
