survSplit <- function(formula, data, subset, na.action=na.pass,
                              cut, start="tstart", id, zero=0, episode,
                              end, event) {
    Call <- match.call()

    if (missing(formula) || is.data.frame(formula)) {
        # an old style call
        # match arguments and build a formula
        if (is.data.frame(formula)) data <- formula
        else if (missing(data)) stop(gettextf("'%s' argument is required", "data"))

        if (missing(end) || missing(event))
            stop(gettextf("'%s' or '%s' and '%s arguments are required", "formula", "end", "event"))

        if (!(is.character(event) && length(event) ==1 &&
              event %in% names(data)))
            stop(gettextf("'%s' argument must be a variable name in the data set", "event"))

        if (!(is.character(end) && length(end) ==1 &&
              end %in% names(data)))
            stop(gettextf("'%s' argument must be a variable name in the data set", "end"))
    
        if (!(is.character(start) && length(start)==1))
            stop(gettextf("'%s' argument must be a variable name", "start"))
        if (start %in% names(data)) temp <- paste(start, end, event, sep=',')
        else temp <- paste(end, event, sep=',')
        
        formula <- as.formula(paste("Surv(", temp, ")"))
    }

    else if (missing(formula)) 
        stop(gettextf("'%s' or '%s' and '%s arguments are required", "formula", "end", "event"))

    # create a call to model.frame() that contains the formula (required)
    #  and any other of the relevant optional arguments
    # then evaluate it in the proper frame
    indx <- match(c("data", "weights", "subset"),
                  names(Call), nomatch=0) 
    temp <- Call[c(1L,indx)]  # only keep the arguments we wanted
    temp$formula <- formula
    temp$na.action <- na.action
    temp[[1L]] <- quote(stats::model.frame)  # change the function called
    mf <- eval.parent(temp)      

    Y <- model.response(mf)
    if (!is.Surv(Y)) stop ("the model must have an object of class \"Surv\" as the response")
    if (!(attr(Y, "type") %in% c("right", "mright", "counting", "mcounting")))
        stop(gettextf("not valid for %s censored survival data", attr(Y, "type")))
    nY <- ncol(Y)
    if (nY ==2) Y <- cbind(zero, Y)
    if (any(Y[,1] >= Y[,2]))    stop("start time must be < stop time")
        
    if (!is.numeric(cut) || any(!is.finite(cut)))
        stop("'cut' argument must be a vector of finite numbers")
    cut <- sort(cut)
    ntimes <- length(cut)
    n <- nrow(data)

    if (!missing(id)) {
        if (!is.character(id)) stop(gettextf("'%s' argument must be a variable name", "id"))
        if (id %in% names(mf)) stop("the suggested id name is already present")
        id <- make.names(id)
        if (id %in% names(mf)) stop("the suggested id name is already present")
        mf[[id]] <- 1:nrow(mf)
    }

    storage.mode(Y) <- "double"
    index <- .Call("survsplit", Y[,1], Y[,2], as.double(cut))
    newdata <- mf[index$row, -1, drop=FALSE]
    row.names(newdata) <- NULL    # erase R's manufactured row names
    attr(newdata, "terms") <- NULL

    status <- Y[index$row, 3]
    status[index$censor] <- 0
    if (!is.null(attr(Y, "states")))  
        status <- factor(status, labels=c("censor", attr(Y, "states")))

    # Did the user hand me a Surv call with multiple variables, or a
    #  premade Surv object?
    if (class(formula[[2]]) == "call" && formula[[2]][[1]]== as.name("Surv")){
        # it was a call, figure out the names
        # The user might have used something like Surv(status=abc, time=fred),
        #  so use match.call to resolve it.
        temp <- match.call(Surv, formula[[2]])
        for (i in c("time", "time2", "event")) {
            if (!(is.null(temp[[i]]) || is.name(temp[[i]])))
                stop("cannot deal with complex arguments within a Surv call")
        }
        if (nY ==2) {
            end <- temp$time
            if (is.null(temp$status)) event <- temp$time2
            else event <- temp$event
        }
        else {
            start <- temp$time
            end   <- temp$time2
            event <- temp$event
            }

        newdata[[as.character(start)]] <- index$start
        newdata[[as.character(end)]]   <- index$end
        newdata[[as.character(event)]] <- status
    }
    else {
        if (class(formula[[2]]) != "name")
            stop("left hand side was not recognized")
        temp <- as.character(formula[[2]])
        newdata[temp] <- Surv(index$start, index$end, status)
    }

    if (!missing(episode)) {
        if (!is.character(episode)) stop(gettextf("'%s' argument must be a character string", "episode"))
        newdata[[make.names(episode)]] <- index$interval +1
    }
    newdata
}
