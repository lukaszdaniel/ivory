survSplit <- function(formula, data, subset, na.action=na.pass,
                              cut, start="tstart", id, zero=0, episode,
                              end="tstop", event="event") {
    Call <- match.call()
    if (missing(formula) || is.data.frame(formula)) {
        # an old style call
        # match arguments and build a formula
        if (missing(data)) {
            if (!missing(formula)) {
                names(Call)[[2]] <- "data"
                # The line above is sneaky: it makes model.frame() work later
                data <- formula
            }
            else  stop(gettextf("'%s' argument is required", "data"))
        }
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
        
        formula <- as.formula(paste("Surv(", temp, ")~ ."))
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
    states <- attr(Y, "states")
    if (!is.Surv(Y)) stop ("the model must have an object of class \"Surv\" as the response")
    if (!(attr(Y, "type") %in% c("right", "mright", "counting", "mcounting")))
        stop(gettextf("not valid for %s censored survival data", attr(Y, "type")))
    nY <- ncol(Y)
    ymiss <- is.na(Y)  # these pass through unchanged
    if (nY ==2) {
        if (any(Y[!ymiss,1] <= zero))
            stop("'zero' parameter must be less than any observed times")
        Y <- cbind(zero, Y)
    }
    temp <- (Y[!ymiss,1] >= Y[!ymiss,2])
    if (any(temp)) stop("start time must be < stop time")
        
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
    index <- .Call(Csurvsplit, Y[,1], Y[,2], as.double(cut))
    newdata <- mf[index$row, -1, drop=FALSE]
    row.names(newdata) <- NULL    # erase R's manufactured row names
    attr(newdata, "terms") <- NULL

    status <- Y[index$row, 3]
    status[index$censor] <- 0
    if (!is.null(states))  
        status <- factor(status, labels=c("censor", states))

    # Did the user hand me a Surv call with multiple variables, or a
    #  premade Surv object?
    if (inherits(formula[[2]], "call") && formula[[2]][[1]]== as.name("Surv")){
        # it was a call, figure out the names
        # The user might have used something like Surv(status=abc, time=fred),
        #  so use match.call to find "abc" and "fred".  But give up if there
        #  is anything complex.
        temp <- match.call(Surv, formula[[2]])
        if (nY==2) {
            if (missing(end) && !is.null(temp[["time"]]) 
                && is.name(temp[["time"]]))
                end <- as.character(temp[["time"]])  # $time might match 'time2'
             
            if (missing(event) && !is.null(temp$time2) && is.name(temp$time2)) 
                event <- as.character(temp$time2)
            if (missing(event) && !is.null(temp$event) && is.name(temp$event))
                event <- as.character(temp$event)
        }
        else {
            if (missing(end) && !is.null(temp[["time"]]) 
                && is.name(temp["time"]))
                start <- as.character(temp[["time"]])
            if (missing(end) && !is.null(temp$time2) && is.name(temp$time2)) 
                end <- as.character(temp$time2)
            if (missing(event) && !is.null(temp$event) && is.name(temp$event))
                event <- as.character(temp$event)
            if (missing(start) && !is.null(temp$time) && is.name(temp$time))
                start <- as.character(temp$time)
        }

        newdata[[start]] <- index$start
        newdata[[end]]   <- index$end
        newdata[[event]] <- status
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
