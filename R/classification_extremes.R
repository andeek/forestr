classification_extremes_init <- function(y, offset, parms, wt) {
  #taken from itree
  if (!is.null(offset)) stop("No offset allowed in classification models")
  fy <- as.factor(y)
  y <- as.integer(fy)


  #number of classes. determines which purity function to use in the C code.
  numclass <- max(y[!is.na(y)])
  counts <- tapply(wt, factor(y, levels=1:numclass), sum)
  counts <- ifelse(is.na(counts), 0, counts)   #in case of an empty class
  if (missing(parms) || is.null(parms)){
    parms <- list(prior = counts/sum(counts),
                  loss = matrix(rep(1,numclass^2)-diag(numclass),numclass),
                  split = 1,classOfInterest=1)
    warning("No classOfInterest specified, so assuming the class of interest is class 1.")
  } else if (is.list(parms)) {
    if (is.null(names(parms))) stop("The parms list must have names")
    temp <- pmatch(names(parms), c("prior", "loss", "split", "classOfInterest"), nomatch=0L)
    if (any(temp==0L))
      stop("'parms' component not matched: ", names(parms)[temp==0L])
    names(parms) <- c("prior", "loss", "split","classOfInterest")[temp]


    # Prior:
    if (is.null(parms$prior)) {
      temp <- c(counts/sum(counts))
    } else {
      #print warnings
      warning("Passing priors and/or observations weights with class extremes
              finds splits with high proportion of specified class of interest
              weighted by prior/observation weight.")

      temp <- parms$prior
      if (sum(temp) !=1) stop("Priors must sum to 1")
      if (any(temp<0)) stop("Priors must be >= 0")
      if (length(temp) != numclass) stop("Wrong length for priors")
    }


    # Loss matrix:
    if (is.null(parms$loss)) {
      temp2<- 1 - diag(numclass)
    } else {
      stop("Class extremes method is not defined for a loss matrix.")
    }

    # Splitting rule:
    if (is.null(parms$split)) {
      temp3 <- 1L
    } else {
      stop("Invalid 'split' parameter: class extremes does not use gini or information.")
    }

    #now deal with the classOfInterest.
    #no class of interest was specified:
    if (is.null(parms$classOfInterest)){
      warning("No classOfInterest specified, so assuming the class of interest is class 1.")
      class_of_interest <- 1
    } else {
      #something was specified:
      users_requested_class <- parms$classOfInterest
      # check only one item was passed
      if(length(users_requested_class) > 1){
        stop("For method='extremes' with k-class classification, must specify an integer {1,...,k}\
             corresponding to the class of interest or the name of the level of y if y is a factor.")
      }

      #check if it's a string, if so, match to levels of y.
      if(is.character(users_requested_class)){
        class_of_interest <- pmatch(users_requested_class, levels(fy))

        #check to make sure it's not NA, if it is print an error.
        if(is.na(class_of_interest)){
          stop("The specified class is not in levels(y) -- this could happen \
               if the y variable is coded as an integer rather than a factor.")
        }
      } else {
        #not a string:
        classes <- (1:max(y[!is.na(y)]))
        if(is.numeric(users_requested_class) && users_requested_class %in% classes){
          class_of_interest <- users_requested_class
        } else {
          stop("The specified class of interest is not valid.")
        }
      }
      #end of figuring out what the class of interest is.
    }
    #end of dealing with classOfInterest


    #add warning about weights, if necessary.
    if(length(unique(wt))!=1){
      warning("Passing priors and/or observations weights with class extremes \
              finds splits with high proportion of specificed class of interest \
              weighted by prior/observation weight.")
    }

    parms <- list(prior=temp, loss=matrix(temp2,numclass), split=temp3,
                  classOfInterest=class_of_interest)
  } else stop("Parameter argument must be a list")

  list(y = y, parms = parms, numresp = numclass+1L, counts = counts,
       ylevels = levels(fy), numy = 1L,
       print = function(yval, ylevel, digits) {
         if (is.null(ylevel))
           temp <- as.character(yval[,1L])
         else    temp <- ylevel[yval[,1L]]

         nclass <- (ncol(yval) -1L)/2
         if (nclass <5) {
           yprob <- format(yval[, 1L+nclass + 1L:nclass],
                           digits=digits,nsmall=digits)
         }
         else yprob <- formatg(yval[, 1L+nclass + 1L:nclass], digits=2)
         if(is.null(dim(yprob))) # yprob is a vector
           yprob <- matrix(yprob, ncol=length(yprob))
         temp <- paste(temp, ' (', yprob[,1L], sep='')
         for(i in 2L:ncol(yprob))
            temp  <- paste(temp, yprob[, i], sep=' ')
         temp <- paste(temp, ")", sep="")
         temp
       },
       summary = function(yval, dev, wt, ylevel, digits) {
         nclass <- (ncol(yval)-1L) /2
         group <- yval[, 1]
         counts <- yval[, 1L+ (1L:nclass)]
         yprob  <- yval[, 1L+nclass + 1L:nclass]
         if(!is.null(ylevel)) group <- ylevel[group]

         temp1 <- formatg(counts, format="%5g")
         temp2 <- formatg(yprob,  format="%5.3f")
         if (nclass >1) {
           temp1 <- apply(matrix(temp1, ncol=nclass), 1,
                          paste, collapse=' ')
           temp2 <- apply(matrix(temp2, ncol=nclass), 1,
                          paste, collapse=' ')
         }
         paste("  predicted class=", format(group, justify='left'),
               "  expected loss=", formatg(dev/wt, digits),"\n",
               "    class counts: ", temp1,"\n",
               "   probabilities: ", temp2,
               sep='')
       },
       text= function(yval, dev, wt, ylevel, digits, n, use.n) {

         nclass <- (ncol(yval)-1L) /2
         group <- yval[, 1L]
         counts <- yval[, 1L+ (1L:nclass)]
         if(!is.null(ylevel)) group <- ylevel[group]

         temp1 <- formatg(counts, digits)
         if (nclass >1) {
           temp1 <- apply(matrix(temp1, ncol=nclass), 1,
                          paste, collapse='/')
         }

         if(use.n)  {out <- paste(format(group, justify='left'),"\n",
                                  temp1,sep="")}      else
                                  {out <- format(group,justify="left")}

         return(out)
     })
}

classification_extremes_eval <- function(y, wt, parms) {
  y <- as.factor(y)
  y.labels <- levels(y)
  y.tab <- table(y)
  class <- y.labels[which.max(y.tab)]
  dev <- sum(wt*(y != class)) #weighted?

  list(label = class, deviance = dev)
}

classification_extremes <- function(y, wt, x, parms, continuous) {
  n <- length(y)
  no.classes <- length(table(y))
  class.labs <- as.numeric(names(table((y))))
  class.interest <- parms$classOfInterest
  #what to do with parms$prior parms$loss parms$split??

  if (continuous) {
    if(no.classes == 1) {
      return(list(goodness = rep(0,n-1), direction = rep(-1, n-1))) #only 1 class
    } else {
      ymat <- matrix(rep(0, length(x)), ncol = 1)
      ymat[which(y == class.labs[class.interest]), class.interest] <- wt[which(y==class.labs[class.interest])]

      left.success <- cumsum(ymat)[-n]
      left.n <- cumsum(wt)[-n]
      left.p <- left.success/left.n
      right.success <- sum(ymat) - left.success
      right.p <- right.success/rev(left.n)

      goodness <- apply(cbind(left.p, right.p), 1, max)
      direction <- rep(-1, (n - 1)) #not sure about this

      list(goodness = goodness, direction = direction)
    }
  } else {
    n.cat <- length(unique(x))
    if(no.classes == 1) {
      return(list(goodness = rep(0, n.cat - 1), direction = rep(-1, n.cat -1))) #only 1 class
    } else {
      #categorical x
      #weighted numclassxnumcat array
      ymat <- matrix(rep(0, length(x)), ncol = 1)
      ymat[which(y == class.labs[class.interest]), class.interest] <- wt[which(y==class.labs[class.interest])]

      ux <- sort(unique(x))
      wtsum <- tapply(wt, x, sum)
      ysum <- tapply(ymat, x, sum)
      p <- ysum/wtsum

      #attempt to use anova code but with weighted categories (ymat)
      ord <- order(p)
      n <- length(ord)
      left.success <- cumsum(ysum[ord])[-n]
      right.success <- sum(ysum) - left.success
      left.n <- cumsum(wtsum[ord])[-n]
      right.n <- sum(wt) - left.wt
      left.p <- left.success/left.n
      right.p <- right.success/rev(left.n)
      goodness <- apply(cbind(left.p, right.p), 1, max)
      list(goodness = goodness,
           direction = rep(-1, length(goodness)))
    }
  }
}
