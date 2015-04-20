regression_extremes_init <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if(missing(parms)){
    warning("Nothing specified, so assuming high means.")
    meantype <- 1
  } else {
    parms <- as.list(parms)
    #allow a user to pass "high" or "low" or list with 1st element = high or low, but nothing else

    # check only one item was passed
    if(length(parms) > 1){
      stop("For method='extremes' with regression must specify parms=1 or parms=-1 for \\
			high and low means respectively")
    }

    # we coerced to a list and that list has length=1, so get 1st element
    meantype <- parms[[1]]

    # print error if meantype isn't correctly specified.
    if(meantype != 1 && meantype != -1){
      stop("For method='extremes' must specify parms=1 or parms=-1 for \\
			high and low means respectively")
    }
  }
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  tfun <- function(yval, dev, wt, ylevel, digits, n, use.n ) {
    if(use.n) {paste(formatg(yval,digits),"\nn=", n,sep="")} else {paste(formatg(yval,digits))}
  }
  environment(sfun) <- environment(tfun) <- .GlobalEnv
  list(y = c(y), parms = meantype, numresp = 1, numy = 1, summary = sfun, text = tfun)
}

regression_extremes_eval <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label = wmean, deviance = rss)
}

regression_extremes_split <- function(y, wt, x, parms, continuous) {
  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    if(parms == 1) goodness <- apply(cbind(lmean, rmean), 1, max) else goodness <- apply(cbind(lmean, rmean), 1, min)
    list(goodness = goodness, direction = sign(lmean))
  } else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    # For anova splits, we can order the categories by their means
    # then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    if(parms == 1) goodness <- max(lmean, rmean) else goodness <- min(lmean, rmean)
    list(goodness = goodness,
         direction = ux[ord])
  }
}
