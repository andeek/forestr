regression_purity_init <- function(y, offset, parms, wt) {
  #copied from anova
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}

regression_purity_eval <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label = wmean, deviance = rss)
}

regression_purity_split <- function(y, wt, x, parms, continuous) {
  # Center y
  n <- length(y)
  y <- y - sum(y*wt)/sum(wt)
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    lvar <- cumsum(((y*wt)[-n] - lmean)^2)/left.wt
    rvar <- cumsum(((y*wt)[-1] - rmean)^2)/right.wt
    goodness <- apply(cbind(lvar, rvar), 1, min)
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
    lvar <- cumsum(((ysum[ord])[-n] - lmean)^2)/left.wt
    rvar <- cumsum(((ysum[ord])[-1] - rmean)^2)/right.wt
    goodness <- apply(cbind(lvar, rvar), 1, min)
    list(goodness = goodness,
         direction = ux[ord])
  }
}
