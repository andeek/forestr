#' Diagnostic methods for \code{forestr} to compute the proximity matrix
#'
#' @param object an object of class forest as created by \code{forest}
#' @param ... not currently used
#'
#' @return A data frame with the proximity values.
#' @examples
#' mtcars.forestr <- forestr(factor(cyl) ~ ., data = mtcars)
#' proximity(mtcars.forestr)
#'
#' @export
proximity <- function(object, ...) {
  UseMethod("proximity")
}

#' Proximity on a forestr object
#'
#' tidy on a NULL input returns an empty data frame, which means it can be
#' combined with other data frames (treated as "empty")
#'
#' @param object an object of class forest as created by \code{forest}
#' @param ... not currently used
#'
#' @return A data frame with the proximity values.
#'
#'
#' @export
proximity.forestr <- function(object, ...) {
  object.pred <- predict(object, object$data)
  names(object.pred$raw_preds) <- 1:object$B
  raw <- do.call(rbind, object.pred$raw_preds)
  raw$tree <- as.numeric(sapply(strsplit(rownames(raw), "[.]"), function(x) x[1]))

  raw[, c("row", "where", "tree")] -> table


  #TODO: this is very slow, but works. think of way to make faster
  x <- matrix(0, nrow = nrow(object$data), ncol = nrow(object$data))
  colnames(x) <- rownames(x) <- rownames(object$data)
  tabs <- split(table, table$tree)
  for(k in 1:length(tabs)) {
    for(i in 1:nrow(object$data)) {
      for(j in 1:nrow(object$data)) {
        if(tabs[[k]][i, "where"] == tabs[[k]][j, "where"]) x[i, j] <- x[i, j] + 1
      }
    }
  }
  return(x/object$B)
}

#' Diagnostic methods for \code{forestr} to compute the importance for each variable
#'
#' @param object an object of class forest as created by \code{forest}
#' @param ... not currently used
#'
#' @return A data frame with the variable importance.
#' @examples
#' mtcars.forestr <- forestr(factor(cyl) ~ ., data = mtcars)
#' importance(mtcars.forestr)
#'
#'
#' @export
importance <- function(object, ...) {
  UseMethod("importance")
}

#' Importance on a forestr object
#'
#' tidy on a NULL input returns an empty data frame, which means it can be
#' combined with other data frames (treated as "empty")
#'
#' @param object an object of class forest as created by \code{forest}
#' @param ... not currently used
#'
#' @return A data frame with the proximity values.
#'
#' @import dplyr
#'
#' @export
importance.forestr <- function(object, ...) {
  y <-  eval(parse(text = as.character(as.formula(as.character(object$call[2])))[2]), envir = object$data)

  object$data$idx <- 1:nrow(object$data)
  oob_true <- do.call(rbind, lapply(object$trees, function(tree){
    sample <- object$data[unique(sapply(strsplit(rownames(tree$data), "[.]"), function(x) x[1])), ]
    notsample <- object$data[-sample$idx, ]
    data.frame(pred = predict(tree, notsample %>% select(-idx))$yval, true = y[notsample$idx])
  }))
  oob_true %>%
    group_by(true) %>%
    summarise(false = sum(loss(pred, true)), count = n()) -> number_false

  varnames <- attr(terms(as.formula(as.character(object$call[2])), data = object$data %>% select(-idx)), "term.labels")
  number_false_perm <- lapply(varnames, function(var) {
    #permute the values of each variable
    permdata <- object$data
    permdata[, var] <- permdata[sample(1:nrow(object$data), replace = FALSE), var]
    permdata$idx <- 1:nrow(object$data)

    oob_perm <- do.call(rbind, lapply(object$trees, function(tree){
      sample <- permdata[unique(sapply(strsplit(rownames(tree$data), "[.]"), function(x) x[1])), ]
      notsample <- permdata[-sample$idx, ]
      data.frame(pred = predict(tree, notsample %>% select(-idx))$yval, true = y[notsample$idx])
    }))
    oob_perm %>%
      group_by(true) %>%
      summarise(false = sum(loss(pred, true)), count = n())
  })
  names(number_false_perm) <- varnames

  sapply(number_false_perm, function(x) {
    if(object$type == "classification") {
      number_false$count - number_false$false - (x$count - x$false)
    } else {
      x$false/x$count - number_false$false/number_false$count
    }
  }) %>%
    colSums %>%
    as.data.frame() %>%
    rename_(importance = ".") -> importance
  importance$var <- rownames(importance)
  rownames(importance) <- NULL
  importance <- importance %>% mutate(importance = importance/object$B) %>% select(var, importance)

  return(importance)

}
