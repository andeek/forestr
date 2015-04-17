#' Predict
#'
#' Predict methods to predict from forestr tree and random forest
#' within forestr using the \code{predict} function
#'
#' @param object an object of class forest as created by \code{forest}
#' @param newdata dataframe containing new data
#' @param ... not currently used
#'
#' @return A list with the following components
#'    \item{response}{the predicted classes or values for each observation}
#'    \item{votes}{if \code{object$type} is classification, the vote matrix used to create the prediction}
#'
#' @examples
#'
#' mtcars.forest <- forest(factor(cyl) ~ ., data = mtcars[1:25, ])
#' predict(mtcars.forest, mtcars[26:32, ])
#'
#' @name predict


#' @rdname predict
#'
#' @import dplyr
#'
#' @export
predict.forestr <- function(object, newdata, ...) {
  y <-  eval(parse(text = as.character(as.formula(as.character(object$call[2])))[2]), envir = newdata)
  #predict for each tree
  object$raw_results %>%
    ungroup() %>%
    group_by(b) %>%
    do(pred = data.frame(pred = predict(.$rf[[1]], newdata), true = y, row = rownames(newdata))) -> preds

  preds <- do.call(rbind, preds$pred)

  if(object$type == "classification") {
    #votes
    votes <- preds %>% group_by(row, pred) %>% summarise(count = n()) %>% spread(pred, count, fill = 0)
    votes$value <- names(votes)[apply(votes[, -1], 1, which.max) + 1]
    votes <- inner_join(votes, data.frame(row = rownames(newdata), idx = 1:nrow(newdata)), by = "row") %>% arrange(idx) %>% select(-idx) #reordering by original
  } else {
    #TODO: stupid name convention, consider changing for regression
    votes <- preds %>% group_by(row) %>% summarise(value = mean(pred))
    votes <- inner_join(votes, data.frame(row = rownames(newdata), idx = 1:nrow(newdata)), by = "row") %>% arrange(idx) %>% select(-idx) #reordering by original
  }
  return(list(response = votes$vote, vote = votes))
}


predict.forest_tree <- function(object, newdata, ...) {
  stopifnot(identical(names(object$data), names(newdata)))

  newdata$idx <- 1:nrow(newdata)
  rules <- unlist(lapply(object$path[rownames(object$frame)[object$frame$var == "<leaf>"]], function(x) paste(x[-1], collapse = " & ")))
  split <- lapply(1:length(rules), function(x) data.frame(idx = newdata[eval(parse(text = rules[x]), envir = newdata), "idx"]))
  if(nrow(object$frame) == 1) split <- list(newdata)
  names(split) <- object$frame[object$frame$var == "<leaf>", "yval"]

#   do.call(rbind, split) -> pred
#   pred %>%
#     mutate(yvals = substr(rownames(pred), 1, 1)) %>%
#     arrange(idx) %>%
#     select(yvals) %>%
#     unlist(use.names = FALSE) %>% as.numeric -> pred
#   object$ylevels[pred]

  do.call(rbind, lapply(1:length(split), function(x) split[[x]] %>% mutate(yval = names(split)[x]))) %>%
    arrange(idx) %>%
    select(yval) %>% data.matrix() %>% as.numeric() -> pred

  if(object$type == "classification") object$ylevels[pred] else pred
}


