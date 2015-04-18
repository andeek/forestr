#' Predict Method for forestr objects
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
#' mtcars.forestr <- forestr(factor(cyl) ~ ., data = mtcars[1:25, ])
#' predict(mtcars.forestr, mtcars[26:32, ])
#'
#' @name predict


#' @rdname predict
#'
#' @import dplyr
#' @importFrom tidyr separate
#'
#' @export
predict.forestr <- function(object, newdata, ...) {
  y <-  eval(parse(text = as.character(as.formula(as.character(object$call[2])))[2]), envir = newdata)
  #predict for each tree
  raw_preds <- lapply(object$tree, function(x) {
    tmp <- predict(x, newdata)
    data.frame(pred = tmp$yval, true = y, row = as.character(rownames(newdata)), where = tmp$node)
  })
  preds <- do.call(rbind, raw_preds)

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
  return(list(response = votes$value, vote = votes, raw_preds = raw_preds))
}


predict.forest_tree <- function(object, newdata, ...) {
  stopifnot(identical(names(object$data), names(newdata)))

  newdata$idx <- 1:nrow(newdata)
  rules <- unlist(lapply(object$path[rownames(object$frame)[object$frame$var == "<leaf>"]], function(x) paste(x[-1], collapse = " & ")))
  split <- lapply(1:length(rules), function(x) data.frame(idx = newdata[eval(parse(text = rules[x]), envir = newdata), "idx"]))
  if(nrow(object$frame) == 1) split <- list(newdata)
  names(split) <- paste(object$frame[object$frame$var == "<leaf>", "yval"], rownames(object$frame[object$frame$var == "<leaf>",]), sep = "_")

  do.call(rbind, lapply(1:length(split), function(x) split[[x]] %>% mutate(yval = names(split)[x]))) %>%
    separate(yval, into = c("yval", "node"), by = "_") %>%
    mutate(yval = as.numeric(yval), node = as.numeric(node)) %>%
    arrange(idx) %>%
    select(-idx) %>% as.list() -> pred

  if(object$type == "classification") list(yval = object$ylevels[pred$yval], node = pred$node) else pred
}


