#' Predict
#'
#' Predict methods to predict from forestr tree and random forest
#' within forestr using the \code{predict} function
#'
#' @param object an object of class forest as created by \code{forest}
#' @param newdata dataframe containing new data
#' @param ... not currently used
#'
#' @return currently nothing
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

}


predict.forest_tree <- function(object, newdata, ...) {
  stopifnot(identical(names(object$data), names(newdata)))

  newdata$idx <- 1:nrow(newdata)
  rules <- unlist(lapply(object$path[rownames(object$frame)[object$frame$var == "<leaf>"]], function(x) paste(x[-1], collapse = " & ")))
  split <- lapply(1:length(rules), function(x) data.frame(idx = newdata[eval(parse(text = rules[x]), envir = newdata), "idx"]))
  names(split) <- object$frame[object$frame$var == "<leaf>", "yval"]

  do.call(rbind, split) -> pred
  pred %>%
    mutate(yvals = substr(rownames(pred), 1, 1)) %>%
    arrange(idx) %>%
    select(yvals) %>%
    unlist(use.names = FALSE) %>% as.numeric -> pred
  object$ylevels[pred]
}


