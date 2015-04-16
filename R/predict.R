#' Predict
#'
#' Predict methods to predict from forestr tree and random forest
#' within forestr using the \code{predict} function
#'
#' @param object an object of class forest as created by \code{forest}
#' @param newdata dataframe containing new data
#'
#' @return A data.frame of class forestr with columns
#'   \item{b}{1:B}
#'   \item{sample}{Bootstrap sampled datasets.}
#'   \item{rf}{Random forest object}
#'
#' @examples
#'
#' mtcars.forest <- forest(factor(cyl) ~ ., data = mtcars[1:25, ])
#' predict(mtcars.forest, mtcars[26:32, ])
#'
#' @name predict


#' @rdname predict.forest
#'
#' @import dplyr
#'
#' @export
`predict.forest` <- function(object, newdata) {

}


`predict.forest_tree` <- function(object, newdata) {
  stopifnot(identical(names(newdata), names(object$data)))

  newdata$idx <- 1:nrow(newdata)
  rules <- unlist(lapply(object$path[rownames(object$frame)[object$frame$var == "<leaf>"]], function(x) paste(x[-1], collapse = " & ")))
  split <- lapply(1:length(rules), function(x) data.frame(idx = newdata[eval(parse(text = rules[x]), envir = newdata), "idx"]))
  names(split) <- object$frame[object$frame$var == "<leaf>", "yval"]

  do.call(rbind, split) -> pred
  pred %>%
    mutate(yvals = sapply(strsplit(rownames(pred), "[.]"), function(x) x[1])) %>%
    select(yvals) %>%
    unlist(use.names = FALSE) %>% as.numeric -> pred
  object$ylevels[pred]
}


