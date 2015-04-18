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
#' @export
proximity.forestr <- function(object, ...) {

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
#' @export
importance.forestr <- function(object, ...) {

}
