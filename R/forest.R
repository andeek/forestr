#' Forest
#'
#' Runs a random forest with user specified splitting criteria
#' using the \code{forest} function.
#'
#' @param formula formula specification of forest variables
#' @param data dataframe with y and x variables
#' @param B number of bootstrap samples
#'
#' @return A data.frame with columns
#'   \item{b}{1:B}
#'   \item{sample}{Bootstrap sampled datasets.}
#'   \item{rf}{Random forest object}
#'
#' @examples
#'
#' forest(factor(cyl) ~ ., data = mtcars)
#' @name forest


#' @rdname forest
#'
#' @import dplyr
#' @import rpart
#'
#' @export
forest <- function(formula, data, mvars, B = 1000){
  data.frame(b = 1:B) %>%
    group_by(b) %>%
    do(sample = draw_boot_sample(data)) -> data_star

  data_star %>%
    group_by(b) %>%
    do(rf = grow_random_forest(formula, .$sample[[1]])) -> rf

  data_star %>%
    left_join(rf) -> results

  return(results)
}


draw_boot_sample <- function(data) {
  n <- nrow(data)
  return(data[sample(1:n, replace = TRUE), ])
}


grow_random_forest <- function(formula, dat_star, mvars) {

  #new_formula <- select_vars(formula, mvars)
  #rpart(new_formula, dat_star, )

}
