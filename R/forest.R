#' Forest
#'
#' Runs a random forest with user specified splitting criteria
#' using the \code{forest} function.
#'
#' @param formula formula specification of forest variables
#' @param data dataframe with y and x variables
#' @param B number of bootstrap samples
#' @param mvars number of variables to sample in each node of
#' the tree during building the random forest
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
forest <- function(formula, data, mvars, B = 1000, min_size){
  data.frame(b = 1:B) %>%
    group_by(b) %>%
    do(sample = draw_boot_sample(data)) -> data_star

  data_star %>%
    group_by(b) %>%
    do(rf = grow_forest(formula, .$sample[[1]], mvars, min_size)) -> rf

  data_star %>%
    left_join(rf) -> results

  return(results)
}


draw_boot_sample <- function(data) {
  #draw a bootstrap sample of observations
  n <- nrow(data)
  return(data[sample(1:n, replace = TRUE), ])
}

grow_forest <- function(formula, data_star_b, mvars, min_size) {
  #control function for planting trees

}

plant_tree <- function(formula, data_star_b, mvars, min_size) {
  #function to be used recursively to grow the forest
  #split at current node with random variables (only 1 level deep)
  new_formula <- select_vars(formula, data_star_b, mvars)
  stub <- rpart(new_formula, data_star_b, control = rpart.control(maxdepth = 1)) #TODO change splitting mechanism

  #controls for continuing to grow tree
  left <- stub$where == unique(stub$where)[1]
  left_t <- dim(table(stub$y[left])) == 1 | sum(left) <= min_size
  right_t <- dim(table(stub$y[!left])) == 1 | sum(!left) <= min_size

  if(left_t) {
    if(right_t) {
      #both terminal

    } else {
      #left is terminal, not right

    }
  } else if(right_t) {
    #right is terminal, not left

  } else {
    #neither is terminal

  }

}

select_vars <- function(formula, data_star_b, mvars) {
  #select a random subset of variables
  right <- paste(sample(attr(terms(formula, data = data_star_b), "term.labels"), mvars, replace = FALSE), collapse = " + ")
  left <- as.character(formula)[[2]]

  as.formula(paste(left, right, sep = " ~ "))
}
