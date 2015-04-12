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
#' @param ... extra parameters to pass to rpart
#'
#' @return A data.frame of class forestr with columns
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
#' @import rpart.utils
#'
#' @export
forest <- function(formula, data, mvars, B = 1000, min_size, ...){
  #TODO: error check parameters

  data.frame(b = 1:B) %>%
    group_by(b) %>%
    do(sample = draw_boot_sample(data)) -> data_star

  data_star %>%
    group_by(b) %>%
    do(rf = grow_forest(formula, .$sample[[1]], mvars, min_size)) -> rf

  data_star %>%
    left_join(rf) -> results

  #TODO class results as "forestr"
  return(results)
}

grow_forest <- function(formula, data_star_b, mvars, min_size, ...) {
  #control function for planting trees
  tree <- plant_tree(formula, data_star_b, mvars, min_size, ...)
  frame <- tree$frame %>%
    select(-starts_with("yval2")) %>%
    mutate(node = as.numeric(rownames(tree$frame)))

  frame %>%
    filter(var == "<leaf>") %>%
    group_by(node) %>%
    mutate(continue = n > min_size && dim(table(tree$y[tree$where == node])) > 1) %>%
    filter(continue) -> nodes_to_split

  split <- nrow(nodes_to_split) > 0
  while(split) {

    #split notes that need to be
    nodes_to_split %>%
      group_by(node) %>%
      do(split = plant_tree(formula, data_star_b[tree$where == node,], mvars, min_size, ...)) -> tree1

    #insert into frame where appropriate
    list_frame <- split(frame, frame$node)
    remove <- NULL
    lapply(names(list_frame), function(x) {
      if(list_frame[[x]]$node %in% tree1$node) {
        remove <<- c(remove, x)
        tmp <- tree1 %>% filter(node == as.numeric(x))
        tmp$split[[1]]$frame %>%
          select(-starts_with("yval2")) %>%
          mutate(node = c(x, (as.numeric(x) + as.numeric(rownames(.)))[-1])) -> tmp

        split(tmp, tmp$node)
      }
    }) -> list_frame2
    frame <- do.call(rbind, c(list_frame[!(names(list_frame) %in% remove)], unlist(list_frame2, recursive = FALSE)))

    #check if more nodes to split
    frame %>%
      filter(var == "<leaf>") %>%
      group_by(node) %>%
      mutate(continue = n > min_size && dim(table(tree$y[tree$where == node])) > 1) %>%
      filter(continue) -> nodes_to_split

    split <- nrow(nodes_to_split) > 0
  }
  return(frame)
}

plant_tree <- function(formula, data_star_b, mvars, min_size, ...) {
  #function to be used recursively to grow the forest
  #split at current node with random variables (only 1 level deep)
  new_formula <- select_vars(formula, data_star_b, mvars)
  stub <- rpart(new_formula, data_star_b, control = rpart.control(maxdepth = 1, minbucket = min_size)) #TODO change splitting mechanism
  return(stub)
}

draw_boot_sample <- function(data) {
  #draw a bootstrap sample of observations
  n <- nrow(data)
  return(data[sample(1:n, replace = TRUE), ])
}

select_vars <- function(formula, data_star_b, mvars) {
  #select a random subset of variables
  right <- paste(sample(attr(terms(formula, data = data_star_b), "term.labels"), mvars, replace = FALSE), collapse = " + ")
  left <- as.character(formula)[[2]]

  as.formula(paste(left, right, sep = " ~ "))
}
