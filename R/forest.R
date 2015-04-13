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
  frame <- tree$frame %>% select(var, n, wt, dev, yval)
  path <- path.rpart(tree, 1:nrow(frame), print.it = FALSE)
  locs <- tree$where
  idx <- which(frame$var == "<leaf>" & frame$n > min_size & frame$dev > 0)
  ylevels <- attr(tree, "ylevels")
  split <- length(idx) > 0
  max_node <- max(as.numeric(names(path))) #store for unique naming

  while(split) {
    #split notes that need to be
    frame[idx, ] %>%
      mutate(node = names(path[idx])) %>%
      group_by(node) %>%
      do(tree = plant_tree(formula, data_star_b[locs == .$node, ], mvars, min_size, ...)) -> split_nodes

    split_nodes %>%
      group_by(node) %>%
      do(frame = .$tree[[1]]$frame %>% select(var, n, wt, dev, yval),
         path = path.rpart(.$tree[[1]], 1:nrow(.$tree[[1]]$frame), print.it = FALSE),
         locs = .$tree[[1]]$where,
         ylevels = attr(.$tree[[1]], "ylevels"),
         names = c(.$node, max_node + 1:nrow(.$tree[[1]]$frame))[1:nrow(.$tree[[1]]$frame)]) -> splits

    #insert into frame/path where appropriate
    for(i in 1:length(splits$locs)) {
      locs[locs == as.numeric(rownames(frame)[idx[i]])] <- unlist(splits$names[[i]][splits$locs[[i]]])

      inserts <- cbind(matrix(path[names(path) %in% names(splits$path[[i]])][[1]], nrow = length(splits$path[[i]]), ncol = length(path[names(path) %in% names(splits$path[[i]])][[1]]), byrow = TRUE),
            do.call(rbind, splits$path[[i]])[, -1])
      rownames(inserts) <- splits$names[[i]] #TODO: this isn't working... non-unique node numbers
      splits$path[[i]] <- split(inserts, rownames(inserts))
      splits$path[[i]] <- lapply(splits$path[[i]], function(x) {
        if(length(x) == 1) NULL
        else if(x[length(x)] == "root") x[-length(x)]
        else x
      })
    }

    frame <- frame[-idx, ] %>%
      rbind(do.call(rbind, splits$frame))
    #TODO rename rows of data.frame corresponding to row node numbers

    new_path <- unlist(splits$path, recursive = FALSE)
    path <- c(path, new_path[new_path != "root" & !sapply(new_path, is.null)])

    #check if more nodes to split
    idx <- which(frame$var == "<leaf>" & frame$n > min_size & frame$dev > 0)
    split <- length(splits$idx[[1]]) > 0
    max_node <- max(as.numeric(names(path))) #store for unique naming
  }

  res <- list(frame = frame, path = path, where = locs)
  class(res) <- "forest_tree"

  return(res)
}

plant_tree <- function(formula, data_star_b, mvars, min_size, ...) {
  #function to be used recursively to grow the forest
  #split at current node with random variables (only 1 level deep)
  new_formula <- select_mvars(formula, data_star_b, mvars)
  stub <- rpart(new_formula, data_star_b, control = rpart.control(maxdepth = 1, minbucket = min_size)) #TODO change splitting mechanism
  return(stub)
}

draw_boot_sample <- function(data) {
  #draw a bootstrap sample of observations
  n <- nrow(data)
  return(data[sample(1:n, replace = TRUE), ])
}

select_mvars <- function(formula, data_star_b, mvars) {
  #select a random subset of variables
  right <- paste(sample(attr(terms(formula, data = data_star_b), "term.labels"), mvars, replace = FALSE), collapse = " + ")
  left <- as.character(formula)[[2]]

  as.formula(paste(left, right, sep = " ~ "))
}
