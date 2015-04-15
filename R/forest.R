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
forest <- function(formula, data, mvars = NULL, B = 500, min_size = NULL, ...){
  #TODO: error check parameters
  #default vals
  y <-  eval(parse(text = as.character(formula)[2]), envir = data)
  if(is.null(mvars)) mvars <- if(!is.null(y) & !is.factor(y)) max(floor((ncol(data) - 1)/3), 1) else floor(sqrt(ncol(data) - 1))
  if(is.null(min_size)) min_size = if (!is.null(y) && !is.factor(y)) 5 else 1
  
  data.frame(b = 1:B) %>%
    group_by(b) %>%
    do(sample = draw_boot_sample(data)) -> data_star
  
  safe_grow <- failwith(NULL, grow_forest, quiet = TRUE)
  
  data_star %>%
    group_by(b) %>%
    do(rf = suppressWarnings(safe_grow(formula, .$sample[[1]], mvars, min_size))) -> rf
  
  data_star %>%
    left_join(rf) -> results

  #TODO class results as "forestr"
  return(results)
}

grow_forest <- function(formula, data_star_b, mvars, min_size, ...) {
  browser()
  #control function for planting trees
  tree <- plant_tree(formula, data_star_b, mvars, min_size, ...)
  frame <- tree$frame %>% select(var, n, wt, dev, yval)
  path <- path.rpart(tree, 1:nrow(frame), print.it = FALSE)
  locs <- tree$where
  idx <- which(frame$var == "<leaf>" & frame$n > min_size & frame$dev > 0)
  ylevels <- attr(tree, "ylevels")
  split <- length(idx) > 0
  max_node <- max(as.numeric(names(path))) #store for unique naming
  
  unsplit <- NULL
  while(split & length(idx) > 0) {
    #split notes that need to be
    frame[idx, ] %>%
      mutate(node = names(path[idx])) %>%
      group_by(node) %>%
      do(tree = plant_tree(formula, data_star_b[locs == .$node, ], mvars, min_size, ...)) %>%
      group_by(node) %>%
      do(frame = .$tree[[1]]$frame %>% select(var, n, wt, dev, yval),
         path = path.rpart(.$tree[[1]], 1:nrow(.$tree[[1]]$frame), print.it = FALSE),
         locs = .$tree[[1]]$where,
         ylevels = attr(.$tree[[1]], "ylevels")) %>%
         mutate(split = nrow(frame)) -> splits    
    
    #keep track of and remove nodes that aren't getting split for some reason
    unsplit <- c(unsplit, splits[splits$split == 1, "node"] %>% data.matrix)
    splits <- splits %>% filter(split > 1) 
    idx <- which(rownames(frame) %in% splits$node)
    if(nrow(splits) > 0) {
      #insert into frame/path where appropriate
      for(i in 1:nrow(splits)) {
        node_names <- c(as.numeric(splits$node[i]), (nrow(splits$frame[[i]]) > 1) * (max_node + 1:(nrow(splits$frame[[i]]) - 1)))
        node_names <- node_names[node_names > 0]
        max_node <<- max(max_node, max(node_names))
        
        locs[locs == as.numeric(rownames(frame)[idx[i]])] <- node_names[splits$locs[[i]]]
  
        inserts <- cbind(matrix(rep(path[splits$node[[i]]][[1]], length(splits$path[[i]])), nrow = length(splits$path[[i]]), byrow = TRUE),
                         do.call(rbind, splits$path[[i]])[, -1])
        rownames(inserts) <- node_names
        
        splits$path[[i]] <- split(inserts, rownames(inserts))
        splits$path[[i]] <- lapply(splits$path[[i]], function(x) { if(x[length(x)] == "root") x[-length(x)] else x })
        
        rownames(splits$frame[[i]]) <- names(splits$path[[i]])
      }
      
      frame <- frame[-idx, ] %>%
        rbind(do.call(rbind, splits$frame))
  
      new_path <- unlist(splits$path, recursive = FALSE)
      path <- c(path[-idx], new_path[!sapply(new_path, is.null)])
    }
    
    #check if more nodes to split
    idx <- which(frame$var == "<leaf>" & frame$n > min_size & frame$dev > 0)
    max_node <- max(as.numeric(names(path))) #store for unique naming
    split <- length(idx) > 0
    #stop trying to split if unsuccessful after 5 times splitting the same variable
    if(split) idx <- which(rownames(frame) %in% setdiff(rownames(frame)[idx], names(table(unsplit))[table(unsplit) >= 3]))
  }
  yval <- ylevels[frame[as.character(locs), "yval"]]
  res <- list(frame = frame, path = path, where = locs, yval = yval)
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
