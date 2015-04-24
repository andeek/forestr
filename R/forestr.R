#' Classification and Regression with Random Forest and Choice of Splitting Function
#'
#' Runs a random forest with user specified splitting criteria
#' using the \code{forestr} function.
#'
#' @param formula,x formula specification of forest variables,
#'        (for the \code{print} method, a \code{forestr} object)
#' @param data dataframe with y and x variables
#' @param B number of bootstrap samples
#' @param mvars number of variables to sample in each node of
#'        the tree during building the random forest
#' @param min_size minimum size of a terminal node
#' @param method optional splitting method, currently \code{"extremes"} and \code{"purity"} implemented.
#' @param ... extra parameters to pass to rpart
#'
#' @return An object of class \code{forestr} with components
#'   \item{call}{the original call to \code{forest}}
#'   \item{type}{one of \code{regression} or \code{classification}}
#'   \item{predicted}{the predicted values of the input data based on out of bag samples}
#'   \item{importance}{mean decrease in accurace over all classes for each variable in the model}
#'   \item{votes}{matrix giving the votes for each class on each observation (classification)}
#'   \item{oob}{out of bag error for the model including confusion matrix (classification)}
#'   \item{trees}{random forest tree objects}
#'
#' @examples
#'
#' forestr(factor(cyl) ~ ., data = mtcars)
#' @name forestr


#' @rdname forestr
#'
#' @import dplyr
#' @import rpart
#' @importFrom tidyr spread
#'
#' @export
forestr <- function(formula, data, mvars, B = 500, min_size, method, ...){

  y <-  eval(parse(text = as.character(formula)[2]), envir = data)
  #TODO: error check parameters
  stopifnot(class(y) %in% c("factor", "character", "integer", "numeric"))

  #default vals
  type <- ifelse(class(y) %in% c("factor", "character"), "classification", "regression")
  type2 <- NULL
  if(!missing(method)) if(method %in% c("extremes", "purity")) type2 <- method
  if(missing(mvars)) mvars <- if(!is.null(y) & !is.factor(y)) max(floor((ncol(data) - 1)/3), 1) else floor(sqrt(ncol(data) - 1))
  if(missing(min_size)) min_size <- if (!is.null(y) && !is.factor(y)) 5 else 1

  safe_grow <- failwith(NULL, grow_forest, quiet = FALSE)

  data.frame(b = 1:B) %>%
    group_by(b) %>%
    do(sample = draw_boot_sample(data)) -> results

  if(missing(method)) {
    results %>%
      ungroup() %>%
      group_by(b) %>%
      do(rf = safe_grow(formula = formula, data_star_b = .$sample[[1]] %>% select(-idx), mvars = mvars, min_size = min_size, type = type, type2 = type2, ...)) %>%
      right_join(results, by = "b") -> results
  } else {
    results %>%
      ungroup() %>%
      group_by(b) %>%
      do(rf = safe_grow(formula = formula, data_star_b = .$sample[[1]] %>% select(-idx), mvars = mvars, min_size = min_size, type = type, type2 = type2, method = method, ...)) %>%
      right_join(results, by = "b") -> results
  }


  results %>%
    ungroup() %>%
    group_by(b) %>%
    do(pred = data.frame(pred = predict(.$rf[[1]], data[-unique(.$sample[[1]][, "idx"]), ])$yval, true = y[-unique(.$sample[[1]][, "idx"])])) %>%
    right_join(results, by = "b") -> results

  data.frame(row = do.call(c, lapply(results$sample, function(x) rownames(data)[(1:nrow(data))[-unique(x$idx)]])),
             do.call(rbind, results$pred)) -> preds

  if(type == "classification") {
    votes <- preds %>% group_by(row, pred) %>% summarise(count = n()) %>% spread(pred, count, fill = 0)
    votes$value <- names(votes)[apply(votes[, -1], 1, which.max) + 1]
    votes <- inner_join(votes, data.frame(row = rownames(data), idx = 1:nrow(data)), by = "row") %>% arrange(idx) %>% select(-idx) #reordering by original

    table(votes$value, y) -> misclass_table
  } else {
    #TODO: stupid name convention, consider changing for regression
    votes <- preds %>% group_by(row) %>% summarise(value = mean(pred))
    votes <- inner_join(votes, data.frame(row = rownames(data), idx = 1:nrow(data)), by = "row") %>% arrange(idx) %>% select(-idx) #reordering by original
  }
  oob_error <- mean(loss(votes$value, y))

  res <- list(call = match.call(), type = type, type2 = type2, votes = votes, oob = oob_error, data = data)
  if(type == "classification") res$misclass <- misclass_table
  res$trees <- results$rf
  res$B <- B
  res$mvars <- mvars
  class(res) <- "forestr"
  return(res)
}

loss <- function(pred, y) {
  if(class(y) %in% c("factor", "character")) pred != y else (pred - y)^2
}

grow_forest <- function(formula, data_star_b, mvars, min_size, type, type2, ...) {
  #control function for planting trees
  tree <- plant_tree(formula, data_star_b, mvars, min_size, ...)
  frame <- tree$frame %>% select(var, n, wt, dev, yval)
  path <- path.rpart(tree, 1:nrow(frame), print.it = FALSE)
  locs <- tree$where
  idx <- which(frame$var == "<leaf>" & frame$n > min_size & frame$dev > 0)
  if(length(type2) > 0) if(type2 == "extremes") idx <- which(frame$var == "<leaf>" & frame$n > min_size & abs(frame$dev) > 1e-10 & abs(frame$dev) < 1 - 1e-10) #extremes gives deviance of 1 if all not of interest, stop trying to split this...?
  ylevels <- attr(tree, "ylevels")
  split <- length(idx) > 0
  max_node <- max(as.numeric(names(path))) #store for unique naming

  unsplit <- NULL
  while(split & length(idx) > 0) {
    #split notes that need to be
    frame[idx, ] %>%
      mutate(node = names(path[idx])) %>%
      ungroup() %>%
      group_by(node) %>%
      do(tree = plant_tree(formula, data_star_b[locs == .$node, ], mvars, min_size, ...)) %>%
      ungroup() %>%
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
        max_node <- max(max_node, max(node_names))

        #if(length(locs[locs == as.numeric(rownames(frame)[idx[i]])]) != length(splits$locs[[i]])) return(list(locs = locs, frame = frame, splits = splits, path = path, idx = idx))
        locs[locs == as.numeric(splits$node[i])] <- node_names[splits$locs[[i]]]

        inserts <- cbind(matrix(rep(path[splits$node[[i]]][[1]], length(splits$path[[i]])), nrow = length(splits$path[[i]]), byrow = TRUE),
                         do.call(rbind, splits$path[[i]])[, -1])
        rownames(inserts) <- node_names

        splits$path[[i]] <- split(inserts, as.numeric(rownames(inserts))) #as.numeric because otherwise out of order
        splits$path[[i]] <- lapply(splits$path[[i]], function(x) { if(x[length(x)] == "root") x[-length(x)] else x })

        #names and yvals fixed to top level
        rownames(splits$frame[[i]]) <- names(splits$path[[i]])
        if(type == "classification") splits$frame[[i]][ , "yval"] <- sapply(splits$ylevels[[i]][splits$frame[[i]]$yval], grep, x = ylevels)
      }

      frame <- frame[-idx, ] %>%
        rbind(do.call(rbind, splits$frame))

      new_path <- unlist(splits$path, recursive = FALSE)
      path <- c(path[-idx], new_path[!sapply(new_path, is.null)])
    }

    #check if more nodes to split
    idx <- which(frame$var == "<leaf>" & frame$n > min_size & frame$dev > 0)
    if(length(type2) > 0) if(type2 == "extremes") idx <- which(frame$var == "<leaf>" & frame$n > min_size & abs(frame$dev) > 1e-10 & abs(frame$dev) < 1 - 1e-10)
    max_node <- max(as.numeric(names(path))) #store for unique naming
    split <- length(idx) > 0
    #stop trying to split if unsuccessful after 3 times splitting the same variable
    if(split) idx <- which(rownames(frame) %in% setdiff(rownames(frame)[idx], names(table(unsplit))[table(unsplit) >= 3]))
  }

  if(type == "classification") yval <- ylevels[frame[as.character(locs), "yval"]] else yval <- frame[as.character(locs), "yval"]
  res <- list(frame = frame, path = path, where = locs, yval = yval, data = data_star_b, ylevels = ylevels, type = type)
  class(res) <- "forest_tree"

  return(res)
}

plant_tree <- function(formula, data_star_b, mvars, min_size, ...) {
  #function to be used recursively to grow the forest
  #split at current node with random variables (only 1 level deep)
  new_formula <- select_mvars(formula, data_star_b, mvars)
  stub <- rpart(new_formula, data_star_b, control = rpart.control(maxdepth = 1, minbucket = min_size), ...)
  return(stub)
}

draw_boot_sample <- function(data) {
  #draw a bootstrap sample of observations
  n <- nrow(data)
  idx <- sample(1:n, replace = TRUE)
  return(cbind(idx, data[idx, ]))
}

select_mvars <- function(formula, data_star_b, mvars) {
  #select a random subset of variables
  right <- paste(sample(attr(terms(formula, data = data_star_b), "term.labels"), mvars, replace = FALSE), collapse = " + ")
  left <- as.character(formula)[[2]]

  as.formula(paste(left, right, sep = " ~ "))
}

#' @rdname forestr
#'
#' @export
print.forestr <- function(x, ...) {
  cat("\nCall:\n", deparse(x$call), "\n")
  cat("               Type of random forest: ", paste(x$type, x$type2, sep = " - "), "\n",
      sep = "")
  cat("                     Number of trees: ", x$B, "\n",
      sep = "")
  cat("No. of variables tried at each split: ", x$mvars, "\n\n",
      sep = "")
  if (x$type == "classification") {
    if (!is.null(x$misclass)) {
      cat("        OOB estimate of  error rate: ", round(x$oob * 100, digits = 2), "%\n", sep = "")
      cat("Confusion matrix:\n")
      print(x$misclass)
    }
  }
  if (x$type == "regression") {
    if (!is.null(x$mse)) {
      cat("     OOB Mean of squared residuals: ", x$oob,
          "\n", sep = "")
    }
  }
}
