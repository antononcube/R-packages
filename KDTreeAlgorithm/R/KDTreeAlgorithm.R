EuclideanDistance <- function(a, b) {
  sqrt(sum((a - b) ^ 2))
}

#==========================================================
# KDimensionalTree
#==========================================================

#' Create a K-dimensional tree object.
#' @description Contructs a K-dimensional tree S3 object based on given points.
#' @param points A numerical matrix, each row corresponding to a point.
#' @param distanceFunction Distance function for to use.
#' If NULL then Euclidean distance is used.
#' @export
KDimensionalTree <- function(points, distanceFunction = NULL) {
  if (is.vector(points)) {
    points <- matrix(points, ncol = 1)
  }

  if (!is.matrix(points)) {
    stop("The points argument is expected to be a matrix or a vector.")
  }

  rownames(points) <- if (is.null(rownames(points))) seq_len(nrow(points)) else rownames(points)

  if(is.null(distanceFunction)) { distanceFunction <- EuclideanDistance}

  structure(
    list(
      points = points,
      tree = build_tree(points),
      distanceFunction = distanceFunction
    ),
    class = "KDimensionalTree"
  )
}

#' Gist of KDimenstionalTree object.
#' @export
print.KDimensionalTree <- function(tree) {
  cat("KDimensionalTree:\n")
  cat("  Number of points:", nrow(tree$points), "\n")
  cat("  Distance function:", deparse(substitute(tree$distanceFunction)), "\n")
}

#==========================================================
# Insert into KDimensionalTree
#==========================================================

insert.KDimensionalTree <- function(tree, point) {
  if (is.vector(point)) {
    point <- matrix(point, ncol = ncol(tree$points))
  }

  if (!is.matrix(point) || ncol(point) != ncol(tree$points)) {
    stop("The point must be a vector or a matrix with the same number of columns as the tree's points.")
  }

  tree$points <- rbind(tree$points, point)
  rownames(tree$points)[nrow(tree$points)] <- as.character(nrow(tree$points))
  tree$tree <- build_tree(tree$points)
  tree
}

#==========================================================
# Build tree
#==========================================================

build_tree <- function(points, depth = 0) {
  if (nrow(points) == 0) return(NULL)

  axis <- depth %% ncol(points)
  points <- points[order(points[, axis + 1]), , drop = FALSE]
  median <- nrow(points) %/% 2

  list(
    point = points[median + 1, , drop = FALSE],
    left = if(median > 0) { build_tree(points[1:median, , drop = FALSE], depth + 1) } else { NULL },
    right = if(median + 2 <= nrow(points)) { build_tree(points[(median + 2):nrow(points), , drop = FALSE], depth + 1) } else {NULL}
  )
}

#==========================================================
# KNearest
#==========================================================

#' Nearest k-points.
#' @description For a given point finds the nearest point(s) to it.
#' @param tree K-dimensional tree S3 object.
#' @param Query point.
#' @param k Number of nearest neighbors to find.
#' @param format Format of the result.
#' One NULL, "data.frame", "list", or "points".
#' @export
KNearest <- function(tree, point, k = 1, format = NULL) {
  # Handle point
  if (is.vector(point)) {
    point <- matrix(point, ncol = ncol(tree$points))
  }

  if (!is.matrix(point) || ncol(point) != ncol(tree$points)) {
    stop("The point must be a vector or a matrix with the same number of columns as the tree's points.")
  }

  # Handle format
  if(is.null(format)) { format <- "list" }

  if(!(is.character(format) && length(format) == 1)) {
    stop('The value of the argument format is expected to be NULL, "data.frame", "list", or "points".', call. = TRUE)
  }

  # Compute NNs
  res <- k_nearest_rec(tree$tree, point, k, 0, tree$distanceFunction)

  # Format result
  if (tolower(format) == "list") {

    return(res)

  } else if (tolower(format) %in% c("values", "points")) {

    return(t(sapply(res, function(x) x$point)))

  } else if (tolower(format) %in% c("dataframe", "data.frame", "table")) {

    dfRes <- do.call(rbind, lapply(res, function(x) {
      data.frame(RowName = rownames(x$point), Distance = x$distance)
    }))

    dfRes <- cbind(dfRes, Index = match(dfRes$RowName, rownames(tree$points)))[, c("RowName", "Index", "Distance")]

    return(dfRes)

  } else {
    warning("Unknown return format specified.")
    return(res)
  }
}

k_nearest_rec <- function(node, point, k, depth, distanceFunction) {
  if (is.null(node)) return(list())

  axis <- depth %% ncol(point)
  next_branch <- if (point[1, axis + 1] < node$point[1, axis + 1]) node$left else node$right
  other_branch <- if (point[1, axis + 1] < node$point[1, axis + 1]) node$right else node$left

  best <- k_nearest_rec(next_branch, point, k, depth + 1, distanceFunction)
  dist <- distanceFunction(point, node$point)
  best <- append(best, list(list(point = node$point, distance = dist)))
  best <- best[order(sapply(best, function(x) x$distance))]
  if (length(best) > k) best <- best[1:k]

  if (length(best) < k || abs(point[1, axis + 1] - node$point[1, axis + 1]) <= best[[length(best)]]$distance) {
    best <- append(best, k_nearest_rec(other_branch, point, k, depth + 1, distanceFunction))
    best <- best[order(sapply(best, function(x) x$distance))]
    if (length(best) > k) best <- best[1:k]
  }

  best
}

#==========================================================
# Nearest within ball
#==========================================================

#' Nearest within ball.
#' @description For a given point finds the nearest point(s) to it within a given ball radius.
#' @param tree K-dimensional tree S3 object.
#' @param Query point.
#' @param radius Ball radius.
#' @param format Format of the result.
#' One NULL, "data.frame", "list", or "points".
#' @export
NearestWithinBall <- function(tree, point, radius, format = TRUE) {
  # Handle point
  if (is.vector(point)) {
    point <- matrix(point, ncol = ncol(tree$points))
  }

  if (!is.matrix(point) || ncol(point) != ncol(tree$points)) {
    stop("The point must be a vector or a matrix with the same number of columns as the tree's points.")
  }

  # Handle format
  if(is.null(format)) { format <- "list" }

  if(!(is.character(format) && length(format) == 1)) {
    stop('The value of the argument format is expected to be NULL, "data.frame", "list", or "points".', call. = TRUE)
  }

  # Compute NNs
  res <- nearest_within_ball_rec(tree$tree, point, radius, 0, tree$distanceFunction)

  # Format result
  if (tolower(format) == "list") {

    return(res)

  } else if (tolower(format) %in% c("values", "points")) {

    return(t(sapply(res, function(x) x$point)))

  } else if (tolower(format) %in% c("dataframe", "data.frame", "table")) {

    dfRes <- do.call(rbind, lapply(res, function(x) {
      data.frame(RowName = rownames(x$point), Distance = x$distance)
    }))

    dfRes <- cbind(dfRes, Index = match(dfRes$RowName, rownames(tree$points)))[, c("RowName", "Index", "Distance")]

    return(dfRes)

  } else {
    warning("Unknown return format specified.")
    return(res)
  }
}

nearest_within_ball_rec <- function(node, point, r, depth, distanceFunction) {
  if (is.null(node)) return(list())

  axis <- depth %% ncol(point)
  dist <- distanceFunction(point, node$point)
  inside <- if (dist <= r) list(list(point = node$point, distance = dist)) else list()

  next_branch <- if (point[1, axis + 1] < node$point[1, axis + 1]) node$left else node$right
  other_branch <- if (point[1, axis + 1] < node$point[1, axis + 1]) node$right else node$left

  neighbors <- nearest_within_ball_rec(next_branch, point, r, depth + 1, distanceFunction)
  neighbors <- append(neighbors, inside)

  if (abs(point[1, axis + 1] - node$point[1, axis + 1]) <= r) {
    neighbors <- append(neighbors, nearest_within_ball_rec(other_branch, point, r, depth + 1, distanceFunction))
  }

  neighbors
}
