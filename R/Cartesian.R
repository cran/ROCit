#' @title Cartesian Product of Two Vectors
#'
#'
#' @description Function \code{cartesian_2D} takes two vectors as input and
#' returns the two dimensional cartesian product.
#'
#'
#' @param array_x A vector, indicating the first set.
#' @param array_y A vector, indicating the second set.
#'
#'
#'
#' @return A matrix of \code{length(array_x)} * \code{length(array_y)} rows
#' and two columns. Each row indicates an ordered pair.
#'
#'
#' @section Comment:
#' \code{cartesian_2D} is used internally in other function(s) of \pkg{ROCit}.
#' Works if matrix/data frames are passed as arguments. However,
#' returns might not be valid if arguments are not one dimensional.
#'
#'
#' @examples
#' x <- seq(3)
#' y <- c(10,20,30)
#' cartesian_2D(x,y)
#'
#'
#' @export
cartesian_2D <- function(array_x, array_y) {
  cbind(x = rep(array_x, each = length(array_y)),
        array_y = rep(array_y, length(array_x)))
}



