#' @title Survival Probability
#'
#'
#' @description Function \code{getsurvival} calculates survival probability
#' from an object of class "density" at specified value.
#'
#'
#' @param x An object of class "density".
#' @param cutoff Value at which survival probability will be calculated.
#'
#' @details The survival function S, of a random variable \eqn{X} is defined by,
#' \deqn{S(X=x) = 1 - F(X=x)} where \eqn{F} is the cumulative density
#' function (CDF) of \eqn{X}.
#'
#'
#' @return Survival probability.
#'
#'
#' @section Comment:
#' \code{getsurvival} is used internally in other function(s) of \pkg{ROCit}.
#'
#' @examples
#' data("Loan")
#' k <- density(Loan$Income)
#' # What portion have income over 100,000
#' getsurvival(k,100000)
#'
#'
#' @export
getsurvival <- function(x, cutoff){
  if(!(methods::is(x, "density"))){
    stop("x is not of class \"density\" ")
  }
  density <- x
  densityxvals <- density$x
  indexes <- which(densityxvals >= cutoff)
  trapezoidarea(densityxvals[indexes], density$y[indexes])
}








