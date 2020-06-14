#' @title Approximate Area with Trapezoid Rule

#' @description \code{trapezoidarea} calculates the approximated area
#' under curve, using trapezoidal rule.
#' @param x,y Numeric vectors of same length, representing the \code{x} and \code{y} coordinates
#' of the points.
#' @return  Numeric value of the area under curve approximated with trapezoid rule.
#' @details The function approximates the area bounded by the following 4 curves:
#' \deqn{x = a, x = b, y = 0, y = f(x)}
#'
#' @details \eqn{a} and \eqn{b} are set at the min and max value of given \code{x}
#' coordinates. \eqn{(x, y)} are some points on the \eqn{y = f(x)} curve.
#'
#'
#'
#' @section Comment:
#' \code{trapezoidarea} is used internally in other function(s) of \pkg{ROCit}.
#'
#' @examples # Area under rectangle -----------------
#' @examples trapezoidarea(seq(0, 10), rep(1, 11))
#'
#' @examples # Area under triangle ------------------
#' @examples trapezoidarea(seq(0, 10), seq(0, 10))
#'
#' @examples # Area under normal pdf ----------------
#' @examples x_vals <- seq(-3, 3, 0.01); y_vals <- dnorm(x_vals)
#' @examples trapezoidarea(x = x_vals, y = y_vals) # theoretically 1
#'
#' @export
trapezoidarea <- function(x,y){
  if(length(x)!=length(y)){
    stop("x and y differ in lengths")
  }
  cond1 <- methods::is(x, "integer")
  cond2 <- methods::is(x, "numeric")
  cond3 <- methods::is(y, "integer")
  cond4 <- methods::is(y, "numeric")
  if(!((cond1 || cond2) && (cond3 || cond4))){
    stop("x and y should be integer or numeric")
  }
  mydata <- as.data.frame(cbind(x,y))
  sorted <- mydata[order(x),]
  x <- sorted$x
  y <- sorted$y
  return(sum(diff(x)*(y[-1]+y[-length(y)]))/2)
}

#' @title ML Estimate of Normal Parameters

#' @description The function calculates the maximum likelihood (ML) estimates
#' of the two parameters \eqn{\mu} and \eqn{\sigma}, when a set of numbers are
#' assumed to be normally distributed.
#' @param x A numeric vector.
#'
#' @return A \code{"list"} object of two numeric components,
#' \eqn{\mu} and \eqn{\sigma}.
#'
#' @details If a set of observations
#' are assumed to be normally
#' distributed, two parameters, mean \eqn{\mu} and the variance (the square
#' of \eqn{\sigma})
#' are to be estimated. In theory, the ML estimate of \eqn{\mu} is the mean of the
#' observations. And the ML estimate of square of \eqn{\sigma} is the mean
#' squared deviation of the observations from the estimated \eqn{\mu}.


#' @section Comment:
#' \code{MLestimates} is used internally in other function(s) of \pkg{ROCit}.
#'

#' @examples # Find the two parameters
#' @examples set.seed(10)
#' @examples points <- rnorm(200, 10, 5)
#' @examples ML <- MLestimates(points)
#' @examples message("The ML estimates are: mean = ", round(ML$mu, 3),
#' @examples         " , SD = ", round(ML$sigma, 3))
#'
#' @examples #-----------------------------------------
#'
#' @examples # Superimpose smooth curve over hostogram
#' @examples set.seed(100)
#' @examples x <- rnorm(400)
#' @examples hist(x, probability = TRUE, col = "gray90")
#' @examples ML <- MLestimates(x)
#' @examples x <- seq(-3, 3, 0.01)
#' @examples density <- dnorm(x, mean = ML$mu, sd = ML$sigma)
#' @examples lines(density~x, lwd = 2)
#'
#' @export
MLestimates <- function(x){
  mu <- mean(x)
  sigma <- sqrt(mean((x - mu)^2))
  list(mu = mu, sigma = sigma)
}



