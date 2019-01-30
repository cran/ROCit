#' @title Log Odds of Probability
#'
#' @description \code{logit} transforms probability value(s) into log-odds.
#'
#' @param x A number or numeric vector from \eqn{[0 1]}.
#'
#' @return Log-odds of \code{x}, \eqn{log(x/(1-x))}.
#'
#'
#'
#' @examples logit(0.2)
#'
#' @examples set.seed(1)
#' @examples logit(runif(10, 0, 1))
#'
#' @export
logit <- function(x){
  log(x/(1-x))
}



#' @title Logistic Transformation
#'
#' @description \code{invlogit} returns the logistic transformed value.
#'
#'
#' @param x A number or numeric vector from \eqn{(-Inf Inf)}.
#'
#' @return Logistic transformed value of \code{x}, \eqn{1/(1+exp(-x))}
#'
#'
#' @examples invlogit(0.5)
#'
#' @examples invlogit(c(-Inf, Inf))
#'
#' @examples set.seed(10)
#' @examples invlogit(runif(10, -3, 3))
#'
#' @export
invlogit <- function(x) {
  1/(1+exp(-x))
}

