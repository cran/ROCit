
#' @title Log Odds of Probability
#' @description \code{logit} transforms probability value(s) into log-odds.
#'
#'
#' @param x A number or numeric vector from \eqn{[0 1]}.
#'
#' @return Log-odds of \code{x}, \eqn{log(x/(1-x))}.
#'
#' @name logit-deprecated
#' @usage logit(x)
#' @seealso \code{\link{ROCit-deprecated}}
#' @keywords internal
NULL

#' @rdname ROCit-deprecated
#' @section \code{logit}:
#' For \code{logit}, use \code{\link{qlogis}}.
#'
#' @export
logit <- function(x){
  .Deprecated("qlogis")
  "My return value"
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
#'
#'
#' @name invlogit-deprecated
#' @usage invlogit(x)
#' @seealso \code{\link{ROCit-deprecated}}
#' @keywords internal
NULL

#' @rdname ROCit-deprecated
#' @section \code{invlogit}:
#' For \code{invlogit}, use \code{\link{plogis}}.
#'
#' @export
invlogit <- function(x) {
  .Deprecated("plogis")
  "My return value"
}
















