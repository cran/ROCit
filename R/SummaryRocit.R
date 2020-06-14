#' @title Summary of rocit object
#'
#' @description Prints the summary of rocit object.
#'
#'
#' @param object An object of class \code{rocit}, returned by rocit function.
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#' @return NULL
#'
#'
#' @seealso \code{\link{rocit}}, \code{\link{print.rocit}}
#'
#'
#' @examples
#' data("Diabetes")
#' roc_empirical <- rocit(score = Diabetes$chol, class = Diabetes$dtest,
#'                        negref = "-")
#'# ---------------------
#'summary(roc_empirical)
#'
#'
#' @method summary rocit
#' @export
summary.rocit<- function(object, ... = NULL)
{
  msg <- NULL
  msg <- c(msg, paste("Method used:", object$method))
  msg <- c(msg, paste("Number of positive(s):", object$pos_count))
  msg <- c(msg, paste("Number of negative(s):", object$neg_count))
  msg <- c(msg, paste("Area under curve:", round(object$AUC, 4)))
  pdf <- data.frame(msg)
  names(pdf) <- NULL
  print(pdf, row.names = FALSE, right = FALSE, quote = FALSE)
}




