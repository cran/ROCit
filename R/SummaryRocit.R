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
  mthd <- object$method
  mthd <- paste0(toupper(substr(mthd,1,1)),(substr(mthd,2,nchar(mthd))))

  row_1 <- paste(mthd, "ROC curve")
  row_2 <- paste("Number of postive responses : ", object$pos_count)
  row_3 <- paste("Number of negative responses : ", object$neg_count)
  row_4 <- paste("Area under curve : " , object$AUC)
  d <- as.data.frame(rbind(row_1, row_2, row_3, row_4))
  d <- format(d, justify = "left")
  row.names(d) <- NULL
  colnames(d) <- NULL
  print(d, row.names = FALSE)
}


