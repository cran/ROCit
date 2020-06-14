#' @title Print \code{rocci} Object
#'
#' @param x An object of class \code{"rocci"},
#' returned by \code{\link{ciROC}} function.
#'
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#' @seealso \code{\link{ciROC}}, \code{\link{rocit}}
#'
#'
#' @examples
#' data("Diabetes")
#' roc_empirical <- rocit(score = Diabetes$chol, class = Diabetes$dtest,
#'                        negref = "-") # default method empirical
#' roc_binormal <- rocit(score = Diabetes$chol, class = Diabetes$dtest,
#'                      negref = "-", method = "bin")
#'
#'# ---------------------
#' print(ciROC(roc_empirical))
#' print(ciROC(roc_binormal))
#'
#'
#'
#' @method print rocci
#'
#' @export
print.rocci <- function(x, ... = NULL){
  msg <- NULL
  msg <- c(msg, paste("Method of ROC estimation:", x$'ROC estimation method'))
  msg <- c(msg, paste("Confidence level:", x$'Confidence level'))
  msg <- c(msg, "=========================")
  pdf <- data.frame(msg)
  names(pdf) <- NULL
  print(pdf, row.names = FALSE, right = FALSE, quote = FALSE)


  pdf2 <- cbind(FPR = x$FPR, TPR = x$TPR,
                TPR_Lower_Bound = x$LowerTPR,
                TPR_Upper_Bound = x$UpperTPR)

  if(nrow(pdf2) < 21){
    hd <- pdf2
    hd <- format(hd, digits = 5)
  }else{
    hd <- utils::head(pdf2)
    hd <- format(hd, digits = 5)
    hd <- rbind(hd, c("...", "...", "...", "..."))
    hd <- rbind(hd, c("...", "...", "...", "..."))
    hd <- rbind(hd, c("...", "...", "...", "..."))
    tl <- utils::tail(pdf2)
    tl <- format(tl, digits = 5)
    hd <- rbind(hd, tl)
    hd <- rbind(hd, c("Note:", "> 20 estimation", "points", ""))
  }
  hd <- data.frame(hd)
  print(hd, quote = FALSE, row.names = FALSE)
}



