#' @title Print \code{rocit} Object
#'
#' @param x An object of class \code{"rocit"},
#' returned by \code{\link{rocit}} function.
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#' @seealso \code{\link{rocit}}, \code{\link{summary.rocit}}
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
#' print(roc_empirical)
#' print(roc_binormal)
#'
#'
#' @method print rocit
#'
#' @export
print.rocit <- function(x, ... = NULL){
  msg <- NULL
  msg <- c(msg, paste("Method used:", x$method))
  msg <- c(msg, "===== Positive(s) =====")
  msg <- c(msg, paste("Number of positive(s):", x$pos_count))
  msg <- c(msg, paste("Mean of positive(s):",
                      format(mean(x$pos_D), digits = 4)))
  msg <- c(msg, paste("Variance of positive(s):",
                      format(var(x$pos_D), digits = 4)))
  msg <- c(msg, "===== Negative(s) =====")
  msg <- c(msg, paste("Number of negative(s):", x$neg_count))
  msg <- c(msg, paste("Mean of negative(s):",
                      format(mean(x$neg_D), digits = 4)))
  msg <- c(msg, paste("Variance of negative(s):",
                      format(var(x$neg_D), digits = 4)))
  msg <- c(msg, "===== AUC =====")
  msg <- c(msg, paste("Area under curve:", round(x$AUC, 4)))
  msg <- c(msg, "")

  msg <- c(msg, "=========================")
  pdf <- data.frame(msg)
  names(pdf) <- NULL
  print(pdf, row.names = FALSE, right = FALSE, quote = FALSE)


  fprtpr <- data.frame(cbind(FPR = x$FPR, TPR = x$TPR))

  if(nrow(fprtpr) < 21){
    hd <- fprtpr
    hd <- format(hd, digits = 4)
  }else{
    hd <- utils::head(fprtpr)
    hd <- format(hd, digits = 4)
    hd <- rbind(hd, c("...", "..."))
    hd <- rbind(hd, c("...", "..."))
    hd <- rbind(hd, c("...", "..."))
    tl <- utils::tail(fprtpr)
    tl <- format(tl, digits = 4)
    hd <- rbind(hd, tl)
    hd <- rbind(hd, c("Note:", "> 20 {FPR, TPR}"))
    hd <- rbind(hd, c("     ", "pairs estimated"))
  }

  hd <- data.frame(hd)
  print(hd, quote = FALSE, row.names = FALSE)
}



