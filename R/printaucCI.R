#' @title Print Confidence Interval of AUC
#'
#'
#'
#' @param x An object of class \code{rocitaucci} created with
#' \code{\link{ciAUC}}.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#'
#' @examples
#' data("Diabetes")
#' logistic.model <- glm(as.factor(dtest)~chol+age+bmi,
#'                       data = Diabetes,family = "binomial")
#' score <- logistic.model$fitted.values
#' class <- logistic.model$y
#' # Make the rocit objects
#' rocit_bin <- rocit(score = score, class = class, method = "bin")
#' obj_1 <- ciAUC(rocit_bin, level = 0.9)
#' obj_2 <- ciAUC(rocit_bin, delong = TRUE)
#' obj_3 <- ciAUC(rocit_bin, delong = TRUE, logit = TRUE)
#' # Print
#' print(obj_1)
#' print(obj_2)
#' print(obj_3)

#' @method print rocitaucci
#'
#' @export
print.rocitaucci <- function(x, ... = NULL) {
  line1 <- paste("  ", "estimated AUC : ", x$AUC, sep = "")
  line2 <- paste("  ", "AUC estimation method : ",
                 x$AUC_est_method, sep = "")
  linespace <- " "
  line3 <- paste("  ", x$comment, sep = "")
  line4 <-
    paste("  ", "confidence level = ", x$conf_level * 100, "%",
          sep = "")
  line5 <-
    paste("  ", "lower = ", x$lower, "     ", "upper = ",
          x$upper, sep = "")
  retdf <-
    data.frame(rbind(line1, line2, linespace, line3, line4, line5))
  retdf <- format(retdf, justify = "left")
  row.names(retdf) <- NULL
  colnames(retdf) <- NULL
  print(retdf, row.names = FALSE)
}
