#' @title  Plot ROC Curve
#'
#' @description This function generates receiver operating
#' characteristic (ROC) curve. This is an S3 method for object of class
#' \code{"rocit"}, returned by \code{\link{rocit}} function.
#'
#'
#' @param x An object of class \code{"rocit"},
#' returned by \code{\link{rocit}} function.
#'
#'
#' @param col Colors to be used in the plot. If multiple specified,
#' the first color is used for the ROC curve, and the second color is used for
#' the chance line (\eqn{y = x} line), otherwise single color is used.
#'
#' @param legend A logical value indicating whether legends to appear in the plot.
#'
#' @param legendpos Position of the legend. A single keyword from
#' \code{"bottomright"}, \code{"bottom"},
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"},
#' \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"},
#' as in \code{\link[graphics]{legend}}. Ignored if
#' \code{legend} is \code{FALSE}.
#'
#' @param YIndex A logical value indicating whether optimal
#' \emph{Youden Index} (i.e where \eqn{|TPR - FPR| is maximum}) to
#' be marked in the plot.
#'
#' @param values A logical value, indicating whether values to be returned.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#' @return If \code{values = TRUE}, then AUC, Cutoff, TPR, FPR,
#' optimal Youden Index with
#' associated TPR, FPR, Cutoff are returned silently.
#'
#' @note Customized plots can be made by using the
#' returned values of the function.
#'
#' @examples
#' data("Loan")
#' score <- Loan$Score
#' class <- ifelse(Loan$Status == "FP", 0, 1)
#' rocit_emp <- rocit(score = score, class = class)
#' # -----------------------
#' plot(rocit_emp)
#' plot(rocit_emp, col = c(2,4), legendpos = "bottom",
#'      YIndex = FALSE, values = FALSE)
#' # -----------------------
#' rocit_bin <- rocit(score = score, class = class, method = "bin")
#' # -----------------------
#' plot(rocit_emp, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE)
#' lines(rocit_bin$TPR~rocit_bin$FPR, col = 2, lwd = 2)
#' legend("bottomright", col = c(1,2),
#'        c("Empirical ROC", "Binormal ROC"), lwd = 2)
#'
#' @method plot rocit
#' @export
plot.rocit <- function(x, col = c("#2F4F4F", "#BEBEBE"),
                       legend = TRUE, legendpos = "bottomright",
                       YIndex = TRUE, values = TRUE, ... = NULL)
{

  col <- rep(col, 2)

  y1 <- x$TPR
  x1 <- x$FPR
  graphics::plot(x1, y1, type = "l",
       ylab = "Sensitivity (TPR)",
       xlab = "1-Specificity (FPR)",
       col = col[1], lwd = 2)
  graphics::grid(col = "gray60")
  graphics::abline(0, 1, lwd = 2, col = col[2], lty = 2)
  diff <- y1 - x1
  maxIndex <- which.max(diff)
  xYI <- x1[maxIndex]
  yYI <- y1[maxIndex]
  cYI <- x$Cutoff[maxIndex]
  if(YIndex){
    graphics::points(x = xYI, y = yYI, pch = 16, cex = 1)
    graphics::points(x = xYI, y = yYI, pch = 3, cex = 3)
    graphics::text(x = (xYI + 0.2), y = (yYI - 0.1),
         "Optimal (Youden Index) point", font = 3)
  }


  methodName <- x$method
  methodName <- paste0(toupper(substr(methodName, 1, 1)),
                       substr(methodName, 2, nchar(methodName)))
  maintext <- paste(methodName, "ROC plot")

  if(legend){
    graphics::legend(legendpos,
           c(paste(methodName, "ROC curve"), "Chance line"),
           lty = c(1, 2), lwd = 2, col = col)
  }

  if(values){
    return(invisible(list(method = x$method,
                          AUC = x$AUC,
                          Cutoff = x$Cutoff,
                          `TPR/Sensitivity/Recall` = x$TPR,
                          `FPR/1-Specificity` = x$FPR,
                          `optimal Youden Index point` =
                            c(value = max(diff),
                              FPR = xYI,
                              TPR = yYI,
                              cutoff = cYI))))


  }
}

