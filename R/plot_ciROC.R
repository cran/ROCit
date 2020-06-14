#' @title  Plot ROC Curve with confidence limits
#'
#' @description This function plots  receiver operating
#' characteristic (ROC) curve with confidence limits.
#' This is an S3 method for object of class
#' \code{"rocci"}, returned by \code{\link{ciROC.rocit}} function.
#'
#'
#'
#' @param x An object of class \code{"rocci"},
#' returned by \code{\link{ciROC.rocit}} function.
#'
#' @param col Color(s) to be used for the plot. First two colors are used
#' for the ROC curve and confidence limits if multiple colors supplied. Same
#' color is used if single color supplied.
#'
#' @param lty The line type. Same as in \code{\link{par}}. First two or one are
#' used (like \code{col}) depending on the length of \code{lty}.
#'
#' @param lwd The line width. Same as in \code{\link{par}}. First two or one are
#' used (like \code{col}) depending on the length of \code{lwd}.
#'
#' @param grid Logical, indicating whether to add rectangular grid. Calls
#' \code{\link{grid}} with default settings.
#'
#'
#' @param legend Logical, indicating whether to add legends to the plot.
#'
#' @param legendpos Position of the legend. A single keyword from
#' \code{"bottomright"}, \code{"bottom"},
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"},
#' \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"},
#' as in \code{\link[graphics]{legend}}. Ignored if
#' \code{legend} is \code{FALSE}.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @examples
#' score <- c(rnorm(300,30,15), rnorm(300,50,15))
#' class <- c(rep(0,300), rep(1,300))
#' rocit_object <- rocit(score = score, class = class, method = "bi")
#' rocci_object <- ciROC(rocit_object)
#' # ---------------------------
#' plot(rocci_object)
#' plot(rocci_object, col = c(2,4))
#' plot(rocci_object, col = c(2,4), legendpos = "bottom", lty = c(1,3))
#'

#'
#' @seealso \code{\link{ciROC}}, \code{\link{rocit}}, \code{\link{plot.rocit}}
#'
#'
#' @export
plot.rocci <- function(x, col = c("#2F4F4F","#404040"), lty = c(1,2),
                       lwd = c(2,1), grid = TRUE, legend = TRUE,
                       legendpos = "bottomright", ... = NULL)
{
  col <- rep(col, 2)
  lty <- rep(lty, 2)
  lwd <- rep(lwd, 2)

  graphics::plot(x$TPR~x$FPR, xlab = "1 - Specificity (FPR)",
       ylab = "Sensitivity (TPR)", type = "l",
       col = col[1], lwd = lwd[1], lty = lty[1])
  if(grid) graphics::grid()
  graphics::lines(x$LowerTPR~x$FPR, lty = lty[2], lwd = lwd[2], col = col[2])
  graphics::lines(x$UpperTPR~x$FPR, lty = lty[2], lwd = lwd[2], col = col[2])
  if(legend){
    method <- x$`ROC estimation method`
    method <- paste0(toupper(substr(method,1,1)),substr(method,2,nchar(method)))
    kotha_1 <- paste(method, "ROC curve")
    level <- x$`Confidence level`
    level <- paste0(round(as.numeric(substr(level, 1,nchar(level)-1)),2),"%")
    kotha_2 <- paste(level, "Confidence Interval")
    graphics::legend(legendpos, c(kotha_1, kotha_2), lty = c(lty[1], lty[2]),
           lwd = c(lwd[1], lwd[2]), col = c(col[1], col[2]))
  }
}
