#' @title Plot \code{"gainstable"} Object
#'
#' @description An S3 method to make different plots using
#' entries of gains table.
#'
#'
#'
#' @param x An object of class \code{"gainstable"}, created with
#' the function \code{\link{gainstable}}.
#'
#' @param y \code{NULL}.
#'
#' @param type Plot type. See "Details".
#'
#' @param col Colors to be used for plot.
#'
#' @param legend A logical value indicating whether legend to appear.
#' See "Details"
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @details Currently three types are available. \code{type = 1} shows
#' lift and cumulative lift against population depth. \code{type = 2}
#' shows response
#' rate and cumulative response rate against population depth.
#'  \code{type = 3} shows
#' cumulative capture rate of positive responses against population depth.
#' For \code{type} 1 and 2, three colors and for 3, two colors
#' are required.
#' If more than required specified, then first 3 (for \code{type} 1, 2) or
#' 2 (for \code{type} 3) colors are used. If less than required specified,
#' then
#' specified colors are repeated.
#' If \code{legend} is \code{TRUE},
#' then legend appears in the plot. For \code{type} 1 and 2, legend
#' position is \code{"topright"}, for 3, \code{"bottomright"}.
#' @examples
#' data("Loan")
#' class <- Loan$Status
#' score <- Loan$Score
#' rocit_emp <- rocit(score = score, class = class, negref = "FP")
#' # ----------------------------------------------------------------
#' gtable <- gainstable(rocit_emp)
#' # ----------------------------------------------------------------
#' plot(gtable)
#' plot(gtable, legend = FALSE)
#' plot(gtable, col = 2:4)
#' plot(gtable, type = 2, col = 2:4)
#' plot(gtable, type = 3, col = 2:3)

#' @seealso \code{\link{gainstable}}, \code{\link{rocit}}
#'
#' @method plot gainstable
#' @export
plot.gainstable <- function(x, y = NULL, type = 1,
                            col = c("#BEBEBE", "#26484F", "#8B4500"),
                            legend = TRUE, ... = NULL){

  if(length(type) > 1){
    stop("multiple types not allowed")
  }

  if(!(type == 1 || type == 2 || type == 3)){
    stop("invalid type (allowed 1, 2, 3)")
  }

  # lencol <- length(col)
  # if (length(unique(type)) > 1 && lencol < 3) {
  #   stop("At least three colors must be supplied for specified type")
  # }else{
  #   if(unique(type) == 3){
  #     if(lencol < 2) stop("At least two colors must be supplied for type 3")
  #   }else{
  #     if(lencol < 3){
  #       stop("At least three colors must be supplied for specified type")
  #     }
  #   }
  # }

  col <- rep(col, 3)

  liftclift <- function(){
    xx <- round(x$Depth * 100, 2)
    y1 <- x$Lift
    y2 <- x$CLift
    ymax <- max(c(max(y1), max(y2))) * 1.1

    graphics::plot(y1~xx, type = "o", pch = 15, col = col[1], lwd = 2,
         ylab = "Lift,  Cum. Lift", xlim=c(0,100),
         xlab = "Population depth (%)", ylim = c(0, ymax), xaxt = "none")
    graphics::lines(y2~xx, type = "o", pch=16, cex=1.2,
          col = col[2], lwd = 2)

    graphics::grid(ny = NULL, nx = NA, col = "gray50")
    xaxismark <- seq(10, 90, 20)
    graphics::abline(v = xaxismark, lty = 3, col = "gray50")
    xaxismark <- seq(0, 100, 20)
    graphics::abline(v = xaxismark, lty = 3, col = "gray50")
    graphics::axis(1, at = xaxismark, labels = xaxismark)
    graphics::abline(h=1, lwd = 1.5, lty = 2, col = col[3])
    if(legend){
      graphics::legend("topright", c("Lift", "Cumulative Lift"),
             lwd = 2, lty = 1, pch = 15:16,
             col = c(col[1], col[2]))
    }
  }

  respratecresprate <- function(){
    xx <- round(x$Depth * 100, 2)
    y1 <- x$RespRate
    y2 <- x$CRespRate
    ymax <- max(c(max(y1), max(y2))) * 1.1

    graphics::plot(y1~xx, type = "o", pch = 15, col = col[1], lwd = 2,
         ylab = "Resp. Rate,  Cum. Resp. Rate", xlim=c(0,100),
         xlab = "Population depth (%)", ylim = c(0, ymax), xaxt = "none")
    graphics::lines(y2~xx, type = "o", pch=16, cex=1.2,
          col = col[2], lwd = 2)

    graphics::grid(ny = NULL, nx = NA, col = "gray50")
    xaxismark <- seq(10, 90, 20)
    graphics::abline(v = xaxismark, lty = 3, col = "gray50")
    xaxismark <- seq(0,100,20)
    graphics::abline(v = xaxismark, lty = 3, col = "gray50")
    graphics::axis(1, at = xaxismark, labels = xaxismark)
    graphics::abline(h=y2[length(y2)], lwd = 1.5, lty = 2, col = col[3])

    if(legend){
      graphics::legend("topright", c("Response Rate", "Cumulative Response Rate"),
             lwd = 2, lty = 1, pch = 15:16,
             col = c(col[1], col[2]))
    }
  }

  ccaprate <- function(){
    xx <- round(x$Depth * 100, 2)
    y1 <- x$CCapRate
    ymax <- 1.1

    graphics::plot(y1~xx, type = "o", pch = 15, col = col[1], lwd = 2,
         ylab = "Cum. Cap. Rate", xlim=c(0,100),
         xlab = "Population depth (%)", ylim = c(0, ymax), xaxt = "none")

    graphics::grid(ny = NULL, nx = NA, col = "gray50")
    xaxismark <- seq(10, 90, 20)
    graphics::abline(v = xaxismark, lty = 3, col = "gray50")
    xaxismark <- seq(0, 100, 20)
    graphics::abline(v = xaxismark, lty = 3, col = "gray50")
    graphics::axis(1, at = xaxismark, labels = xaxismark)
    graphics::abline(0,0.01, lwd = 1.5, lty = 2, col = col[2])

    if(legend){
      graphics::legend("bottomright", c("Cumulative Capture Rate"),
             lwd = 2, lty = 1, pch = 15,
             col = col[1])
    }
  }
  if(type == 1){
    liftclift()
  }
  if(type == 2){
    respratecresprate()
  }
  if(type == 3){
    ccaprate()
  }
}
