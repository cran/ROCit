#' @title KS Plot
#'
#' @description See \code{\link{ksplot.rocit}}.
#'
#' @param object An object of class \code{"rocit"}, returned by
#' \code{\link{rocit}} function.
#'
#' @param ... Arguments to be passed to methods.
#' See \code{\link{ksplot.rocit}}.



#' @export
ksplot <- function(object,...){
  UseMethod("ksplot")
}


#' @title KS Plot
#'
#' @description Generates  cumulative density of
#' diagnostic variable in positive and negative responses.
#'
#' @details  This function plots the cumulative density functions
#' $F(c)$ and $G(c) of the
#' diagnostic variable in the negative and positive populations.
#' If the positive population have higher value then
#' negative curve ($F(c)$) ramps up quickly. The KS statistic is the maximum
#' difference of $F(c)$ and $G(c)$.
#'
#'
#' @param object An object of class \code{"rocit"}, returned by
#' \code{\link{rocit}} function.
#'
#' @param col Colors to be used for plot. Minimum three colors need
#' to be supplied for F(c), G(c) and KS Stat mark.
#'
#' @param lty Line types of the plots.
#'
#' @param legend A logical value indicating whether legends to
#' appear in the plot.
#'
#' @param legendpos Position of the legend. A single keyword from
#' \code{"bottomright"}, \code{"bottom"},
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"},
#' \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"},
#' as in \code{\link[graphics]{legend}}. Ignored if
#' \code{legend} is \code{FALSE}.
#'
#' @param values  A logical value, indicating whether values to be returned.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#' @return If \code{values = TRUE}, then  Cutoff, F(c), G(c), KS stat,
#' KS Cutoff  are returned silently.
#'
#' @note Customized plots can be made by using the
#' returned values of the function.
#'
#' @examples
#' data("Diabetes")
#' logistic.model <- glm(as.factor(dtest)~chol+age+bmi,
#'                       data = Diabetes,family = "binomial")
#' class <- logistic.model$y
#' score <- qlogis(logistic.model$fitted.values)
#' # -------------------------------------------------------------
#' roc_emp <- rocit(score = score, class = class) # default method empirical
#' # -------------------------------------------------------------
#' kplot1 <- ksplot(roc_emp)
#' message("KS Stat (empirical) : ", kplot1$`KS stat`)
#' message("KS Stat (empirical) cutoff : ", kplot1$`KS Cutoff`)
#'
#'
#' @method ksplot rocit
#' @export
ksplot.rocit <- function(object, col=c("#26484F", "#BEBEBE", "#FFA54F"),
                         lty = c(1,1,1),legend = T,
                         legendpos = "bottomright",
                         values = T,... = NULL)
{
  if(length(col) < 3){
    col <- rep(col,3)
  }

  if(length(lty) < 3){
    lty <- rep(lty,3)
  }



  filtertemp <- function(array, index){
    array[-index]
  }



  if(object$method == "empirical") {
    # all empirical code goes here
    Cutoff <- object$Cutoff
    finiteIndex <- which(is.finite(Cutoff))
    Cutoff <- Cutoff[finiteIndex]
    F_Ybar <- 1 - object$FPR[finiteIndex]
    G_Y <- 1 - object$TPR[finiteIndex]

    dropIndex <- which(G_Y < 0.0001)
    if(length(dropIndex)>0){
      Cutoff <- filtertemp(Cutoff, dropIndex)
      F_Ybar <- filtertemp(F_Ybar, dropIndex)
      G_Y <- filtertemp(G_Y, dropIndex)
    }

    dropIndex <- which(F_Ybar < 0.0001)
    if(length(dropIndex)>0){
      Cutoff <- filtertemp(Cutoff, dropIndex)
      F_Ybar <- filtertemp(F_Ybar, dropIndex)
      G_Y <- filtertemp(G_Y, dropIndex)
    }

    diff <- abs(G_Y - F_Ybar)
    KS_stat <- max(diff)
    KSstatIndex <- which.max(diff)

    xrange <- max(Cutoff) - min(Cutoff)
    xmin <- min(Cutoff) - 0.1 * xrange
    xmax <- max(Cutoff) + 0.1 * xrange

    graphics::plot(F_Ybar~Cutoff, type = "l", col = col[1], lty = lty[1],
         xlim = c(xmin, xmax), ylim = c(0, 1),
         xlab = "", lwd = 2, ylab = "")
    graphics::lines(G_Y ~ Cutoff, lwd = 2, col = col[2], lty = lty[2])
    graphics::grid(col="gray60")
    graphics::abline(v = Cutoff[KSstatIndex], lty = 3)
    graphics::segments(
      x0 = Cutoff[KSstatIndex],
      y0 = G_Y[KSstatIndex],
      x1 = Cutoff[KSstatIndex],
      y1 = F_Ybar[KSstatIndex],
      lwd = 3,
      col = col[3], lty = lty[3])

    graphics::mtext("Cutoff (c)", side = 1, line = 2.5)
    graphics::mtext("G(c) and F(c)", side = 2, line = 2.5)
    graphics::mtext("G: CDF of +ve scores",
          side = 3, font = 3, line = 1, cex = 0.85)
    graphics::mtext("F: CDF of -ve scores",
          side = 3, font = 3, line = 0, cex = 0.85)
    if(legend){
      graphics::legend(legendpos,
             c("F(c)", "G(c)", "KS stat"),
             lwd = 2,
             col = col[1:3], lty = lty[1:3])
    }

    if(values){
      return(invisible(list(method = object$method,
                            Cutoff = Cutoff,
                            `F(c)` = F_Ybar,
                            `G(c)` = G_Y,
                            `KS stat` = KS_stat,
                            `KS Cutoff` = Cutoff[KSstatIndex])))
    }else{return()}

  }


  if(object$method == "binormal"){
    negMU <- object$param$negparam$mu
    negSD <- object$param$negparam$sigma
    posMU <- object$param$posparam$mu
    posSD <- object$param$posparam$sigma

    minD <- min(c((negMU - 3 * negSD), (posMU - 3 * posSD)))
    maxD <- max(c((negMU + 3 * negSD), (posMU + 3 * posSD)))


    Cutoff <- seq(minD, maxD, (maxD - minD) / 99)
    F_Ybar <- pnorm(Cutoff,
                    mean = object$param$negparam$mu,
                    sd = object$param$negparam$sigma)
    G_Y <- pnorm(Cutoff,
                 mean = object$param$posparam$mu,
                 sd = object$param$posparam$sigma)

    dropIndex <- which(G_Y < 0.0001)
    if(length(dropIndex)>0){
      Cutoff <- filtertemp(Cutoff, dropIndex)
      F_Ybar <- filtertemp(F_Ybar, dropIndex)
      G_Y <- filtertemp(G_Y, dropIndex)
    }

    dropIndex <- which(F_Ybar < 0.0001)
    if(length(dropIndex)>0){
      Cutoff <- filtertemp(Cutoff, dropIndex)
      F_Ybar <- filtertemp(F_Ybar, dropIndex)
      G_Y <- filtertemp(G_Y, dropIndex)
    }

    diff <- abs(G_Y - F_Ybar)
    KS_stat <- max(diff)
    KSstatIndex <- which.max(diff)

    xrange <- max(Cutoff) - min(Cutoff)
    xmin <- min(Cutoff) - 0.1 * xrange
    xmax <- max(Cutoff) + 0.1 * xrange

    graphics::plot(F_Ybar~Cutoff, type = "l", col = col[1], lty = lty[1],
         xlim = c(xmin, xmax), ylim = c(0, 1),
         xlab = "", lwd = 2, ylab = "")
    graphics::lines(G_Y ~ Cutoff, lwd = 2, col = col[2], lty = lty[2])
    graphics::grid(col="gray60")
    graphics::abline(v = Cutoff[KSstatIndex], lty = 3)
    graphics::segments(
      x0 = Cutoff[KSstatIndex],
      y0 = G_Y[KSstatIndex],
      x1 = Cutoff[KSstatIndex],
      y1 = F_Ybar[KSstatIndex],
      lwd = 3,
      col = col[3], lty = lty[3])

    graphics::mtext("Cutoff (c)", side = 1, line = 2.5)
    graphics::mtext("G(c) and F(c)", side = 2, line = 2.5)
    graphics::mtext("G: CDF of +ve scores",
          side = 3, font = 3, line = 1, cex = 0.85)
    graphics::mtext("F: CDF of -ve scores",
          side = 3, font = 3, line = 0, cex = 0.85)
    if(legend){
      graphics::legend(legendpos,
             c("F(c)", "G(c)", "KS stat"),
             lwd = 2,
             col = col[1:3], lty = lty[1:3])
    }

    if(values){
      return(invisible(list(method = object$method,
                            Cutoff = Cutoff,
                            `F(c)` = F_Ybar,
                            `G(c)` = G_Y,
                            `KS stat` = KS_stat,
                            `KS Cutoff` = Cutoff[KSstatIndex])))
    }else{return()}
  }


  if(object$method == "non-parametric"){
    # all nonparametric code goes here
    Cutoff <- object$Cutoff
    finiteIndex <- which(is.finite(Cutoff))

    Cutoff <- Cutoff[finiteIndex]
    F_Ybar <- 1 - object$FPR[finiteIndex]
    G_Y <- 1 - object$TPR[finiteIndex]

    dropIndex <- which(G_Y < 0.0001)
    if(length(dropIndex)>0){
      Cutoff <- filtertemp(Cutoff, dropIndex)
      F_Ybar <- filtertemp(F_Ybar, dropIndex)
      G_Y <- filtertemp(G_Y, dropIndex)
    }

    dropIndex <- which(F_Ybar < 0.0001)
    if(length(dropIndex)>0){
      Cutoff <- filtertemp(Cutoff, dropIndex)
      F_Ybar <- filtertemp(F_Ybar, dropIndex)
      G_Y <- filtertemp(G_Y, dropIndex)
    }

    diff <- abs(G_Y - F_Ybar)
    KS_stat <- max(diff)
    KSstatIndex <- which.max(diff)

    xrange <- max(Cutoff) - min(Cutoff)
    xmin <- min(Cutoff) - 0.1 * xrange
    xmax <- max(Cutoff) + 0.1 * xrange

    graphics::plot(F_Ybar~Cutoff, type = "l", col = col[1], lty = lty[1],
         xlim = c(xmin, xmax), ylim = c(0, 1),
         xlab = "", lwd = 2, ylab = "")
    graphics::lines(G_Y ~ Cutoff, lwd = 2, col = col[2], lty = lty[2])
    graphics::grid(col="gray60")
    graphics::abline(v = Cutoff[KSstatIndex], lty = 3)
    graphics::segments(
      x0 = Cutoff[KSstatIndex],
      y0 = G_Y[KSstatIndex],
      x1 = Cutoff[KSstatIndex],
      y1 = F_Ybar[KSstatIndex],
      lwd = 3,
      col = col[3], lty = lty[3])

    graphics::mtext("Cutoff (c)", side = 1, line = 2.5)
    graphics::mtext("G(c) and F(c)", side = 2, line = 2.5)
    graphics::mtext("G: CDF of +ve scores",
          side = 3, font = 3, line = 1, cex = 0.85)
    graphics::mtext("F: CDF of -ve scores",
          side = 3, font = 3, line = 0, cex = 0.85)
    if(legend){
      graphics::legend(legendpos,
             c("F(c)", "G(c)", "KS stat"),
             lwd = 2,
             col = col[1:3], lty = lty[1:3])
    }

    if(values){
      return(invisible(list(method = object$method,
                            Cutoff = Cutoff,
                            `F(c)` = F_Ybar,
                            `G(c)` = G_Y,
                            `KS stat` = KS_stat,
                            `KS Cutoff` = Cutoff[KSstatIndex])))
    }else{return()}
  }
}
