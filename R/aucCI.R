#' @title Confidence Interval of AUC
#'
#' @description See \code{\link{ciAUC.rocit}}.
#'
#'
#'
#' @param object An object of class \code{"rocit"}, returned by
#'  \code{\link{rocit}}.
#'
#' @param ... Arguments to be passed to methods.
#' See \code{\link{ciAUC.rocit}}.
#'
#'
#' @export
ciAUC <- function(object, ...){
  UseMethod("ciAUC")
}

#' @title Confidence Interval of AUC
#'
#'
#' @description \code{ciAUC} constructs confidence interval
#' of area under curve (AUC) of receiver operating characteristic (ROC)
#' curve. This is an S3 method defined for object of class \code{"rocit"}.
#'
#'
#' @seealso \code{\link{rocit}}, \code{\link{ciROC.rocit}}
#'
#'
#' @param object An object of class \code{"rocit"}, returned by
#'  \code{\link{rocit}}.
#' @param level Level of confidence, must be within the range (0 1).
#' Default is 0.95.
#' @param delong Logical; indicates whether DeLong formula should
#' be used to estimate the variance of AUC. Default is \code{FALSE}.
#'
#' @param logit Logical; indicates whether confidence interval of
#' logit transformed AUC should be evaluated first. Default is \code{FALSE}
#'
#' @param nboot Number of bootstrap samples, if bootstrap method is desired.
#' Default is NULL. If a numeric value is specified, overrides
#' \code{logit} and \code{delong} arguments.
#'
#' @param step Logical, default in \code{FALSE}.  See \code{\link{rocit}}.
#'
#'
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @return An object of class \code{"rocitaucci"}.
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
#' # Confidence interval of AUC
#' ciAUC(rocit_bin, level = 0.9)
#' ciAUC(rocit_bin, delong = TRUE, logit = TRUE)
#'
#' @method ciAUC rocit
#' @export
ciAUC.rocit <- function(object, level = 0.95, delong = FALSE, logit = FALSE,
                        nboot = NULL, step = FALSE, ... = NULL)
{
  if (level < 0 || level > 1) {
    stop("level must be within 0 and 1")
  }
  nY <- object$pos_count
  nYbar <- object$neg_count
  if (is.numeric(nboot)) {
    test1 <- nboot < 30
    test2 <- nboot != round(nboot)

    if (test1 | test2) {
      nboot <- 100
      warning("nboot is either less than 30 or non-integer, default 100 is used\n")
    }

    bootfun <- function(k)
    {
      set.seed(10 * k)
      pos_D <- sample(object$pos_D, nY, replace = T)
      set.seed(100 * k)
      neg_D <- sample(object$neg_D, nYbar, replace = T)


      score <- c(pos_D, neg_D)
      class <- c(rep(1, nY), rep(0, nYbar))


      return(rocit(score = score, class = class, step = step)$AUC)




      # convertedclass <- convertclass(class, reference = 0)
      # tempdata <- rankorderdata(score, convertedclass)
      # D <- tempdata[, 1]
      # Y <- tempdata[, 2]
      # n <- nY + nYbar
      # tempfun <- function(j)
      #   gettptnfpfn(x = Y, depth = j, nY, nYbar)
      # tempdata <- t(apply(matrix(1:n), 1, tempfun))
      #
      # Cutoff <- c(Inf, D,-Inf)
      # TP <- c(0, tempdata[, 1], tempdata[, 1][n])
      # FP <- c(0, tempdata[, 2], tempdata[, 2][n])
      # TN <- c(nYbar, tempdata[, 3], tempdata[, 3][n])
      # FN <- c(nY, tempdata[, 4], tempdata[, 4][n])
      # TPR <- TP / (TP + FN)
      # FPR <- FP / (FP + TN)
      # return(trapezoidarea(FPR, TPR))
    }


    loQuantile <- (1 - level) / 2
    upQuantile <- level + loQuantile
    seeds <- round(runif(nboot, 1, (1000 * nboot)))
    bootAUC <- apply(matrix(seeds), 1, bootfun)
    lower <- as.numeric(quantile(bootAUC, loQuantile))
    upper <- as.numeric(quantile(bootAUC, upQuantile))

    comment <-
      paste("bootstrap CI of AUC with ", nboot, " boot samples",
            sep = "")
    rval <- list(
      AUC = object$AUC,
      AUC_est_method = object$method,
      comment = comment,
      conf_level = level,
      lower = lower,
      upper = upper
    )

    class(rval) <- "rocitaucci"
    return(rval)

  } else{
    multiplier <-qnorm((1+level)/2)
    AUC <- object$AUC
    if (!delong) {
      Q1 <- AUC / (2 - AUC)
      Q2 <- 2 * AUC ^ 2 / (1 + AUC)
      SE_AUC <- sqrt((1 / (nY * nYbar)) * (AUC * (1 - AUC) + (nY - 1) *
                                             (Q1 - AUC ^ 2)
                                           + (nYbar - 1) * (Q2 - AUC ^ 2)))
    } else{
      pos_D <- object$pos_D
      neg_D <- object$neg_D
      SE_AUC <- sqrt((var(ecdf(pos_D)(neg_D))) / object$neg_count +
                       (var(ecdf(neg_D)(pos_D))) / object$pos_count)
    }

    if (!logit) {
      upper <- min((AUC + multiplier * SE_AUC), 1)
      lower <- max((AUC - multiplier * SE_AUC), 0)
    } else{
      logitAUC <- qlogis(AUC)
      multiplier2 <- SE_AUC / (AUC * (1 - AUC))
      logitupper <- logitAUC + multiplier * multiplier2
      logitlower <- logitAUC - multiplier * multiplier2
      upper <- plogis(logitupper)
      lower <- plogis(logitlower)
    }

    comment <- "CI of AUC"
    if (delong)
      comment <- paste(comment, ", delong method of variance used",
                       sep = "")
    if (logit)
      comment <- paste(comment,
                       ", logit tranformation applied", sep = "")


    rval <- list(
      AUC = object$AUC,
      AUC_est_method = object$method,
      comment = comment,
      conf_level = level,
      lower = lower,
      upper = upper
    )

    class(rval) <- "rocitaucci"
    return(rval)
  }
}
