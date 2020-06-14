

#' @title Performance Metrics of Binary Classifier
#'
#' @description See \code{\link{measureit.default}},
#' \code{\link{measureit.rocit}}
#'
#' @param ... Arguments to be passed to methods.
#' See \code{\link{measureit.default}}, \code{\link{measureit.rocit}}.
#'
#' @export
measureit <- function(...){
  UseMethod("measureit")
}

#' @title Performance Metrics of Binary Classifier
#'
#' @description This function  computes various performance metrics
#' at different cutoff values.
#'
#'
#'
#' @param score An numeric array of diagnostic score.
#' @param class An array of equal length of score,
#' containing the class of the observations.
#' @param negref The reference value, same as the
#' \code{reference} in \code{\link{convertclass}}.
#' Depending on the class of \code{x},
#' it can be numeric or character type. If specified, this value
#' is converted to 0 and other is converted to 1. If NULL, reference is
#' set alphabetically.
#'
#' @param measure The performance metrics to be evaluated. See "Details"
#' for available options.
#'
#' @param step Logical, default in \code{FALSE}.The algorithm used in
#' \code{measureit} first rank orders the
#' data and calculates TP, FP, TN, FN by treating all predicted
#' up to certain level as positive. If \code{step} is \code{TRUE},
#' then these numbers are evaluated for all the observations,
#' regardless of tie in the data. If \code{step} is
#' \code{FALSE}, only one set of stats are retained for a single value of
#' \code{D}.
#'
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @return An object of class \code{"measureit"}. By default it contains the
#' followings:
#' \item{Cutoff}{Cutoff at which metrics are evaluated.}
#' \item{Depth}{What portion of the observations fall on or above the cutoff.}
#' \item{TP}{Number of true positives, when the observations having
#' score equal or greater than cutoff are predicted positive.}
#' \item{FP}{Number of false positives, when the observations having
#' score equal or greater than cutoff are predicted positive.}
#' \item{TN}{Number of true negatives, when the observations having
#' score equal or greater than cutoff are predicted positive.}
#' \item{FN}{Number of false negatives, when the observations having
#' score equal or greater than cutoff are predicted positive.}
#'
#' When other metrics are called via \code{measure}, those also appear
#' in the return in the order they are listed above.
#'
#'
#'
#'
#' @details Various performance metrics for binary classifier are
#' available that are cutoff specific. For a certain cutoff value, all the
#' observations having score equal or greater are predicted as
#' positive. Following metrics can be called for
#' via \code{measure} argument:
#' \itemize{
#' \item{\code{ACC:} Overall accuracy of  classification =
#' \eqn{P(Y = \hat{Y})} = (TP + TN) / (TP + FP + TN + FN)}
#' \item{\code{MIS:} Misclassification rate = \eqn{1 - ACC}}
#' \item{\code{SENS:} Sensitivity = \eqn{P(\hat{Y} = 1|Y = 1) = TP / (TP + FN)}}
#' \item{\code{SPEC:} Specificity = \eqn{P(\hat{Y} = 0|Y = 0) = TN / (TN + FP)}}
#' \item{\code{PREC:} Precision  = \eqn{P(Y = 1| \hat{Y} = 1) = TP / (TP + FP)}}
#' \item{\code{REC:} Recall. Same as sensitivity.}
#' \item{\code{PPV:} Positive predictive value. Same as precision}
#' \item{\code{NPV:} Positive predictive value = \eqn{P(Y = 0| \hat{Y} = 0) =
#' TN / (TN + FN)}}
#' \item{\code{TPR:} True positive rate. Same as sensitivity.}
#' \item{\code{FPR:} False positive rate. Same as \eqn{1 - specificity}.}
#' \item{\code{TNR:} True negative rate. Same as specificity.}
#' \item{\code{FNR:} False negative rate = \eqn{P(\hat{Y} = 0|Y = 1) =
#' FN / (FN +TP)}}
#' \item{\code{pDLR:} Positive diagnostic likelihood ratio = \eqn{TPR / FPR}}
#' \item{\code{nDLR:} Negative diagnostic likelihood ratio = \eqn{FNR / TNR}}
#' \item{\code{FSCR:} F-score, defined as \eqn{2 * (PPV * TPR) / (PPV + TPR)}}
#' }
#'     \emph{Exact match} is required. If the values passed in the
#' \code{measure} argument do not match with the
#' available options, then ignored.
#'
#'
#' @seealso \code{\link{measureit.rocit}}, \code{\link{print.measureit}}
#'
#' @note The algorithm is designed for complete cases. If NA(s) found in
#' either \code{score} or \code{class}, then removed.
#'
#'      Internally sorting is performed, with respect to the
#' \code{score}. In case of tie, sorting is done with respect to \code{class}.
#' @examples
#' data("Diabetes")
#' logistic.model <- glm(factor(dtest)~chol+age+bmi,
#'                       data = Diabetes,family = "binomial")
#' class <- logistic.model$y
#' score <- logistic.model$fitted.values
#' # -------------------------------------------------------------
#' measure <- measureit(score = score, class = class,
#'                      measure = c("ACC", "SENS", "FSCR"))
#' names(measure)
#' plot(measure$ACC~measure$Cutoff, type = "l")
#' plot(measure$TP~measure$FP, type = "l")
#'
#' @author Riaz Khan, \email{mdriazahmed.khan@@jacks.sdstate.edu}
#' @method measureit default
#' @export
measureit.default <- function(score, class, negref = NULL,
                    measure = c("ACC", "SENS"), step = FALSE, ... = NULL) {
  if(length(score) != length(class)) {
    stop("score and class differ in length")
  }
  scoretest1 <- class(score) == "integer"
  scoretest2 <- class(score) == "numeric"

  if(!(scoretest1 || scoretest2)){
    stop("score must be numeric or integer")
  }

  scoreNA <- which(is.na(score))
  classNA <- which(is.na(class))
  unionNA <- union(scoreNA, classNA)


  na_cases <- FALSE
  if(length(unionNA) > 0){
    # message("removing NA(s) from score and/or class")
    score <- score[-unionNA]
    class <- class[-unionNA]
    na_cases <- TRUE
    na_msg <- "NA(s) in score and/or class, removed from the data."
    # message("NA(s) removed")
  }


  if(!is.null(negref)){
    if(!(negref %in% unique(class))){
      stop("(negative) reference not found in class")
    }
  }



  convertedclass <- convertclass(class, reference = negref)
  tempdata <- rankorderdata(score, convertedclass)
  D <- tempdata[, 1]
  Y <- tempdata[, 2]

  Ybar <- 1 - Y
  # DY <- D[which(Y == 1)]
  # DYbar <- D[which(Y == 0)]
  nY <- sum(Y)
  nYbar <- sum(Ybar)

  TP <- cumsum(Y)
  FP <- cumsum(Ybar)
  TN <- nYbar - FP
  FN <- nY - TP
  Depth <- c(1:(nY + nYbar)) / (nY + nYbar)
  Cutoff <- D

  if(!step){
    df <- data.frame(cbind(index = 1:(nY + nYbar), D))
    revdf <- df[rev(row.names(df)), ]
    keep <- rev(revdf$index[!duplicated(revdf$D)])
    TP <- TP[keep]
    FP <- FP[keep]
    TN <- TN[keep]
    FN <- FN[keep]
    Depth <- Depth[keep]
    Cutoff <- Cutoff[keep]
  }

  Depth <- c(0, Depth)
  Cutoff <- c(Inf, Cutoff)
  TP <- c(0, TP)
  FP <- c(0, FP)
  TN <- c(nYbar, TN)
  FN <- c(nY, FN)


  # nY <- sum(Y)
  # n <- length(D)
  # nYbar <- n - nY
  # tempfun <- function(j){
  #   gettptnfpfn(x=Y, depth = j, nY, nYbar)
  # }
  # tempdata <- t(apply(matrix(1:n), 1, tempfun))
  # Cutoff <- c(Inf, D,-Inf)
  # Depth <- c(0, (1:n) / n, 1)
  # TP <- c(0, tempdata[, 1], tempdata[, 1][n])
  # FP <- c(0, tempdata[, 2], tempdata[, 2][n])
  # TN <- c(nYbar, tempdata[, 3], tempdata[, 3][n])
  # FN <- c(nY, tempdata[, 4], tempdata[, 4][n])




  tempdata <- cbind(Cutoff = Cutoff, Depth = Depth,
                    TP = TP, FP = FP, TN = TN, FN = FN)



  if("ACC" %in% measure) {
    ACC <- (TP + TN) / (TP + FP + TN + FN)
    tempdata <- cbind(tempdata, ACC)
  }
  if ("MIS" %in% measure) {
    if ("ACC" %in% measure) {
      MIS <- 1 - ACC
    } else{
      MIS <- (FP + FN) / (TP + FP + TN + FN)
    }
    tempdata <- cbind(tempdata, MIS)
  }
  if ("SENS" %in% measure) {
    SENS <- TP / (TP + FN)
    tempdata <- cbind(tempdata, SENS)
  }
  if ("SPEC" %in% measure) {
    SPEC <- TN / (TN + FP)
    tempdata <- cbind(tempdata, SPEC)
  }
  if ("PREC" %in% measure) {
    PREC <- TP / (TP + FP)
    tempdata <- cbind(tempdata, PREC)
  }
  if ("REC" %in% measure) {
    if ("SENS" %in% measure) {
      REC <- SENS
    } else{
      REC <- TP / (TP + FN)
    }
    tempdata <- cbind(tempdata, REC)
  }
  if ("PPV" %in% measure) {
    if ("PREC" %in% measure) {
      PPV <- PREC
    } else{
      PPV <- TP / (TP + FP)
    }
    tempdata <- cbind(tempdata, PPV)
  }
  if ("NPV" %in% measure) {
    NPV <- TN / (TN + FN)
    tempdata <- cbind(tempdata, NPV)
  }
  if ("TPR" %in% measure) {
    if ("SENS" %in% measure) {
      TPR <- SENS
    } else{
      if ("REC" %in% measure) {
        TPR <- REC
      } else{
        TPR <- TP / (TP + FN)
      }
    }
    tempdata <- cbind(tempdata, TPR)
  }
  if ("FPR" %in% measure) {
    if ("SPEC" %in% measure) {
      FPR <- 1 - SPEC
    } else{
      FPR <- FP / (FP + TN)
    }
    tempdata <- cbind(tempdata, FPR)
  }
  if ("TNR" %in% measure) {
    if ("SPEC" %in% measure) {
      TNR <- SPEC
    } else{
      if ("FPR" %in% measure) {
        TNR <- 1 - FPR
      } else{
        TNR <- TN / (TN + FP)
      }
    }
    tempdata <- cbind(tempdata, TNR)
  }
  if ("FNR" %in% measure) {
    test1 <- "SENS" %in% measure
    test2 <- "TPR" %in% measure
    test3 <- "REC" %in% measure
    if (test1 || test2 || test3) {
      if (test1) {
        FNR <- 1 - SENS
      } else{
        if (test2) {
          FNR <- 1 - TPR
        } else{
          FNR <- 1 - REC
        }
      }
    } else{
      FNR <- FN / (FN + TP)
    }
    tempdata <- cbind(tempdata, FNR)
  }
  if ("pDLR" %in% measure) {
    pDLR <- TP * (FP + TN) / (FP * (TP + FN))
    tempdata <- cbind(tempdata, pDLR)
  }
  if ("nDLR" %in% measure) {
    nDLR <- FN * (TN + FP) / (TN * (FN + TP))
    tempdata <- cbind(tempdata, nDLR)
  }
  if("FSCR" %in% measure){
    TPRtest1 = "SENS" %in% measure
    TPRtest2 = "TPR" %in% measure
    TPRtest3 = "REC" %in% measure
    if(TPRtest1 || TPRtest2 || TPRtest3){
      if(TPRtest1){
        FSCRterm1 <- SENS
      }else{
        if(TPRtest2){
          FSCRterm1 <- TPR
        }else{
          FSCRterm1 <- REC
        }
      }
    }else{
      FSCRterm1 <- TP / (TP + FN)
    }
    PPVtest1 <- "PREC" %in% measure
    PPVtest2 <- "PPV" %in% measure
    if(PPVtest1 || PPVtest2){
      if(PPVtest1){
        FSCRterm2 <- PREC
      }else{
        FSCRterm2 <- PPV
      }
    }else{
      FSCRterm2 <- TP / (TP + FP)
    }
    FSCR <- 2 * (FSCRterm1 * FSCRterm2)/(FSCRterm1 + FSCRterm2)
    tempdata <- cbind(tempdata, FSCR)
  }
  retval <- data.frame(tempdata)
  class(retval) = "measureit"
  if(na_cases){
    warning(na_msg)
  }
  return(retval)
}






#' @title Performance Metrics of Binary Classifier
#'
#' @description This is an S3 method for object of class \code{"rocit"}.
#' It computes various performance metrics at different cutoff values.
#'
#'
#'
#' @param x An object of class \code{"rocit"} created with \code{\link{rocit}}.
#'
#' @param measure The performance metrics to be evaluated. See "Details"
#' for available options.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @return An object of class \code{"measureit"}, same as returned by
#' \code{\link{measureit.default}}.
#'
#'
#'
#' @details This function calls \code{\link{measureit.default}}. From the
#' components of \code{"rocit"} objects, it calculates the \code{score}
#' and \code{class}
#' variables internally. See \code{\link{measureit.default}} for other
#' details and available options for \code{measure} argument.
#'
#'
#' @seealso \code{\link{measureit.default}}, \code{\link{print.measureit}}
#'
#' @note See \code{\link{measureit.default}}.
#'
#' @examples
#' data("Diabetes")
#' logistic.model <- glm(as.factor(dtest)~chol+age+bmi,
#'                       data = Diabetes,family = "binomial")
#' class <- logistic.model$y
#' score <- logistic.model$fitted.values
#' rocit_object <- rocit(score = score, class = class)
#' # -------------------------------------------------------------
#' measure <- measureit(rocit_object, measure = c("ACC", "SENS", "FSCR"))
#' names(measure)
#' plot(measure$ACC~measure$Cutoff, type = "l")
#' plot(measure$TP~measure$FP, type = "l")
#'
#' @method measureit rocit
#' @export
measureit.rocit <- function(x, measure = c("ACC", "SENS"), ... = NULL){
  score <- c(x$pos_D, x$neg_D)
  class <- c(rep(1, x$pos_count), rep(0, x$neg_count))
  measureit.default(score = score, class = class,
                    negref = 0, measure = measure)

}
