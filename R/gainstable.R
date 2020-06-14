#' @title  Gains Table for Binary Classifier
#' @description See \code{\link{gainstable.default}},
#' \code{\link{gainstable.rocit}}.
#'
#' @param ... Arguments to be passed to methods.
#' See \code{\link{gainstable.default}}, \code{\link{gainstable.rocit}}.
#'
#'
#'
#' @export
gainstable <- function(...){
  UseMethod("gainstable")
}

#' @title Gains Table for Binary Classifier
#' @description Default S3 method to create gains table from a vector of
#' diagnostic score and the class of observations.
#'
#' @param score An numeric array of diagnostic score. Same as in
#' \code{\link{rocit}}.
#'
#' @param class An array of equal length of score,
#' containing the class of the observations. Same as in \code{\link{rocit}}.
#'
#' @param negref The reference value, same as the
#' \code{reference} in \code{\link{convertclass}}.
#' Depending on the class of \code{x},
#' it can be numeric or character type. If specified, this value
#' is converted to 0 and other is converted to 1. If NULL, reference is
#' set alphabetically. Same as in \code{\link{rocit}}.
#'
#'
#' @param ngroup Number of desired groups in gains table. Ignored if
#' \code{breaks} is specified.
#' See "Details".
#'
#' @param breaks Percentiles (in percentage) at which observations
#' should be separated to
#' form groups. If specified, \code{ngroup} is ignored. See "Details".
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @details \code{gainstable} function creates gains table containing
#' \code{ngroup} number of groups or buckets. The algorithm first orders
#' the score variable with respect to score variable. In case of tie,
#' it class becomes the ordering variable, keeping the positive responses first.
#' The algorithm calculates the ending index in each bucket as
#' \eqn{round((length(score) / ngroup) * (1:ngroup))}. Each bucket should have
#' at least 5 observations.
#'
#'
#'       If buckets' end index are to be ended at desired level of
#' population, then \code{breaks} should be specified.
#' If specified, it overrides \code{ngroup} and \code{ngroup} is ignored.
#' \code{breaks} by default always includes 100. If whole number does not exist
#' at specified population, nearest integers are considered.
#'
#'
#' @return A list of class \code{"gainstable"}. It has the following components:
#'
#' \item{Bucket}{The serial number of buckets or groups.}
#' \item{Obs}{Number of observation in the group.}
#' \item{CObs}{Cumulative number of observations up to the group.}
#' \item{Depth}{Cumulative population depth up to the group.}
#' \item{Resp}{Number of (positive) responses in the group.}
#' \item{CResp}{Cumulative number of (positive) responses up to the group.}
#' \item{RespRate}{(Positive) response rate in the group.}
#' \item{CRespRate}{Cumulative (positive) response rate up to the group}
#' \item{CCapRate}{Cumulative overall capture rate of (positive) responses
#' up to the group.}
#' \item{Lift}{Lift index in the group. Calculated as
#' \eqn{GroupResponseRate / OverallResponseRate}.}
#' \item{CLift}{Cumulative lift index up to the group.}
#'
#' @note The algorithm is designed for complete cases. If NA(s) found in
#' either \code{score} or \code{class}, then removed.
#'
#' @seealso \code{\link{gainstable.rocit}}, \code{\link{plot.gainstable}},
#' \code{\link{rocit}}
#'
#'
#' @examples
#' data("Loan")
#' class <- Loan$Status
#' score <- Loan$Score
#' # ----------------------------------------------------------------
#' gtable15 <- gainstable(score = score, class = class,
#'                        negref = "FP", ngroup = 15)
#' gtable_custom <- gainstable(score = score, class = class,
#'                             negref = "FP", breaks = seq(1,100,15))
#' # ----------------------------------------------------------------
#' print(gtable15)
#' print(gtable_custom)
#' # ----------------------------------------------------------------
#' plot(gtable15)
#' plot(gtable_custom)
#' plot(gtable_custom, type = 2)
#' plot(gtable_custom, type = 3)

#'
#' @method gainstable default
#' @export
gainstable.default <- function(score, class, negref = NULL,
                               ngroup = 10, breaks = NULL,
                               ... = NULL)
{
  ls <- length(score)
  if (ls != length(class)) {
    stop("score and class differ in length")
  }

  scoreNA <- which(is.na(score))
  classNA <- which(is.na(class))
  unionNA <- union(scoreNA, classNA)

  if(length(unionNA) > 0){
    message("removing NA(s) from score and/or class")
    score <- score[-unionNA]
    class <- class[-unionNA]
    message("NA(s) removed")
  }


  if (is.null(breaks)) {
    ngrouptest1 <- class(ngroup) == "integer"
    ngrouptest2 <- class(ngroup) == "numeric"

    ngrouptest3 <- ngroup %% 1 == 0
    ngrouptest4 <- ngroup >= 2

    if(!((ngrouptest1 || ngrouptest2) && ngrouptest3 && ngrouptest4)) {
      stop("ngroup must be a positve integer greater than 1")
    }

    if (round(ls / ngroup) < 5) {
      stop("too many groups or insufficient data")
    }
  } else{
    breakstest1 <- class(breaks) == "numeric"
    breakstest2 <- class(breaks) == "integer"
    breakstest3 <- (length(breaks)) + 1 < ls


    if (!((breakstest1 || breakstest2) && breakstest3)) {
      stop("either breaks type invalid or too many breaks")
    }
  }

  scoretest1 <- class(score) == "numeric"
  scoretest2 <- class(score) == "integer"
  if (!(scoretest1 || scoretest2)) {
    stop("score is not numeric or integer")
  }


  if(!is.null(negref)){
    if(!(negref %in% unique(class))){
      stop("(negative) reference not found in class")
    }
  }
  convertedclass <- convertclass(class, reference = negref)
  tempdata <- rankorderdata(score, convertedclass)
  Y <- tempdata$Y
  if (is.null(breaks)) {
    cumNobs <- round(ls / ngroup * 1:ngroup)
  } else{
    if (max(breaks) > 100 || min(breaks) <= 0) {
      stop("allowed range for values of breaks : (0, 100]")
    }
    breaks <- sort(unique(c(breaks, 100)))
    cumNobs <- round((breaks / 100) * ls)
    ngroup <- length(breaks)
  }

  tempfun <- function(x) {
    shifted <- c(0, x[1:(length(x) - 1)])
    x - shifted
  }
  Nobs <- tempfun(cumNobs)
  startIndex <- c(1, (cumNobs[2:ngroup - 1] + 1))
  endIndex <- cumNobs

  totalresponse <- sum(Y)
  overallresprate <- mean(Y)
  temfun <- function(i) {
    start <- startIndex[i]
    end <- endIndex[i]
    n_resp <- sum(Y[start:end])
    cumn_resp <- sum(Y[1:end])
    resprate <- n_resp / Nobs[i]
    cumresprate <- cumn_resp / cumNobs[i]
    CCapRate <- cumn_resp / totalresponse
    lift <- resprate / overallresprate
    cumlift <- mean(Y[1:end]) / overallresprate
    c(
      Resp = n_resp,
      CResp = cumn_resp,
      RespRate = resprate,
      CRespRate = cumresprate,
      CCapRate = CCapRate,
      Lift = lift,
      CLift = cumlift
    )
  }

  tempdata <- data.frame(t(apply(matrix(1:ngroup), 1, temfun)))
  Bucket <- 1:ngroup
  Depth <- cumNobs / ls
  tempdata <- cbind(Bucket, Obs = Nobs, CObs = cumNobs,
                    Depth, tempdata)
  class(tempdata) <- "gainstable"
  return(tempdata)
}





#' @title Gains Table for Binary Classifier
#'
#' @description S3 method to create gains table from object of
#' class \code{"rocit"}.
#'
#'
#' @param x A \code{"rocit"} object, created with \code{\link{rocit}}.
#'
#'
#' @param ngroup Number of desired groups in gains table. See
#' \code{\link{gainstable.default}}.
#'
#' @param breaks Percentiles (in percentage) at which observations
#' should be separated to
#' form groups. See \code{\link{gainstable.default}}
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#'
#' @details \code{gainstable.rocit} calls \code{\link{gainstable.default}}.
#' It creates the \code{score} and \code{class} variables from the
#' supplied \code{"rocit"} object internally. See
#' \code{\link{gainstable.default}} for details.
#'

#'
#'
#' @return A list of class \code{"gainstable"}, same as returned by
#' \code{\link{gainstable.default}}.
#'
#' @seealso  \code{\link{gainstable.default}}, \code{\link{plot.gainstable}},
#'  \code{\link{rocit}}
#'
#' @examples
#' data("Loan")
#' class <- Loan$Status
#' score <- Loan$Score
#' rocit_emp <- rocit(score = score, class = class, negref = "FP")
#' # ----------------------------------------------------------------
#' gtable15 <- gainstable(rocit_emp, ngroup = 15)
#' gtable_custom <- gainstable(rocit_emp, breaks = seq(1,100,15))
# ----------------------------------------------------------------
#' print(gtable15)
#' print(gtable_custom)
#' # ----------------------------------------------------------------
#' plot(gtable15)
#' plot(gtable_custom)
#' plot(gtable_custom, type = 2)
#' plot(gtable_custom, type = 3)
#'
#' @method gainstable rocit
#' @export
gainstable.rocit <- function(x, ngroup = 10, breaks = NULL,
                             ... = NULL)
{
  score <- c(x$pos_D, x$neg_D)
  class <- c(rep(1, x$pos_count), rep(0, x$neg_count))
  gainstable.default(
    score = score,
    class = class,
    negref = 0,
    ngroup = ngroup,
    breaks = breaks
  )

}






