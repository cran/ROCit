#' @title Confidence Interval of ROC curve
#'
#' @description See \code{\link{ciROC.rocit}}.
#'
#' @param object An object of class \code{"rocit"}, returned by
#'   \code{\link{rocit}}. Supports \code{"empirical"} and \code{"binormal"}
#'   ROC curve.
#'
#' @param ... Arguments to be passed to methods.
#' See \code{\link{ciROC.rocit}}.
#'
#'
#' @export
ciROC <- function(object, ...){
  UseMethod("ciROC")
}



#' @title Confidence Interval of ROC curve
#'
#'
#' @description \code{ciROC} constructs confidence interval
#' of receiver operating characteristic (ROC)
#' curve. This is an S3 method defined for object of class \code{"rocit"}.
#'
#'
#'
#'
#' @param object An object of class \code{"rocit"}, returned by
#'   \code{\link{rocit}}. Supports \code{"empirical"} and \code{"binormal"}
#'   ROC curve.
#' @param level Level of confidence, must be within the range (0 1).
#' Default is 0.95.
#'
#' @param nboot Number of bootstrap samples, used to estimate \code{var(A)},
#' \code{var(B)}, \code{cov(A,B)}. Only used for \code{method = "binomial"}.
#' See 'Details'.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#'@details For large values of \eqn{n_Y} and \eqn{n_{\bar{Y}}},
#' the distribution of \eqn{TPR(c)} at
#' \eqn{FPR(c)} can be approximated as a normal distribution
#' with following mean and variance:
#' \deqn{\mu_{TPR(c)}=\sum_{i=1}^{n_Y}I(D_{Y_i}\geq c)/n_Y}
#' \deqn{V ( TPR(c) )=	\frac{  TPR(c) ( 1-  TPR(c))  }{n_Y}
#' + ( \frac{g(c^*)}{f(c^*) } )^2 * K }
#' where \eqn{K=\frac{ FPR(c) (1-FPR(c))}{n_{\bar{Y}} } }, \eqn{g}
#' and \eqn{f} are the probability distribution functions  of
#' the diagnostic variable in positive and negative groups
#' (with corresponding cumulative distribution functions \eqn{G} and \eqn{F}),
#' \eqn{c^*=S^{-1}_{D_{\bar{ Y}}}( FPR(c) )}, and \eqn{S} is the survival
#' function given by: \eqn{S(t)=P(T>t)=1-F(t)}. \code{density} and
#' \code{approxfun} were used to approximate PDF and CDF
#' of the diagnostic score in the two groups and the inverse survival
#' of the diagnostic in the negative responses.
#'
#'
#' For \code{"binomial"} type, variance of \eqn{A+BZ_x} is given by
#' \eqn{V(A)+Z_x^2V(B)+2Z_xCov(A, B)}. Bootstrap method was used to estimate
#' \eqn{V(A)}, \eqn{V(B)} and \eqn{Cov{A,B}}. The lower and upper limit of
#' \eqn{A+BZ_x} are inverse probit transformed to obtain the confidence interval
#' of the ROC curve.
#'
#'
#' @references Pepe, Margaret Sullivan. \emph{The statistical evaluation
#' of medical tests for classification and prediction.} Medicine, 2003.
#'
#'
#' @return A list of class \code{"rocci"}, having following elements:
#' \item{`ROC estimation method``}{The method applied to estimate ROC curve in the
#' \code{rocit} object.}
#'
#' \item{`Confidence level`}{Level of confidence as supplied as argument.}
#' \item{FPR}{An array containing all the FPR values,
#' for which TPR and confidence
#' interval of TPR were estimated.}
#'
#' \item{TPR}{Array containing the TPR values associated with the FPR values.}
#'
#' \item{LowerTPR}{Lower limits of the TPR values. Forced to zero for
#' \code{type = "empirical"}, where empirical TPR is zero.}
#'
#' \item{UpperTPR}{Upper limits of the TPR values. Forced to one for
#' \code{type = "empirical"}, where empirical TPR is one.}
#'
#'
#' @examples
#' data("Loan")
#' score <- Loan$Score
#' class <- ifelse(Loan$Status == "CO", 1, 0)
#' rocit_emp <- rocit(score = score, class = class, method = "emp")
#' # ------------------------------------------------
#' ciROC_emp90 <- ciROC(rocit_emp, level = 0.9)
# ------------------------------------------------
#' plot(ciROC_emp90, egend = TRUE)
#'
#' @seealso \code{\link{plot.rocci}}, \code{\link{rocit}}, \code{\link{ciAUC.rocit}}


#' @method ciROC rocit
#' @export
ciROC.rocit <- function(object, level = 0.95, nboot = 500, ... = NULL){
  if(level < 0 | level > 1){
    stop("supplied level is not valid")
  }
  if(object$method == "non-parametric"){
    stop("Current version does not support non-parametrially estimated ROC")
  }

  if(object$method == "empirical"){
    # call the function
    returnVal <- ciROCemp(rocit_emp = object, level = level)
  }
  if(object$method == "binormal"){
    # call the function
    returnVal <- ciROCbin(rocit_bin = object, level = level, nboot = nboot)
  }
  class(returnVal) <- "rocci"
  return(returnVal)
}
