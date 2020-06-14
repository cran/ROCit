#' @title ROC Analysis of Binary Classifier
#'
#' @import stats
#' @description \code{rocit} is the main function of \pkg{ROCit} package.
#' With the diagnostic score and the class of each observation,
#' it calculates true positive rate (sensitivity) and
#' false positive rate (1-Specificity) at convenient cutoff values
#' to construct ROC curve. The function returns \code{"rocit"} object,
#' which can be passed as arguments for other S3 methods.
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
#' @param method The method of estimating ROC curve. Currently supports
#' \code{"empirical"}, \code{"binormal"} and \code{"nonparametric"}.
#' Pattern matching allowed thorough \code{\link[base:grep]{grep}}.
#'
#' @param step Logical, default in \code{FALSE}. Only applicable for
#' \code{empirical} method and ignored for others. Indicates
#' whether only horizontal and vertical steps should be used
#' to produce the ROC curve. See "Details".
#'
#'
#' @details ROC curve is defined as the set of ordered pairs,
#' \eqn{(FPR(c), TPR(c))}, where, \eqn{-\infty < c < \infty},
#' where, \eqn{FPR(c) = P(D \ge c | Y = 0)} and \eqn{FPR(c) = P(D \ge c | Y = 1)}
#' at cutoff \eqn{c}.
#' Alternately, it can be defined as:
#' \deqn{y(x) = 1 - G[F^{-1}(1-x)],  0 \le x \le 1}
#' where \eqn{F} and \eqn{G} are the cumulative density functions of the
#' diagnostic score in negative and positive responses respectively.
#' \code{rocit} evaluates TPR and FPR values at convenient cutoffs.
#'
#'     As the name implies, empirical TPR and FPR values are evaluated
#'  for \code{method = "empirical"}. For \code{"binormal"}, the distribution
#'  of diagnostic are assumed to be normal and maximum likelihood parameters
#'  are estimated. If \code{method = "nonparametric"}, then kernel density
#'  estimates (using \code{\link[stats:density]{density}}) are applied with
#'  following bandwidth:
#'  \itemize{
#'  \item \eqn{h_Y = 0.9 * min(\sigma_Y, IQR(D_Y)/1.34)/((n_Y)^{(1/5)})}
#'  \item \eqn{h_{\bar{Y}} = 0.9 * min(\sigma_{\bar{Y}},
#'  IQR(D_{\bar{Y}})/1.34)/((n_{\bar{Y}})^{(1/5)})}
#'  }
#'  as described in Zou et al. From the kernel estimates of PDFs, CDFs are
#'  estimated using trapezoidal rule.
#'
#' For \code{"empirical"} ROC, the algorithm firt rank orders the
#' data and calculates TPR and FPR by treating all predicted
#' up to certain level as positive. If \code{step} is \code{TRUE},
#' then the ROC curve is generated based on all the calculated
#' \{FPR, TPR\} pairs regardless of tie in the data. If \code{step} is
#' \code{FALSE}, then the ROC curve follows a diagonal path for the ties.
#'
#'
#' For \code{"empirical"} ROC, trapezoidal rule is
#' applied to estimate area under curve (AUC). For \code{"binormal"}, AUC is estimated by
#' \eqn{\Phi(A/\sqrt(1 + B^2)}, where \eqn{A} and \eqn{B} are functions
#' of mean and variance of the diagnostic in two groups.
#' For \code{"nonparametric"}, AUC is estimated as
#' by
#' \deqn{\frac{1}{n_Yn_{\bar{Y}}}
#' \sum_{i=1}^{n_{\bar{Y}}}
#' \sum_{j=1}^{n_{Y}}
#' \Phi(
#'   \frac{D_{Y_j}-D_{{\bar{Y}}_i}}{\sqrt{h_Y^2+h_{\bar{Y}}^2}}
#'  )}
#'
#'
#'
#'
#' @return A list of class \code{"rocit"}, having following elements:
#' \item{method}{The method applied to estimate ROC curve.}
#' \item{pos_count}{Number of positive responses.}
#' \item{neg_count}{Number of negative responses.}
#' \item{pos_D}{Array of diagnostic scores in positive responses.}
#' \item{neg_D}{Array of diagnostic scores in negative responses.}
#' \item{AUC}{Area under curve. See "Details".}
#' \item{Cutoff}{Array of cutoff values at which the
#' true positive rates and  false positive rates
#'  are evaluated. Applicable for \code{"empirical"} and
#'  \code{"nonparametric"}.}
#' \item{param}{Maximum likelihood estimates of \eqn{\mu} and \eqn{\sigma} of
#' the diagnostic score in two groups. Applicable for \code{"binormal"}.}
#' \item{TPR}{Array of true positive rates (or sensitivities or recalls),
#' evaluated at the cutoff values.}
#' \item{FPR}{Array of false positive rates (or 1-specificity),
#' evaluated at the cutoff values.}
#'
#' @references Pepe, Margaret Sullivan. \emph{The statistical evaluation
#' of medical tests for classification and prediction.} Medicine, 2003.
#'
#' @references Zou, Kelly H., W. J. Hall, and David E. Shapiro.
#' "Smooth non-parametric receiver operating characteristic (ROC)
#'  curves for continuous diagnostic tests." \emph{Statistics in medicine}
#'  16, no. 19 (1997): 2143-2156.
#'
#' @seealso \code{\link{ciROC}}, \code{\link{ciAUC}}, \code{\link{plot.rocit}},
#' \code{\link{gainstable}}, \code{\link{ksplot}}
#'

#'
#'
#' @note The algorithm is designed for complete cases. If NA(s) found in
#' either \code{score} or \code{class}, then removed.
#'
#'
#' @examples
#' # ---------------------
#' data("Diabetes")
#' roc_empirical <- rocit(score = Diabetes$chol, class = Diabetes$dtest,
#'                        negref = "-") # default method empirical
#' roc_binormal <- rocit(score = Diabetes$chol, class = Diabetes$dtest,
#'                      negref = "-", method = "bin")
#'
#'# ---------------------
#' summary(roc_empirical)
#' summary(roc_binormal)
#'
#' # ---------------------
#' plot(roc_empirical)
#' plot(roc_binormal, col = c("#00BA37", "#F8766D"),
#'        legend = FALSE, YIndex = FALSE)
#'
#'
#' @export
rocit <- function(score, class, negref = NULL, method = "empirical",
                  step = FALSE){
  if (length(score) != length(class)) {
    stop("score and class differ in length")
  }
  if (!(is.numeric(score))) {
    stop("score must be numeric")
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

  if (!is.null(negref)) {
    if (!(negref %in% unique(class))) {
      stop("reference not found in class")
    }
  }

  if(length(method) > 1) {
    stop("rocit cannot deal with multiple methods")
  }



  if(!identical(grep(method, "empirical", fixed = T), integer(0))){
    convertedclass <- convertclass(class, reference = negref)
    tempdata <- rankorderdata(score, convertedclass)


    D <- tempdata[, 1]
    Y <- tempdata[, 2]

    Ybar <- 1 - Y
    DY <- D[which(Y == 1)]
    DYbar <- D[which(Y == 0)]
    nY <- sum(Y)
    nYbar <- sum(Ybar)

    TP <- cumsum(Y)
    FP <- cumsum(Ybar)
    TPR <- TP / nY
    FPR <- FP / nYbar
    Cutoff <- D
    if(!step){
      df <- data.frame(cbind(index = 1:(nY + nYbar), D))
      revdf <- df[rev(row.names(df)), ]
      keep <- rev(revdf$index[!duplicated(revdf$D)])
      TPR <- TPR[keep]
      FPR <- FPR[keep]
      Cutoff <- Cutoff[keep]
    }

    TPR <- c(0, TPR)
    FPR <- c(0, FPR)
    Cutoff <- c(Inf, Cutoff)
    AUC <- trapezoidarea(FPR, TPR)


    returnval <- list(method = "empirical",
                      pos_count = nY, neg_count = nYbar,
                      pos_D = DY, neg_D = DYbar,
                      AUC = AUC, Cutoff = Cutoff, TPR = TPR, FPR = FPR)

    class(returnval) <- "rocit"
    if(na_cases){
      warning(na_msg)
    }
    return(returnval)
  }

  if(!identical(grep(method, "binormal", fixed = T), integer(0))){
    D <- score
    Y <- convertclass(class, reference = negref)

    posIndex <- which(Y == 1)
    negIndex <- which(Y == 0)

    DY <- D[posIndex]
    DYbar <- D[negIndex]

    nY <- sum(Y)
    n <- length(D)
    nYbar <- n - nY

    posparam <- MLestimates(DY)
    negparam <- MLestimates(DYbar)

    A <- abs(posparam$mu - negparam$mu) / posparam$sigma
    B <- negparam$sigma / posparam$sigma

    Zx <- c(-Inf, seq(-3, 3, 0.01), Inf)
    Cutoff <- negparam$mu - negparam$sigma * Zx
    FPR <- pnorm(Zx)
    TPR <- pnorm(A + B * Zx)

    AUC <- pnorm(A / sqrt(1 + B ^ 2))
    returnval <- list(method = "binormal",
                      pos_count = nY, neg_count = nYbar,
                      pos_D = DY, neg_D = DYbar,
                      AUC = AUC,
                      param = list(posparam = posparam, negparam = negparam),
                      TPR = TPR, FPR = FPR)
    class(returnval) <- "rocit"
    if(na_cases){
      warning(na_msg)
    }
    return(returnval)
  }

  if(!identical(grep(method, "nonparametric"), integer(0))){
    D <- score
    Y <- convertclass(class, reference = negref)

    posIndex <- which(Y == 1)
    negIndex <- which(Y == 0)

    DY <- D[posIndex]
    DYbar <- D[negIndex]

    nY <- sum(Y)
    n <- length(D)
    nYbar <- n - nY

    posparam <- MLestimates(DY)
    negparam <- MLestimates(DYbar)

    bw_pos <- 0.9 * min(MLestimates(DY)$sigma,IQR(DY)/1.34)/(nY)^(1/5)
    bw_neg <- 0.9 * min(MLestimates(DYbar)$sigma,IQR(DYbar)/1.34)/(nYbar)^(1/5)

    g_x <- density(DY, kernel = "biweight",
                   bw = bw_pos,
                   n = 2 ^ 14)
    f_x <- density(DYbar, kernel = "biweight",
                   bw = bw_neg,
                   n = 2 ^ 14)

    ubound <- max(max(DY), max(DYbar))
    lbound <- min(min(DY), min(DYbar))
    dif <- ubound - lbound
    ubound <- ubound + 1.5 * dif
    lbound <- lbound - 1.5 * dif
    c_vals <- seq(lbound, ubound, (ubound - lbound) / 999)

    tempfun2 <- function(i){
      min(getsurvival(g_x, i), 1)
    }

    TPR <- apply(matrix(c_vals), 1, tempfun2)
    tempfun3 <- function(i){
      min(getsurvival(f_x, i), 1)
    }

    FPR <- apply(matrix(c_vals), 1, tempfun3)

    tempmat <- cbind(FPR, TPR)
    uniqueIndex <- !duplicated(t(apply(tempmat, 1, sort)))
    tempmat <- tempmat[uniqueIndex, ]
    Cutoff <- c_vals[uniqueIndex]
    FPR <- tempmat[, 1]
    TPR <- tempmat[, 2]

    cartesian <- cartesian_2D(DY, DYbar)
    AUC <- mean(pnorm((cartesian[, 1]-
                         cartesian[, 2]) / sqrt((bw_pos) ^ 2 +
                                                  (bw_neg) ^ 2)))
    returnval <- list(method = "non-parametric",
                      pos_count = nY, neg_count = nYbar,
                      pos_D = DY, neg_D = DYbar,
                      AUC = AUC, Cutoff = Cutoff, TPR = TPR, FPR = FPR)
    class(returnval) <- "rocit"
    if(na_cases){
      warning(na_msg)
    }
    return(returnval)
  }
  stop("supplied method is not valid")
}





