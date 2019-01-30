#' @title Get number of TP, FP, TN and FN
#'
#' @import stats
#'
#' @description Function \code{gettptnfpfn} calculates the number of
#' true positive (TP), false positive (FP),
#' true negative (TN) and false negative (FN), if certain depth of the
#' observations are treated as positive. Generally it applies to the
#' observations that are rank-ordered with respect to some diagnostic score.
#'
#'
#' @param x A response vector, consisting of only 1's and 0's.
#' @param depth Depth, up to which the positive predictions are made.
#' @param npos Number of positive responses.
#' @param nneg Number of negative responses.
#'
#' @details Function \code{gettptnfpfn} calculates the number of TP, FP,
#' TN, FN at specified depth. This is designed for numeric 1/0 coding
#' of the responses. Usually it applies to the observations, which are
#' rank-ordered with respect to some diagnostic score.
#'
#'
#' @return A numeric vector of length 4 indicating number of TP , FP, TN, FN,
#' respectively.
#'
#'
#' @section Comment:
#' \code{gettptnfpfn} is used internally in other function(s) of \pkg{ROCit}.
#' Invalid results are produced for depth = 0.
#'
#' @examples
#' k <- c(1,1,0,1,0,0,1,0,0,0,1,0,0,0)
#' # get TP, FP, TN, FN if first 6 are predicted to be positives
#' gettptnfpfn(k, 6)
#'
#'
#' @export
gettptnfpfn <- function(x, depth, npos = sum(x), nneg = length(x) - npos){
  tp <- sum(x[1:depth])
  fp <- depth - sum(x[1:depth])
  tn <- sum((!x) * c(rep(0, depth), rep(1, npos+nneg-depth)))
  fn <- npos+nneg-tp-fp-tn
  return(c(tp, fp, tn, fn))
}





