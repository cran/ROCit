




#' @title Get number of TP, FP, TN and FN
#'
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
#' @name gettptnfpfn-defunct
#' @usage gettptnfpfn(x, depth, npos = sum(x), nneg = length(x) - npos)
#' @seealso \code{\link{ROCit-defunct}}
#' @keywords internal
NULL

#' @rdname ROCit-defunct
#'
#' @export
gettptnfpfn <- function(x, depth, npos = sum(x), nneg = length(x) - npos){
  .Defunct(new = NULL, package = NULL,
           msg = '"gettptnfpfn" has been removed from package ROCit.')
}




