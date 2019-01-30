#' @title Rank order data
#'
#' @import stats
#'
#' @description Function \code{rankorderdata} rank-orders the data
#' with respect to some variable (diagnostic variable).
#'
#'
#' @param score A vector containing (diagnostic) scores.
#' @param class A vector containing the class.
#' @param  dec Logical. \code{TRUE} for descending order, \code{FALSE} for
#' ascending order.
#'
#' @return A dataframe, rank-ordered with respect to the score.
#'
#'
#'
#' @section Comment:
#' \code{rankorderdata} is used internally in other function(s) of \pkg{ROCit}.
#'
#' @examples score <- c(0.4 * runif(20) + 0.2, 0.4*runif(20))
#' @examples class <- c(rep("A",20), rep("B",20))
#' @examples returndata <- rankorderdata(score, class, dec = FALSE)
#' @examples returndata
#'
#' @author Riaz Khan, \email{mdriazahmed.khan@@jacks.sdstate.edu}
#'
#' @export
rankorderdata <- function(score, class, dec = TRUE){
  tempdata <- as.data.frame(cbind(D = score, Y = class))
  tempdata[with(tempdata,order(D, Y, decreasing = dec)),]
}





