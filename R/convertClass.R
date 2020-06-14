#' @title Converts Binary Vector into 1 and 0
#'
#' @description \code{convertclass} converts a binary variable with any
#' response into 1/0 response. It is used internally in other functions of
#' package \pkg{ROCit}.
#'
#' @param x A vector of exactly two unique values.
#' @param reference The reference value. Depending on the class of \code{x},
#' it can be numeric or character type. If specified, this value
#' is converted to 0 and other is converted to 1. If NULL, reference is
#' set alphabetically.
#'
#' @return A numeric vector of 1 and 0. Gives warning if there exists \code{NA}(s)
#'  in \code{x}.
#'
#'
#'
#' @section Comment:
#' \code{convertclass} is used internally in other function(s) of \pkg{ROCit}.
#'
#' @examples x <- c("cat", "cat", "dog", "cat")
#' @examples convertclass(x) # by default, "cat" is converted to 0
#' @examples convertclass(x, reference = "dog")
#'
#' @examples # ----------------------------
#'
#' @examples set.seed(10)
#' @examples x <- round(runif(10, 2, 3))
#' @examples convertclass(x, reference = 3)
#' @examples # numeric reference can be supplied as character
#' @examples convertclass(x, reference = "3") # same result
#'
#'
#' @export
convertclass <- function(x, reference = NULL){
  if(any(is.na(x))){
    warning("NA(s) in the input data")
    na_index <- which(is.na(x))
    y <- x[-na_index]
    if(length(unique(y)) != 2){
      stop("class must have exactly two unique values")
    }

    levs <- levels(factor(y))
    if(!is.null(reference)){
      if (!(reference %in% levs)){
        stop("Provided reference class is not valid")
      }
    }else{
      reference <- levs[1]
    }
    return(ifelse(is.na(x), NA, ifelse(x == reference, 0, 1)))
  }
  else{
    if(length(unique(x)) != 2){
      stop("class must have exactly two unique values")
    }
    levs <- levels(factor(x))
    if(!is.null(reference)){
      if (!(reference %in% levs)){
        stop("Provided reference class is not valid")
      }
    }else{
      reference <- levs[1]
    }
    return(ifelse(x == reference, 0, 1))
  }
}




