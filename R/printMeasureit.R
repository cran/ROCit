#' @title Print \code{'measureit'} Object
#'
#' @description S3 method to print object of \code{"measureit"} class
#' in organized way.
#'
#' @param x An object of class \code{"measureit"}, created with the function
#' \code{\link{measureit}}.
#'
#' @param n How many rows of output is desired in the output. If NULL, then
#' prints all the rows. If specified, then first n rows are printed. If specified
#' n is bigger than the number of possible rows, then n is adjusted. If
#' non integer or negative, default (10 or number of possible rows,
#' whichever is smaller) is set. If \code{NULL}, all rows printed.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#' @examples
#' data("Diabetes")
#' logistic.model <- glm(as.factor(dtest)~chol+age+bmi,
#'                       data = Diabetes,family = "binomial")
#' class <- logistic.model$y
#' score <- logistic.model$fitted.values
#' # -------------------------------------------------------------
#' measure <- measureit(score = score, class = class,
#'                      measure = c("ACC", "SENS", "FSCR"))
#' print(measure, n = 5)
#' print(measure, n = 10)

#'
#'
#'
#'
#' @seealso \code{\link{measureit}}



#' @method print measureit
#' @export
print.measureit <- function(x, n = NULL, ... = NULL){
  temp <- x[[1]]
  for(i in 2:length(names(x))){
    temp <- cbind(temp, x[[i]])
  }
  temp <- data.frame(temp)
  names(temp) <- names(x)

  if(is.null(n)){
    print(temp)
  }else{
    if(n > nrow(temp)){
      n <- nrow(temp)
    }
    if(n < 1 || round(n) != n){
      warning("Invalid n, default is used\n")
      if(n > nrow(temp)){
        n <- nrow(temp)
      }else{
        n = 10
      }
    }
    print(temp[c(1:n), ])
  }
}


