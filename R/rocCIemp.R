#' @title Confidence Interval of Empirical ROC Curve
#'
#'
#' @description Function \code{ciROCemp} estimates confidence interval
#' of empirically estimated ROC curve.
#'
#'
#' @param rocit_emp An object of class \code{rocit}, (\code{method = "empirical"}).
#' @param level Desired level of confidence to be estimated.
#'
#'
#'
#' @return A list object containing TPR, upper and lower bound of TPR
#' at certain FPR values.
#'
#'
#' @section Comment:
#' \code{ciROCemp} is used internally in \code{\link{ciROC.rocit}}
#' of \pkg{ROCit}.
#'
#' @seealso \code{\link{rocit}}, \code{\link{ciROC}}, \code{\link{plot.rocci}}
#'
#'
#' @examples
#' set.seed(100)
#' score <- c(runif(20, 15, 35), runif(15, 25, 45))
#' class <- c(rep(1, 20), rep(0, 15))
#' rocit_object <- rocit(score, class)
#' ciROC <- ciROCemp(rocit_object, level = 0.9)
#' names(ciROC)

#' @export
ciROCemp <- function(rocit_emp, level){
  # argClass <- class(rocit_emp)
  if(!(methods::is(rocit_emp, "rocit"))){
    stop("Argument is not of class \"rocit\" ")
  }

  argMethod <- rocit_emp$method
  if(argMethod != "empirical"){
    stop("Rocit object method is not \"empirical\" ")
  }

  # initialize values
  pos_count <- rocit_emp$pos_count
  neg_count <- rocit_emp$neg_count
  pos_D <- rocit_emp$pos_D
  neg_D <- rocit_emp$neg_D
  TPR <- rocit_emp$TPR
  FPR <- rocit_emp$FPR
  c <- rocit_emp$Cutoff
  max_nD <- max(neg_D)
  min_nD <- min(neg_D)
  # approximate distributions of diagnostic in two groups
  ppdf_temp <- approxfun(density(pos_D))
  npdf_temp <- approxfun(density(neg_D))
  ppdf <- function(x){
    ifelse(is.na(ppdf_temp(x)), 0, ppdf_temp(x))
  }
  npdf <- function(x){
    ifelse(is.na(npdf_temp(x)), 0, npdf_temp(x))
  }


  # CDF for negative diagnostic
  ncdf_2 <- function(y){
    retval <- rep(NA, length(y))
    for(i in 1:length(y)){
      x <- y[i]
      if(x>max(neg_D)){retval[i] <- 1}else{
        if(x<min(neg_D)){retval[i] <- 0}else{
          bw <- (x-min(neg_D))/1000
          myX <- seq(min(neg_D), x, bw)
          myY <- npdf(myX)
          retval[i] <- sum(diff(myX)*(myY[-1]+myY[-length(myY)]))/2
        }
      }
    }
    return(retval)
  }
  # survival function for negative diagnostic
  nSurv <- function(x) {1-ncdf_2(x)}
  # groundwork for c_star
  binwidht <- (max_nD-min_nD)/10000
  DummyX <- seq(min_nD-0.001,max_nD+0.001,binwidht)
  DummyY <- nSurv(DummyX)
  c_starfun=approxfun(DummyY,DummyX)
  c_starfun2 <- function(x){
    ifelse(x==0, max_nD, ifelse(x==1,min_nD,c_starfun(x)))
  }
  # calculations
  c_star <- c_starfun2(FPR)
  var_term2 <- (npdf(c_star)/ppdf(c_star))^2 * FPR * (1-FPR)/neg_count
  var_term1 <- TPR * (1-TPR)/pos_count
  SE_TPR <- sqrt(var_term1 + var_term2)
  multiplier <- qnorm((1+level)/2)
  upper <- TPR + multiplier * SE_TPR
  lower <- TPR - multiplier * SE_TPR
  # cap CI limits within [0,1]
  upFun <- function(x) {min(c(1,x))}
  lowFun <- function(x) {max(c(0,x))}
  upper <- sapply(upper, upFun)
  lower <- sapply(lower, lowFun)
  # return
  TPR0 <- which(TPR == 0)
  TPR1 <- which(TPR == 1)
  lower[TPR1] <- 1
  upper[TPR0] <- 0
  returnval <- list(`ROC estimation method` = rocit_emp$method,
                    `Confidence level` = paste0(100*level,"%"),
                    FPR = FPR, TPR = TPR,
                    LowerTPR = lower, UpperTPR = upper)
  return(returnval)
}
