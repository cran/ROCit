#' @title Confidence Interval of Binormal ROC Curve
#'
#'
#' @description Function \code{ciROCbin} estimates confidence interval
#' of binormally estimated ROC curve.
#'
#'
#' @param rocit_bin An object of class \code{rocit}, (\code{method = "binormal"}).
#' @param level Desired level of confidence to be estimated.
#' @param nboot Number of bootstrap samples, used to estimate \code{var(A)},
#' \code{var(B)}, \code{cov(A,B)}. See \code{\link{ciROC.rocit}}.
#'
#'
#'
#' @return A list object containing TPR, upper and lower bound of TPR
#' at certain FPR values.
#'
#'
#' @section Comment:
#' \code{ciROCbin} is used internally in \code{\link{ciROC.rocit}}
#' of \pkg{ROCit}.
#'
#' @seealso \code{\link{rocit}}, \code{\link{ciROC}}, \code{\link{plot.rocci}}
#'
#'
#' @examples
#' data("Loan")
#' score <- Loan$Score
#' class <- ifelse(Loan$Status == "CO", 1, 0)
#' rocit_bin <- rocit(score = score, class = class, method = "bin")
#' ciROC_bin90 <- ciROCbin(rocit_bin, level = 0.9, nboot = 300)
#' TPR <- ciROC_bin90$TPR
#' FPR <- ciROC_bin90$FPR
#' Upper90 <- ciROC_bin90$UpperTPR
#' Lower90 <- ciROC_bin90$LowerTPR
#' plot(TPR~FPR, type = "l")
#' lines(Upper90~FPR, lty = 2)
#' lines(Lower90~FPR, lty = 2)
#' grid()
#' legend("bottomright", c("Binormal ROC curve", "90% CI"), lty = c(1,2))



#'
#' @export
ciROCbin <- function(rocit_bin, level, nboot){

  #argClass <- class(rocit_bin)
  if(!(methods::is(rocit_bin, "rocit"))){
    stop("Argument is not of class \"rocit\" ")
  }

  argMethod <- rocit_bin$method
  if(argMethod != "binormal"){
    stop("Rocit object method is not \"binormal\" ")
  }

  TPR <- rocit_bin$TPR
  FPR <- rocit_bin$FPR
  pos_count <- rocit_bin$pos_count
  neg_count <- rocit_bin$neg_count
  pos_D <- rocit_bin$pos_D
  neg_D <- rocit_bin$neg_D
  pos_param <- rocit_bin$param$posparam
  neg_param <- rocit_bin$param$negparam
  D <- c(pos_D, neg_D)
  Y <- c(rep(1, pos_count), rep(0,neg_count))
  temp <- as.data.frame(cbind(D,Y))

  baseSeed <- sample(seq(10000),1)
  binFun <- function(i){
    seed <- 2 * baseSeed + 3 * i # 2 and 3 randomly chosen
    set.seed(seed)
    Index <- sample(pos_count+neg_count, replace = T)
    funData <- temp[Index,]
    posIndex <- which(funData$Y == 1)
    posD <- funData$D[posIndex]
    negD <- funData$D[-posIndex]
    pos_mu <- mean(posD)
    pos_sigma <- sd(posD)
    neg_mu <- mean(negD)
    neg_sigma <- sd(negD)
    A <- abs(pos_mu - neg_mu)/pos_sigma
    B <- neg_sigma/pos_sigma
    return(c(A,B))
  }
  setCounter <- seq(nboot)
  ABdata <- sapply(setCounter, binFun)
  Aarray <- ABdata[1,]
  Barray <- ABdata[2,]

  Zx <- c(-Inf, seq(-3, 3, 0.01), Inf)
  Var_ABZx <- var(Aarray) + Zx^2 * var(Barray) + 2 * Zx *cov(Aarray,Barray)

  A <- abs(pos_param$mu - neg_param$mu) / pos_param$sigma
  B <- neg_param$sigma / pos_param$sigma

  multiplier <- qnorm((1+level)/2)

  ABZx_base <- A + B * Zx
  LowerABZx <- ABZx_base - multiplier * sqrt(Var_ABZx)
  UpperABZx <- ABZx_base + multiplier * sqrt(Var_ABZx)

  lower <- pnorm(LowerABZx)
  upper <- pnorm(UpperABZx)

  returnval <- list(`ROC estimation method` = rocit_bin$method,
                    `Confidence level` = paste0(100*level,"%"),
                    FPR = FPR, TPR = TPR,
                    LowerTPR = lower, UpperTPR = upper)
  return(returnval)
}
