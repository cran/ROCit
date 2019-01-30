#' @title Print \code{'gainstable'} Object
#'
#' @description S3 print method to print \code{"gainstable"} object.
#'
#'
#' @param x An object of class \code{"gainstable"}, created with either
#' \code{\link{gainstable.default}} or \code{\link{gainstable.rocit}}.
#'
#' @param maxdigit How many digits after decimal to be printed.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#' @examples
#' data("Loan")
#' class <- Loan$Status
#' score <- Loan$Score
#' rocit_emp <- rocit(score = score, class = class, negref = "FP")
#' # ----------------------------------------------------------------
#' gtable8 <- gainstable(rocit_emp, ngroup = 8)
#' print(gtable8)
#' print(gtable8, maxdigit = 4)



#' @method print gainstable
#' @export
print.gainstable <- function(x, maxdigit = 3, ... = NULL) {
  df <- as.data.frame(
    cbind(
      Bucket = x$Bucket,
      Obs = x$Obs,
      CObs = x$CObs,
      Depth = x$Depth,
      Resp = x$Resp,
      CResp = x$CResp,
      RespRate = x$RespRate,
      CRespRate = x$CRespRate,
      CCapRate = x$CCapRate,
      Lift = x$Lift,
      CLift = x$CLift
    )
  )

  ncol <- ncol(df)
  tempindex <- NULL -> rounddigits
  for (i in 1:ncol) {
    tempindex <- df[, i] %% 1
    rounddigits[i] <- ifelse((max(nchar(tempindex)) > maxdigit), T, F)
  }

  longcols <- which(rounddigits)
  for (i in longcols) {
    df[, i] <- round(df[, i], maxdigit)
  }
  print(df)
}
