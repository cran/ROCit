#' @title Loan Data
#'
#' @description A data containing information about 900 borrowers. It is a
#' modified version of publicly available real data.
#'
#'
#'
#' @format A data frame with 900 rows and 9 variables:
#' \describe{
#'   \item{Amount}{Amount of loan, shown as
#'   percentage of a certain amount.}
#'   \item{Term}{The number of payments on the loan.
#'   Values are in months.}
#'   \item{IntRate}{Interest rate.}
#'   \item{ILR}{Ratio of installment amount and total loan amount.}
#'   \item{EmpLen}{Employment length, categorized.
#'   \itemize{
#'   \item{\code{A:} 0-2 years}
#'   \item{\code{B:} 3-5 years}
#'   \item{\code{C:} 7-8 years}
#'   \item{\code{D:} 8+ years}
#'   \item{\code{U:} Unknown}
#'   }
#'   }
#'   \item{Home}{Status of home ownership.}
#'   \item{Income}{Annual income.}
#'   \item{Status}{A factor indicating whether the loan was fully paid
#'   (\code{FP}) or charged off (\code{CO}) after full term.}
#'   \item{Score}{A risk score calculated from loan amount, interest rate and
#'   annual income. The log-odds of  logistic regression were transformed into
#'   scores using \eqn{PDO = 30}, \eqn{OddsBase = 20} and \eqn{ScoreBase = 400}.
#'   See "References".}
#' }
#'
#' @examples
#' data("Loan")
#' boxplot(Income~Home, data = Loan, col = c(2:4), pch = 16,
#'         ylim = c(0,200000), ylab = "Income",
#'         xlab = "Home Ownership Status",
#'         main = "Annual Income Boxplot")
#' grid()
#'
#' @references Siddiqi, Naeem. \emph{Credit risk scorecards:
#' developing and implementing intelligent credit scoring.}
#' Vol. 3. John Wiley & Sons, 2012.
#'
#' @source \url{http://www.lendingclub.com/info/download-data.action}
"Loan"
