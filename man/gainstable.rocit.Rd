% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gainstable.R
\name{gainstable.rocit}
\alias{gainstable.rocit}
\title{Gains Table for Binary Classifier}
\usage{
\method{gainstable}{rocit}(x, ngroup = 10, breaks = NULL, ... = NULL)
}
\arguments{
\item{x}{A \code{"rocit"} object, created with \code{\link{rocit}}.}

\item{ngroup}{Number of desired groups in gains table. See
\code{\link{gainstable.default}}.}

\item{breaks}{Percentiles (in percentage) at which observations
should be separated to
form groups. See \code{\link{gainstable.default}}}

\item{...}{\code{NULL}. Used for S3 generic/method consistency.}
}
\value{
A list of class \code{"gainstable"}, same as returned by
\code{\link{gainstable.default}}.
}
\description{
S3 method to create gains table from object of
class \code{"rocit"}.
}
\details{
\code{gainstable.rocit} calls \code{\link{gainstable.default}}.
It creates the \code{score} and \code{class} variables from the
supplied \code{"rocit"} object internally. See
\code{\link{gainstable.default}} for details.
}
\examples{
data("Loan")
class <- Loan$Status
score <- Loan$Score
rocit_emp <- rocit(score = score, class = class, negref = "FP")
# ----------------------------------------------------------------
gtable15 <- gainstable(rocit_emp, ngroup = 15)
gtable_custom <- gainstable(rocit_emp, breaks = seq(1,100,15))
print(gtable15)
print(gtable_custom)
# ----------------------------------------------------------------
plot(gtable15)
plot(gtable_custom)
plot(gtable_custom, type = 2)
plot(gtable_custom, type = 3)

}
\seealso{
\code{\link{gainstable.default}}, \code{\link{plot.gainstable}},
 \code{\link{rocit}}
}
