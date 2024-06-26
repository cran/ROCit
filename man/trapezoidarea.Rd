% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trap_MLestimates.R
\name{trapezoidarea}
\alias{trapezoidarea}
\title{Approximate Area with Trapezoid Rule}
\usage{
trapezoidarea(x, y)
}
\arguments{
\item{x, y}{Numeric vectors of same length, representing the \code{x} and \code{y} coordinates
of the points.}
}
\value{
Numeric value of the area under curve approximated with trapezoid rule.
}
\description{
\code{trapezoidarea} calculates the approximated area
under curve, using trapezoidal rule.
}
\details{
The function approximates the area bounded by the following 4 curves:
\deqn{x = a, x = b, y = 0, y = f(x)}

\eqn{a} and \eqn{b} are set at the min and max value of given \code{x}
coordinates. \eqn{(x, y)} are some points on the \eqn{y = f(x)} curve.
}
\section{Comment}{

\code{trapezoidarea} is used internally in other function(s) of \pkg{ROCit}.
}

\examples{
# Area under rectangle -----------------
trapezoidarea(seq(0, 10), rep(1, 11))

# Area under triangle ------------------
trapezoidarea(seq(0, 10), seq(0, 10))

# Area under normal pdf ----------------
x_vals <- seq(-3, 3, 0.01); y_vals <- dnorm(x_vals)
trapezoidarea(x = x_vals, y = y_vals) # theoretically 1

}
