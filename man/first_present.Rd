% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first_present.R
\name{first_present}
\alias{first_present}
\alias{last_present}
\title{Extract first or last non-NA value from a vector}
\usage{
first_present(x, order_by = NULL, default = NA)

last_present(x, order_by = NULL, default = NA)
}
\arguments{
\item{x}{A vector}

\item{order_by}{Another vector to sort \code{x} by.}

\item{default}{A default value to use if the position does not exist in the input.}
}
\value{
The first or last non-NA value in \code{x}.
}
\description{
Returns the first or last non-NA value of \code{x} when sorted by \code{order_by}.
}
\note{
If there are no non-NA values, NA is returned.
}
\examples{
first_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1))
last_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1))

first_present(x = c(NA, 1, 2, NA), order_by = c(1, 2, 3, 4))
last_present(x = c(NA, 1, 2, NA), order_by = c(1, 2, 3, 4))
}
