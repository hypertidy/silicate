% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.sc}
\alias{print.sc}
\title{Methods for silicate}
\usage{
\method{print}{sc}(x, ...)
}
\arguments{
\item{x}{object inheriting from 'sc' class}

\item{...}{ignore currently}
}
\description{
Print a silicate model.
}
\details{
Simple summary of type and content of a silicate model.
}
\examples{
print(TRI(minimal_mesh))
print(SC(minimal_mesh))
print(PATH(minimal_mesh))
print(SC(TRI(minimal_mesh)))
print(ARC(minimal_mesh))
print(SC0(minimal_mesh))
}
