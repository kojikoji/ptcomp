% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setupPtcomp.R
\name{setupPtcomp}
\alias{setupPtcomp}
\title{setupPtcomp}
\usage{
setupPtcomp(count.mat, t.vec, treatment.vec, replicate.vec,
  unit.num = 100)
}
\arguments{
\item{count.mat}{numeric matrix, gene (row) * cell (column) matrix}

\item{t.vec}{numeric vector, pseudo time estimation for each cell}

\item{treatment.vec}{character vector, experimental treatment for each cell}

\item{replicate.vec}{character vector, experimental replicate for each cell}

\item{unit.num}{numeric, how many cells are calculated for one entry of mean expression entry}
}
\value{
ptcomp tibble
}
\description{
Seup \code{tibble} object which is used for following analysis
}
\details{
This function set up the \code{tibble} data frame for following analysis
}
\author{
Yasuhiro Kojima
}
