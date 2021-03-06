% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/go2ranges.R
\name{go2ranges}
\alias{go2ranges}
\title{given a GO term, obtain a GRanges on the basis of EnsDb and GO.db}
\usage{
go2ranges(term = "oxidoreductase activity", ens_build = "v79")
}
\arguments{
\item{term}{character(1) should be GO term}

\item{ens_build}{character(1) either "v75" or "v79"}
}
\description{
given a GO term, obtain a GRanges on the basis of EnsDb and GO.db
}
\examples{
r = go2ranges()
head(r)
}
