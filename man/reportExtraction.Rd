% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportExtraction.R
\name{reportExtraction}
\alias{reportExtraction}
\title{Compute the reporting values of the extraction sector}
\usage{
reportExtraction(gdx, regionSubsetList = NULL)
}
\arguments{
\item{gdx}{A gdx object}

\item{regionSubsetList}{a list containing regions to create report variables region
aggregations. If NULL (default value) only the global region aggregation "GLO" will
be created.}
}
\value{
A magpie object
}
\description{
This function returns a magpie object containing the reporting values of the extraction sector. Values include quantities extracted, average and supply costs for coal, oil, gas and uranium.
}
\examples{

  \dontrun{
    reportExtraction(gdx)
  }
}
\author{
Jerome Hilaire, Lavinia Baumstark
}
