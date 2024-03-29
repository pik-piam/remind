% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportPolicyCosts.R
\name{reportPolicyCosts}
\alias{reportPolicyCosts}
\title{Read in GDX and calculate policy costs, used in convGDX2MIF.R for the
reporting}
\usage{
reportPolicyCosts(gdx, gdx_ref, regionSubsetList = NULL)
}
\arguments{
\item{gdx}{a GDX as created by readGDX, or the file name of a gdx}

\item{gdx_ref}{a reference GDX as created by readGDX, or the file name of a gdx}

\item{regionSubsetList}{a list containing regions to create report variables region
aggregations. If NULL (default value) only the global region aggregation "GLO" will
be created.}
}
\description{
Read in GDX and calculate policy costs functions
}
\examples{

\dontrun{reportPolicyCosts(gdx)}

}
\author{
Lavinia Baumstark
}
