% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readReportingMIF.R
\name{readReportingMIF}
\alias{readReportingMIF}
\title{Reads in a valid MIF (or CSV) reporting file}
\usage{
readReportingMIF(pathToMIF, RData = FALSE, verbose = TRUE)
}
\arguments{
\item{pathToMIF}{character. Path to a valid MIF reporting file.}

\item{RData}{logical. if true data is saved in Rdata format to save read-in
time}

\item{verbose}{logical. If true (un)helpful information will be display.}
}
\value{
quitte object
}
\description{
read in a valid MIF (or CSV) reporting file and return data as a
quitte-object
}
\examples{

  \dontrun{
    qd <- readReportingMIF('/my/magic/remind/run/REMIND_generic.mif')
    qd <- readReportingMIF('/my/magic/remind/run/REMIND_generic.csv')
  }

}
\author{
Anselm Schultes, Jerome Hilaire, Lavinia Baumstark, David Klein
}
