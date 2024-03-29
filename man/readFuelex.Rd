% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFuelex.R
\name{readFuelex}
\alias{readFuelex}
\title{Read Fuelex from GDX file}
\usage{
readFuelex(gdx, enty = NULL)
}
\arguments{
\item{gdx}{a GDX list as created by readGDX, or the file name of a gdx
file(file name is recommended as this speeds up the code)}

\item{enty}{one of the energy types in REMIND: "pecoal", "pegas", "peoil",
"pebiolc", "peur", etc.}
}
\value{
Fuelex data as MAgPIE object
}
\description{
Read Fuelex data for the specified enty (energy type) from a GDX file into a
magpie object.
}
\examples{

  \dontrun{
    coal <- readFuelex(gdx, enty = "pecoal")
    gas <- readFuelex(gdx, enty = "pegas")
  }

}
\author{
David Klein, Lavinia Baumstark
}
