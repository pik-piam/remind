#' dimSums
#'
#' Slightly modified version of the \code{dimSums} function of the magclass package.
#' It will expand magclass objects of length 0 by adding an element in the dimension
#' over which \code{dimSumsSpecial} is applied and set all values to 0 and it
#' will name the spatial dimension "GLO" if summation over this dimension happens.
#'
#' @param x A MAgPIE-object
#' @param dim The dimensions(s) to sum over. A vector of dimension codes or dimension names.
#' See \code{\link{dimCode}} for more information
#' @param na.rm logical. Should missing values (including NaN) be omitted from
#' the calculations?
#' @return A MAgPIE object with values summed over the specified dimensions
#' @importFrom magclass getItems<- getItems new.magpie
#' @author Jan Philipp Dietrich
dimSums <- function(x, dim = 3, na.rm = FALSE) { # nolint
  if (is.null(x)) return(x)
  if (length(x) == 0) {
    elem <- list()
    for (i in 1:3) {
      if (dim(x)[i] == 0) elem[[i]] <- "dummy"
      else elem[[i]] <- getItems(x, dim = i)
    }
    out <- new.magpie(elem[[1]], elem[[2]], elem[[3]], fill = 0)
    if (dim(x)[1] == 0) getItems(out, dim = 1) <- "GLO"
    if (dim(x)[2] == 0) getItems(out, dim = 2) <- NULL
    if (dim(x)[3] == 0) getItems(out, dim = 3) <- NULL
    return(out)
  }
  out <- magclass::dimSums(x, dim = dim, na.rm = na.rm)
  if (1 %in% dim) getItems(out, dim = 1) <- "GLO"
  return(out)
}

"getRegions<-" <- function(x, value) { #nolint
  getItems(x, dim = 1) <- value
  return(x)
}
