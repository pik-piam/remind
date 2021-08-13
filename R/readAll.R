#' @importFrom magclass ndata mbind
readAll <- function(pathToGdx, func, asList = TRUE, ...) {
  if (!is.list(pathToGdx)) {
    pathToGdx <- as.list(pathToGdx)
  }
  if (identical(names(pathToGdx), c(
    "aliases", "sets", "equations",
    "parameters", "variables"
  ))) {
    pathToGdx <- list(pathToGdx)
  }
  out <- list()
  for (i in seq_along(pathToGdx)) {
    out[[i]] <- func(pathToGdx[[i]], ...)
    if (!is.null(names(pathToGdx))) {
      names(out)[i] <- names(pathToGdx)[i]
    }
  }
  if (!all(lapply(out, ndata) == ndata(out[[1]]))) {
    stop("ERROR: different data dimensions. Can't readAll")
  }
  if (asList) {
    return(out)
  } else if (length(out) == 1) {
    out <- setNames(out[[1]], paste(names(out), getNames(out[[1]]),
      sep = "."
    ))
    getNames(out) <- sub("\\.$", "", getNames(out))
    getNames(out) <- sub("^\\.", "", getNames(out))
    return(out)
    print("no")
  } else {
    inp <- out
    if (is.null(names(inp))) {
      names(inp) <- seq_along(inp)
    }
    out <- NULL
    for (i in seq_along(inp)) {
      tmp <- inp[[i]]
      getNames(tmp) <- paste(names(inp)[i], getNames(tmp),
        sep = "."
      )
      out <- mbind(out, tmp)
    }
    getNames(out) <- sub("\\.$", "", getNames(out))
    return(out)
  }
}
