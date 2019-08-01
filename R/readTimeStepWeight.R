#' Read Time Step Weight from GDX file
#' 
#' Read Time Step Weight from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @return Time Step Weight as MAgPIE object
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{readTimeStepWeight(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readTimeStepWeight <- function(gdx) {
  tmp <- readGDX(gdx, c("pm_ts","ts"), format="first_found",react="silent")
  getNames(tmp) <- attr(tmp, "description")
  return(tmp)
}
