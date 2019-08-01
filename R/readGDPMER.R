#' Read GDP|MER from GDX file
#' 
#' Read GDP|MER data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param field one of 'l', 'm', 's', 'lo', 'up'
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{gdp <- readGDPMER(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readGDPMER <- function(gdx, field="l") {
  tmp <- readGDX(gdx, c("vm_cesIO","v_vari"), field=field, format="first_found",react="silent")[ , ,'inco']
  getNames(tmp) <- attr(tmp, "description")
  return(tmp)
}
