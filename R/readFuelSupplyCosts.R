#' Read Fuel supply costs from GDX file
#' 
#' Read Fuel supply costs data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param field one of 'l', 'm', 's', 'lo', 'up')
#' @return Fuel supply costs data as MAgPIE object
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{readFuelSupplyCosts(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readFuelSupplyCosts <- function(gdx, field='l') {
  tmp <- readGDX(gdx, name=c("v_costFu","v_costfu"), field=field, format="first_found")
  getNames(tmp) <- attr(tmp, "description")
  return(tmp)
}
