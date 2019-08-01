#' Read Energy Investments from GDX file
#' 
#' Read Energy Investments data from a GDX file into a MagPIE object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param field one of 'l', 'm', 's', 'lo', 'up')
#' @return Energy Investments data as MAgPIE object
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{invests <- readEnergyInvestments(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readEnergyInvestments <- function(gdx, field='l') {
  tmp <- readGDX(gdx, name=c("v_costInv","v_costin"), field=field, format="first_found")
  getNames(tmp) <- attr(tmp, "description")
  return(tmp)
}
