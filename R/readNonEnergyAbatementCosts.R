#' Read non-Energy Abatement costs from GDX file
#' 
#' Read non-Energy Abatement costs data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @return non-Energy Abatement costs data as MAgPIE object
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{readNonEnergyAbatementCosts(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readNonEnergyAbatementCosts <- function(gdx) {
  tmp <- readGDX(gdx, c('pm_macCost','p_maccost'), format="first_found")
  getNames(tmp) <- attr(tmp, "description")
  return(tmp)
}
