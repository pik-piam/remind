#' Read Fuelex from GDX file
#' 
#' Read Fuelex data for the specified enty (energy type) from a GDX file into a
#' magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param enty one of the energy types in REMIND: "pecoal", "pegas", "peoil",
#' "pebiolc", "peur", etc.
#' @return Fuelex data as MAgPIE object
#' @author David Klein, Lavinia Baumstark
#' @examples
#' 
#'   \dontrun{
#'     coal <- readFuelex(gdx, enty = "pecoal")
#'     gas <- readFuelex(gdx, enty = "pegas")
#'   }
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass collapseNames
readFuelex <- function(gdx,enty=NULL) {
  if (is.null(enty)) stop("No enty provided for readFuelex!")
  out <- readGDX(gdx,name=c("vm_fuExtr","vm_fuelex"), field="l", format="first_found")[,,enty]
  out <- collapseNames(out)
  return(out)
}
