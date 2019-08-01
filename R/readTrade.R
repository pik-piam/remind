#' Read Trade from GDX file
#' 
#' Read Exports or Imports of a commodity from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param type one of \code{"Imports"} or \code{"Exports"}
#' @param variable one of the traded commodities in REMIND: good, pecoal,
#' pegas, peoil, pebiolc, peur, perm
#' @return Trade data as MAgPIE object
#' @author Niklas Roming
#' @examples
#' 
#'   \dontrun{
#'     coal <- readTrade(gdx, "Exports", "pecoal") # Coal exports
#'     gas <- readTrade(gdx, "Imports", "pegas") # Gas exports
#'   }
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readTrade <- function(gdx, type, variable){
  switch(type,
         "Imports" = {
           tmp <- readGDX(gdx, "vm_Mport", field="l", format="first_found")[ , , variable]
         },
         "Exports" = {
           tmp <- readGDX(gdx, "vm_Xport", field="l", format="first_found")[ , , variable]
         },
{stop('"type" must be either "Imports" or "Exports"')}
  )

getNames(tmp) <- attr(tmp, "description")

return(tmp)
}
