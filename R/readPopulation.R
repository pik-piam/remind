#' Read Population from GDX file
#' 
#' Read Population data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{gdp <- readPopulation(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readPopulation <- function(gdx){
  pop <- readGDX(gdx, name=c('pm_pop','pm_datapop'), format="first_found") * 1000
  getNames(pop) <- "population [million]"
  return(pop)
}
