#' Read Consumption from GDX file
#' 
#' Read Consumption data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param field one of 'l', 'm', 's', 'lo', 'up'
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{cons <- readConsumption(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind dimSums getNames<-

readConsumption <- function(gdx, field="l"){
  tmp <- readGDX(gdx, name='vm_cons', field=field, format="first_found")
  tmp <- tmp * 1000
  tmp <- mbind(tmp["GLO",,,invert=T],dimSums(tmp,dim=1))
  getNames(tmp) <- "Consumption (billion US$2005/yr)"
  return(tmp)
}  
