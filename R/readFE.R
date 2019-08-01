#' Read final energy from GDX file
#' 
#' Read final energy data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{fe <- readFE(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass dimSums getNames<-
readFE <- function(gdx){
  fe <- readGDX(gdx,c("vm_prodFe","v_feprod"),field="l",restore_zeros = FALSE,react="silent")
  fe <- dimSums(fe,dim=3)
  fe <- fe * 31.536  
  getNames(fe) <- "final energy [ EJ/yr]"
  return(fe)
}
