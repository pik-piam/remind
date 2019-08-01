#' Read Investments|Non-ESM from GDX file
#' 
#' Read Investments|Non-ESM data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param field one of 'l', 'm', 's', 'lo', 'up')
#' @return Investments|Non-ESM data as MAgPIE object
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{readInvestmentsNonESM(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readInvestmentsNonESM <- function(gdx, field='l') {
  tmp <- readGDX(gdx, c("vm_invMacro","v_invest"), field=field, format="first_found",react="silent")[ , ,'kap']
  getNames(tmp) <- attr(tmp, "description")
  return(tmp)
}
