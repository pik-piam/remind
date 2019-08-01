#' Read OandM costs from GDX file
#' 
#' Read OandM costs data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param field one of 'l', 'm', 's', 'lo', 'up')
#' @return OandM costs data as MAgPIE object
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{readOandMcosts(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<-
readOandMcosts <- function(gdx, field='l') {
  tmp <- readGDX(gdx, c("v_costOM","v_costom"), field=field, format="first_found")
  getNames(tmp) <- attr(tmp, "description")
  return(tmp)
}
