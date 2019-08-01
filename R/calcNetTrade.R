#' Calculate net trade from GDX file
#' 
#' Calculate net trade in a commodity from a GDX file and store it into a
#' magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param variable one of the traded commodities in REMIND: good, pecoal,
#' pegas, peoil, pebiolc, peur, perm
#' @return Trade data as MAgPIE object
#' @author Niklas Roming
#' @seealso \code{\link{readTrade}}, \code{\link{calcNetTradeValue}}
#' @examples
#' 
#'   \dontrun{
#'     coal <- calcNetTrade(gdx, "pecoal")
#'     gas <- calcNetTrade(gdx, "pegas")
#'   }
#' 
#' @export
calcNetTrade <- function(gdx, variable){
  exports <- readTrade(gdx, type = "Exports", variable)
  imports <- readTrade(gdx, type = "Imports", variable)
  trade <- exports - imports
  return(trade)
}
