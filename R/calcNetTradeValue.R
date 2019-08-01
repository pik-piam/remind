#' Calculate monetary value of trade in primary energy from GDX file
#' 
#' Calculate monetary value of trade in primary energy carriers in a GDX file
#' and return as a MagPIE object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param variable one of the primary energy tyes in REMIND that ar: pecoal,
#' pegas, peoil, pebiolc, peur
#' @return Value (in US$2005) of trade
#' @author Niklas Roming
#' @seealso \code{\link{calcNetTrade}}, \code{\link{calcPrice}}
#' @examples
#' 
#'   \dontrun{
#'     coal <- calcNetTradeValue(gdx, "pecoal")
#'     gas <- calcNetTradeValue(gdx, "pegas")
#'   }
#' 
#' @export
#' @importFrom magclass getYears getNames<-
calcNetTradeValue <- function(gdx, variable) {
  trade <- calcNetTrade(gdx, variable)
  getNames(trade) <- attr(trade, "description")
  price <- calcPrice(gdx, enty=variable)
  getNames(price) <- attr(price, "description")
  years <- intersect(getYears(trade), getYears(price))
  return(trade[ , years, ] * price[ , years, ])
}
