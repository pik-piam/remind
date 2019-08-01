#' Calculates normalized prices of a commodity
#' 
#' Read PVP data of a certain commodity from a GDX file into a magpie object
#' and normalize it with the 2005 goods price.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param level spartiel resolution "glo" = global or "reg" = regional
#' @param enty one of the traded commodities in REMIND: good, pecoal, pegas,
#' peoil, pebiolc, peur, perm
#' @param type calculate present values (PVP(t)/PVP_Good(2005)) or nominal
#' values(PVP(t)/PVP_Good(t))
#' @return normalized price data as MAgPIE object
#' @author Niklas Roming
#' @examples
#' 
#'   \dontrun{
#'     coal <- calcPrice(gdx, enty = "pecoal")
#'     gas <- calcPrice(gdx, enty = "pegas")
#'   }
#' 
#' @export
#' @importFrom magclass getYears
calcPrice <- function(gdx,level="glo",enty="good",type="nominal"){
  
  tmp <- readPVP(gdx,level,enty)
  tmp1 <- readPVP(gdx,level,enty="good")
  
  if(type=="present"){
    
    # this is a workaround: previous to REMIND revision XXXX, fix2010 GDXes do not
    # contain prices for 2005 and 2010 (needed for normalization below)
    # Code below lloks if there is a BAU gdx and uses the prices given there
    if(tmp1[ ,2005 , ] == 0){
      if(file.exists(file.path(dirname(gdx), "input_bau.gdx"))){
        tmp1 <- readPVP(file.path(dirname(gdx), "input_bau.gdx"),
                        level, enty="good")
      } else stop("No prices for 2005 found (needed for normalization)!")
    }
    
    #normalization with the 2005 value
    tmp <- tmp / as.numeric(tmp1[, 2005, ])
    
  } else if(type=="nominal"){
    tmp <- tmp / as.numeric(tmp1[,getYears(tmp),])
  } else if(type=="raw"){
    tmp <- tmp
  }
  
  return(tmp)
}
