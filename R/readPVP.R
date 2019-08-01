#' Read PVP ("Present value price") from GDX file
#' 
#' Read PVP data of a certain commodity from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param level spartiel resolution "glo" = global or "reg" = regional
#' @param enty one of the traded commodities in REMIND: "good", "pecoal",
#' "pegas", "peoil", "pebiolc", "peur", "perm"
#' @return PVP data as MAgPIE object
#' @author Jonas Hoersch
#' @examples
#' 
#'   \dontrun{
#'     coal <- readPVP(gdx, enty = "pecoal")
#'     gas <- readPVP(gdx, enty = "pegas")
#'   }
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie getYears
readPVP <- function(gdx, level="glo", enty="good"){
  tmp <- readGDX(gdx, name = c("pm_pvp","p80_pvp"), format="first_found")[ , , enty]
  if(level=="reg"){
    regi <- as.matrix(readGDX(gdx, name = "all_regi", format="first_found"))
    tmp1 <- new.magpie(regi,getYears(tmp),)
    tmp1[regi,,] <- tmp
    #getRegions(tmp1) <- sub("glob","GLO",getRegions(tmp1))
    tmp <- tmp1
       if(level=="reg" & enty=="perm"){
            tmp <- readGDX(gdx, name = c("pm_pvpRegi","p80_pvpRegi"), format="first_found")[ , , enty]
       }
  }
  
#   getNames(tmp) <- attr(tmp, "description") # this causes trouble due to ambiguous set definition
  
  return(tmp)
}
