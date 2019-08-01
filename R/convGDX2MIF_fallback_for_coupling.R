#' Read in GDX and calculate only coupling variables
#' 
#' @description Read in GDX and calculate only variables that are required for MAgPIE coupling,
#' i.e. GHG prices and bioenergy demand.
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param file name of the mif file which is written, if no name is
#' provided a magpie object containing the reporting is returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2030,2050)
#' @importFrom luscale speed_aggregate
#' @author David Klein
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{convGDX2MIF_fallback_for_coupling(gdx)}
#' 
#' @export
#' @importFrom magclass mbind getYears getRegions setNames dimSums
#' @importFrom gdx readGDX
convGDX2MIF_fallback_for_coupling <- function(gdx,file=NULL,scenario="default",t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)) {
  pm_pvpRegi <- readGDX(gdx,name='pm_pvpRegi',format="first_found")[,,"perm"]
  pm_pvp     <- readGDX(gdx,name=c("pm_pvp","p80_pvp"),format="first_found")[,,"good"]
  s_GWP_CH4  <- readGDX(gdx,c("sm_gwpCH4","s_gwpCH4","s_GWP_CH4"),format="first_found")
  s_GWP_N2O  <- readGDX(gdx,c("s_gwpN2O","s_GWP_N2O"),format="first_found")
  TWa_2_EJ   <- as.numeric(1/readGDX(gdx, c("sm_EJ_2_TWa","sm_ej2twyr"), format="first_found"))
  
  fuelex     <- readGDX(gdx,name=c("vm_fuExtr","vm_fuelex"), field="l", restore_zeros=FALSE, format="first_found")
  #fuelex     <- mbind(fuelex,     dimSums(fuelex,dim=1))
  
  y <- Reduce(intersect,list(getYears(pm_pvp),getYears(pm_pvpRegi),getYears(fuelex)))
  pm_pvpRegi     <- pm_pvpRegi[,y,]
  pm_pvp         <- pm_pvp[,y,]
  fuelex_bio     <- fuelex[,y,"pebiolc.1"]
  
  out <- NULL
  out <- mbind(out,setNames(abs(pm_pvpRegi / (pm_pvp[,,"good"] + 1e-10)) * 1000 * 12/44, "Price|Carbon (US$2005/t CO2)"))
  out <- mbind(out,setNames(out[,,"Price|Carbon (US$2005/t CO2)"] * s_GWP_N2O, "Price|N2O (US$2005/t N2O)"))
  out <- mbind(out,setNames(out[,,"Price|Carbon (US$2005/t CO2)"] * s_GWP_CH4, "Price|CH4 (US$2005/t CH4)"))
  out <- mbind(out,setNames(dimSums(fuelex_bio, dim=3) * TWa_2_EJ,"Primary Energy Production|Biomass|Energy Crops (EJ/yr)"))
  
  if(!is.null(file)) write.report(out[,t,],model="REMIND",scenario=scenario,file=file,ndigit=7)
  else return(out)
}