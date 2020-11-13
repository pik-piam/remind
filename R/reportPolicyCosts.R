#' Read in GDX and calculate policy costs, used in convGDX2MIF.R for the
#' reporting
#' 
#' Read in GDX and calculate policy costs functions
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param gdx_ref a reference GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{reportPolicyCosts(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getYears mbind dimSums setNames
reportPolicyCosts <- function(gdx,gdx_ref,regionSubsetList=NULL){
  ####### read in needed data #########
  ## sets
  trade_bau <- readGDX(gdx_ref,name=c("trade"),format="first_found")
  trade     <- readGDX(gdx,name=c("trade"),format="first_found")
  ## parameter
  pm_pvp_bau<- readGDX(gdx_ref,name=c("pm_pvp"),format = "first_found")
  pm_pvp    <- readGDX(gdx,name=c("pm_pvp"),format = "first_found")
  ## variables
  cons_bau  <- readGDX(gdx_ref,name=c("vm_cons"),field="l",restore_zeros=FALSE,format="first_found") 
  gdp_bau   <- readGDX(gdx_ref,name=c("vm_cesIO"),field="l",restore_zeros=FALSE,format="first_found")[,,"inco"]
  Xport_bau <- readGDX(gdx_ref,name=c("vm_Xport"),field = "l",format = "first_found")
  Mport_bau <- readGDX(gdx_ref,name=c("vm_Mport"),field = "l",format = "first_found")
  cons      <- readGDX(gdx,name=c("vm_cons"),field="l",restore_zeros=FALSE,format="first_found")
  gdp       <- readGDX(gdx,name=c("vm_cesIO"),field="l",restore_zeros=FALSE,format="first_found")[,,"inco"]
  Xport     <- readGDX(gdx,name=c("vm_Xport"),field = "l",format = "first_found")
  Mport     <- readGDX(gdx,name=c("vm_Mport"),field = "l",format = "first_found")
  v_costfu_bau         <- readGDX(gdx_ref,name=c("v_costFu","v_costfu"),         field="l",restore_zeros=FALSE,format="first_found")
  v_costom_bau         <- readGDX(gdx_ref,name=c("v_costOM","v_costom"),         field="l",                    format="first_found")
  v_costin_bau         <- readGDX(gdx_ref,name=c("v_costInv","v_costin"),         field = "l",format = "first_found")
  v_costfu         <- readGDX(gdx,name=c("v_costFu","v_costfu"),         field="l",restore_zeros=FALSE,format="first_found")
  v_costom         <- readGDX(gdx,name=c("v_costOM","v_costom"),         field="l",                    format="first_found")
  v_costin         <- readGDX(gdx,name=c("v_costInv","v_costin"),         field = "l",format = "first_found")
  
  ####### calculate minimal temporal and regional resolution #####
  y <- Reduce(intersect,list(getYears(cons),getYears(gdp),getYears(Xport),getYears(Mport),getYears(pm_pvp)))
  cons_bau  <- cons_bau[,y,]
  gdp_bau   <- gdp_bau[,y,]
  Xport_bau <- Xport_bau[,y,]
  Mport_bau <- Mport_bau[,y,]
  pm_pvp_bau <- pm_pvp_bau[,y,]
  cons      <- cons[,y,]
  gdp       <- gdp[,y,]
  Xport     <- Xport[,y,]
  Mport     <- Mport[,y,]
  pm_pvp    <- pm_pvp[,y,]
  v_costfu_bau <- v_costfu_bau[,y,]
  v_costin_bau <- v_costin_bau[,y,]
  v_costom_bau <- v_costom_bau[,y,]
  v_costfu <- v_costfu[,y,]
  v_costin <- v_costin[,y,]
  v_costom <- v_costom[,y,]
  ####### add global values
  cons_bau   <- mbind(cons_bau,dimSums(cons_bau,dim=1))
  gdp_bau    <- mbind(gdp_bau,dimSums(gdp_bau,dim=1))
  Xport_bau  <- mbind(Xport_bau,dimSums(Xport_bau,dim=1))
  Mport_bau  <- mbind(Mport_bau,dimSums(Mport_bau,dim=1))
  v_costfu_bau <- mbind(v_costfu_bau,dimSums(v_costfu_bau,dim=1))
  v_costin_bau <- mbind(v_costin_bau,dimSums(v_costin_bau,dim=1))
  v_costom_bau <- mbind(v_costom_bau,dimSums(v_costom_bau,dim=1))
  cons       <- mbind(cons,dimSums(cons,dim=1))
  gdp        <- mbind(gdp,dimSums(gdp,dim=1))
  Xport      <- mbind(Xport,dimSums(Xport,dim=1))
  Mport      <- mbind(Mport,dimSums(Mport,dim=1))
  v_costfu <- mbind(v_costfu,dimSums(v_costfu,dim=1))
  v_costin <- mbind(v_costin,dimSums(v_costin,dim=1))
  v_costom <- mbind(v_costom,dimSums(v_costom,dim=1))
  ####### some pre-calculations
  currAcc_bau <- dimSums( (Xport_bau[,,trade] - Mport_bau[,,trade] ) * pm_pvp_bau[,,trade]/setNames(pm_pvp_bau[,,'good'],NULL),dim = 3)
  currAcc     <- dimSums( (Xport[,,trade] - Mport[,,trade] ) * pm_pvp[,,trade]/setNames(pm_pvp[,,'good'],NULL),dim = 3)
  ####### calculate reporting parameters ############
  tmp <- NULL 
  tmp <- mbind(tmp,setNames((cons_bau - cons) * 1000, "Policy Cost|Consumption Loss (billion US$2005/yr)" ))
  tmp <- mbind(tmp,setNames((cons_bau - cons)/(cons_bau + 1e-10) * 100, "Policy Cost|Consumption Loss|Relative to Reference Consumption (percent)")) 
  tmp <- mbind(tmp,setNames((gdp_bau - gdp) * 1000, "Policy Cost|GDP Loss (billion US$2005/yr)" ))
  tmp <- mbind(tmp,setNames((gdp_bau - gdp)/(gdp_bau + 1e-10) * 100, "Policy Cost|GDP Loss|Relative to Reference GDP (percent)")) 
  tmp <- mbind(tmp,setNames((v_costfu +v_costin + v_costom  - (v_costfu_bau + v_costin_bau + v_costom_bau)) * 1000, "Policy Cost|Additional Total Energy System Cost (billion US$2005/yr)" ))
  # Policy costs calculated as consumption losses net the effect of climate-policy induced changes in the current account  
  tmp <- mbind(tmp,setNames(((cons_bau + currAcc_bau) - (cons + currAcc)) * 1000, "Policy Cost|Consumption + Current Account Loss (billion US$2005/yr)" ))
  tmp <- mbind(tmp,setNames(((cons_bau + currAcc_bau) - (cons + currAcc))/(cons_bau + currAcc_bau + 1e-10) * 100, "Policy Cost|Consumption + Current Account Loss|Relative to Reference Consumption + Current Account (percent)" ))
  
  # add other region aggregations
  if (!is.null(regionSubsetList))
    tmp <- mbind(tmp, calc_regionSubset_sums(tmp, regionSubsetList))
  
  return(tmp)
}

