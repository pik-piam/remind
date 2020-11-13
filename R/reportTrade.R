#' Read in GDX and calculate trade, used in convGDX2MIF.R for the reporting
#' 
#' Read in trade information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @return MAgPIE object - contains the price variables
#' @author Lavinia Baumstark, Christoph Bertram, Anselm Schultes
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportTrade(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getYears setNames mbind dimSums

reportTrade <- function(gdx,regionSubsetList=NULL) {
  
  TWa_2_EJ        <- 31.536
  sm_tdptwyr2dpgj <- 31.71   #TerraDollar per TWyear to Dollar per GJ
  
  p_eta_conv       <- readGDX(gdx,name=c("pm_dataeta","p_eta_conv"),format = "first_found")
  p_costsPEtradeMp <- readGDX(gdx,name=c("pm_costsPEtradeMp","p_costsPEtradeMp"),format = "first_found")
  pm_pvp       <- readGDX(gdx,name=c("pm_pvp"),format = "first_found")
  Xport        <- readGDX(gdx,name=c("vm_Xport"),field = "l",format = "first_found")
  Mport        <- readGDX(gdx,name=c("vm_Mport"),field = "l",format = "first_found")
  
  # calculate maximal temporal resolution
  y <- Reduce(intersect,list(getYears(pm_pvp),getYears(Xport)))
  Xport      <- Xport[,y,]
  Mport      <- Mport[,y,]
  pm_pvp     <- pm_pvp[,y,]
  p_eta_conv <- p_eta_conv[,y,] 
  
  #AJS for current account 
  set_trade <- readGDX(gdx,name=c("trade"),types = 'sets',format="first_found")
  
  # calculate variables
  trade     <- Xport - Mport
  trade_net <- Xport - (1-p_costsPEtradeMp) * Mport
  price     <- pm_pvp / setNames(pm_pvp[,,"good"],NULL) #so in TeraDollar per either TWyr (pecoal,pegas,peoil,pebiolc), Gt C (perm), and Mt Uran (peUr) respectively
  
  # build reporting 
  tmp <- NULL  
  tmp <- mbind(tmp,setNames(trade_net[,,"pecoal"] * TWa_2_EJ,                                "Trade|Coal (EJ/yr)"))
  tmp <- mbind(tmp,setNames((1-p_costsPEtradeMp[,,"pecoal"]) * Mport[,,"pecoal"] * TWa_2_EJ, "Trade|Imports|Coal (EJ/yr)"))
  tmp <- mbind(tmp,setNames(Xport[,,"pecoal"] * TWa_2_EJ,                                    "Trade|Exports|Coal (EJ/yr)"))
  tmp <- mbind(tmp,setNames(trade_net[,,"pegas"] * TWa_2_EJ,                                 "Trade|Gas (EJ/yr)"))
  tmp <- mbind(tmp,setNames((1-p_costsPEtradeMp[,,"pegas"]) * Mport[,,"pegas"] * TWa_2_EJ,   "Trade|Imports|Gas (EJ/yr)"))
  tmp <- mbind(tmp,setNames(Xport[,,"pegas"] * TWa_2_EJ,                                     "Trade|Exports|Gas (EJ/yr)"))
  tmp <- mbind(tmp,setNames(trade_net[,,"peoil"] * TWa_2_EJ,                                 "Trade|Oil (EJ/yr)"))
  tmp <- mbind(tmp,setNames((1-p_costsPEtradeMp[,,"peoil"]) * Mport[,,"peoil"] * TWa_2_EJ,   "Trade|Imports|Oil (EJ/yr)"))
  tmp <- mbind(tmp,setNames(Xport[,,"peoil"] * TWa_2_EJ,                                     "Trade|Exports|Oil (EJ/yr)"))
  
  tmp <- mbind(tmp,setNames(trade_net[,,"peur"] * 1000,                                      "Trade|Uranium|Mass (ktU/yr)"))
  tmp <- mbind(tmp,setNames(trade_net[,,"peur"] * p_eta_conv[,,"tnrs"] * TWa_2_EJ,           "Trade|Uranium|seelEquivalent (EJ/yr)"))
  tmp <- mbind(tmp,setNames((1-p_costsPEtradeMp[,,"peur"]) * Mport[,,"peur"] * p_eta_conv[,,"tnrs"] * TWa_2_EJ, "Trade|Imports|Uranium (ktU/yr)"))
  tmp <- mbind(tmp,setNames(Xport[,,"peur"] * p_eta_conv[,,"tnrs"] * TWa_2_EJ,               "Trade|Exports|Uranium (ktU/yr)"))
  
  tmp <- mbind(tmp,setNames(trade_net[,,"good"] * 1000,                                       "Trade|Goods (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames((1-p_costsPEtradeMp[,,"good"]) * Mport[,,"good"] * 1000,          "Trade|Imports|Goods (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(Xport[,,"good"] * 1000,                                           "Trade|Exports|Goods (billion US$2005/yr)"))
  
  tmp <- mbind(tmp,setNames(trade_net[,,"pebiolc"] * TWa_2_EJ,                                "Trade|Biomass (EJ/yr)") )
  tmp <- mbind(tmp,setNames((1-p_costsPEtradeMp[,,"pebiolc"]) * Mport[,,"pebiolc"] * TWa_2_EJ,"Trade|Imports|Biomass (EJ/yr)") )
  tmp <- mbind(tmp,setNames(Xport[,,"pebiolc"]  * TWa_2_EJ,                                   "Trade|Exports|Biomass (EJ/yr)"))
  
  tmp <- mbind(tmp,setNames(trade_net[,,"perm"] * 44 / 12 * 1000,                             "Trade|Emi Allowances|Volume (Mt CO2-equiv/yr)"))
  tmp <- mbind(tmp,setNames((1-p_costsPEtradeMp[,,"perm"]) * Mport[,,"perm"] * 44 / 12 * 1000,"Trade|Imports|Emi Allowances|Volume (Mt CO2-equiv/yr)"))
  tmp <- mbind(tmp,setNames(Xport[,,"perm"] * 44 / 12 * 1000,                                 "Trade|Exports|Emi Allowances|Volume (Mt CO2-equiv/yr)"))
  
  # add global values
  tmp   <- mbind(tmp,dimSums(tmp,dim=1))
  trade <- mbind(trade,dimSums(trade,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList)){
    tmp   <- mbind(tmp,   calc_regionSubset_sums(tmp,   regionSubsetList))
    trade <- mbind(trade, calc_regionSubset_sums(trade, regionSubsetList))
  }
  
  # values use global prices
  tmp <- mbind(tmp,setNames(trade[,,"pecoal"] * price[,,"pecoal"] * 1000,                     "Trade|Primary Energy|Coal|Value (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(trade[,,"pegas"] * price[,,"pegas"] * 1000,                       "Trade|Primary Energy|Gas|Value (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(trade[,,"peoil"] * price[,,"peoil"] * 1000,                       "Trade|Primary Energy|Oil|Value (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(trade[,,"peur"] * price[,,"peur"] * 1000,                         "Trade|Uranium|Value (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(trade[,,"pebiolc"] * price[,,"pebiolc"] * sm_tdptwyr2dpgj,        "Trade|Biomass|Value (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(trade[,,"perm"] * price[,,"perm"] * 1000,                         "Trade|Emi Allowances|Value (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(dimSums( trade[,,set_trade] * price[,,set_trade],dim = 3) * 1000, "Trade|All|Value (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(dimSums( trade[,,set_trade] * price[,,set_trade],dim = 3),        "Current Account (billion US$2005/yr)"))
  
  # rename dimensions in the magpie object 
  names(dimnames(tmp))[3] = 'variable'
  
  return(tmp)
}
