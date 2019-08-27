#' Calculate and report relevant fossil fuel cost and quantity variables from MOFEX standalone model.
#' 
#' @param gdx the filepath to a gdx
#' @param gdx_ref reference-gdx for policy costs, a GDX as created by readGDX, or the file name of a gdx
#' @param file path to output .mif file
#' @param scenario scenario name that is used in the *.mif reporting
#' @return MAgPIE object - contains the output from a MOFEX standalone run
#' @author Steve Bi
#' @seealso \code{\link{convGDX2MIF}} \code{\link{reportPE}} \code{\link{reportCosts}}
#' @examples
#' 
#' \dontrun{reportMOFEX(gdx)}
#' 
#' @export
#' @importFrom rlang .data
#' @importFrom magclass mbind getYears collapseNames dimSums setNames mselect as.magpie write.report
#' @importFrom gdx readGDX
#' @importFrom dplyr filter 
#' @importFrom ggplot2 ggplot aes labs geom_area geom_line geom_col scale_fill_brewer facet_grid theme_minimal
#' @importFrom quitte as.quitte
#' @importFrom luscale speed_aggregate

reportMOFEX <- function(gdx,gdx_ref=NULL,file=NULL,scenario='default') {
  
  if (is.null(gdx))  stop("GDX file is missing!")
  igdx(gamsSysDir = "C:/Program Files/GAMS/26.1")

  ####### conversion factors ##########
  TWa_2_EJ     <- 31.536
  tdptwyr2dpgj <- 31.71   #TerraDollar per TWyear to Dollar per GJ
  
  ####### read in needed data #########
  
  fuExtr <- readGDX(gdx,c("vm_fuExtr","vm_fuelex"),field="l",format="first_found")*TWa_2_EJ
  Mport  <- readGDX(gdx,c("vm_Mport"),field="l",format="first_found")*TWa_2_EJ
  Xport  <- readGDX(gdx,c("vm_Xport"),field="l",format="first_found")*TWa_2_EJ
  costFuEx  <- readGDX(gdx,name=c("vm_costFuEx","vm_costfu_ex"),field="l",restore_zeros=FALSE,format="first_found")
  prodPe <- readGDX(gdx,"vm_prodPe",field="l",format="first_found")*TWa_2_EJ
  fuExtrCum <- readGDX(gdx,"v31_fuExtrCum",field="l",format="first_found")*TWa_2_EJ
#  v31_MOFEX_costMinFuelEx <- readGDX(gdx,"v31_MOFEX_costMinFuelEx",field="l",format="first_found")
  
  ## parameters
  grades            <- readGDX(gdx,name=c("p31_grades"), restore_zeros=FALSE, format="first_found", react="silent")
  if(!is.null(grades)) { grades[is.na(grades)] <- 0  }  # substitute na by 0 
  p_cint            <- readGDX(gdx,name=c("pm_cint","p_cint"), format="first_found", react = "silent")
  pm_fuExtrOwnCons <- readGDX(gdx,"pm_fuExtrOwnCons",field="l",format="first_found")*TWa_2_EJ
  pm_pvp        <- readGDX(gdx,name=c("pm_pvp","p80_pvp"),format="first_found")
  p_costsPEtradeMp <- readGDX(gdx,"p_costsPEtradeMp",field="l",format="first_found")*TWa_2_EJ
  
  ## sets
  t <- as.numeric(readGDX(gdx,"ttot"))
  t <- t[t>=2005]  
  set_trade <- readGDX(gdx,name=c("trade"),types = 'sets',format="first_found")
  ts           <- readGDX(gdx,"pm_ts")[,t,]
  
  ## equations
  fuel2pe.m <- readGDX(gdx,name="qm_fuel2pe",types = "equations",field = "m")
  budget.m <- readGDX(gdx,name='qm_budget',types = "equations",field = "m",format = "first_found") # Alternative: calcPrice
  tradebal.m <- readGDX(gdx,name="q31_MOFEX_tradebal",types = "equations",field = "m")
  
  ## Filter time period and fossil fuels
  fuExtr <- fuExtr[,t,c("pegas","pecoal","peoil")]
  costFuEx <- costFuEx[,t,c("pegas","pecoal","peoil")]
  fuExtrCum <- fuExtrCum[,,c("pegas","pecoal","peoil")]
  Xport <- Xport[,t,]
  Mport <- Mport[,t,]
  pm_pvp <- pm_pvp[,t,]
  grades <- grades[,t,]
  prodPe <- prodPe[,t,]
  fuel2pe.m <- fuel2pe.m[,t,]
  budget.m <- budget.m[,t,]
  tradebal.m <- tradebal.m[,t,]
  p_cint          <- collapseNames(mselect(p_cint,all_enty="co2"))
  getSets(p_cint) <- gsub("all_enty1","all_enty",getSets(p_cint))
  mappegrad <- dimnames(grades[,,"xi1",drop=TRUE])$all_enty.rlf
  cost_per_GJ <- (costFuEx / dimSums(fuExtr,dim=3))*1e3
  if (!is.null(pm_fuExtrOwnCons)) {
    pm_fuExtrOwnCons <- mselect(pm_fuExtrOwnCons, 
                           all_enty  = gsub("\\.[0-9]","",mappegrad),
                           all_enty1 = gsub("\\.[0-9]","",mappegrad))
  }
  
  # calculate trade variables
  trade     <- Xport - Mport
  trade_net <- Xport - (1-p_costsPEtradeMp) * Mport
  price     <- pm_pvp / setNames(pm_pvp[,,"good"],NULL) #so in TeraDollar per either TWyr (pecoal,pegas,peoil,pebiolc), Gt C (perm), and Mt Uran (peUr) respectively
  
  
  
  ####### internal functions for reporting ###########
  calcAvgExtractionCosts <- function(i_pe, i_fuelex, i_fuelexcum, i_grades) {
    # Select regional information because grades are only defined for regions
    # and not at the global level
    tmp_fuelex    <- i_fuelex[   which(getRegions(i_fuelex)    != "GLO"),,] 
    tmp_fuelexcum <- i_fuelexcum[which(getRegions(i_fuelexcum) != "GLO"),,] 
    
    # Generate magpie object containing timestep periods
    tmp_ts <- new.magpie(getRegions(tmp_fuelex),t)
    tmp_ts[,,] <- ts
    
    # Filter out unnecessary data
    xi1 <- mselect(i_grades, xirog = "xi1", all_enty = i_pe)[,,,drop=TRUE]
    xi2 <- mselect(i_grades, xirog = "xi2", all_enty = i_pe)[,,,drop=TRUE]
    xi3 <- mselect(i_grades, xirog = "xi3", all_enty = i_pe)[,,,drop=TRUE]
    tmp_fuelexcum           <- tmp_fuelexcum[,,i_pe,drop=TRUE][,2150,,invert=TRUE]
    tmp_fuelex              <- tmp_fuelex[,,i_pe,drop=TRUE]
    getYears(tmp_fuelexcum) <- c(getYears(tmp_fuelexcum)[-1], "y2150")
    
    # deal with xi3=0 
    tmp_fuelexcum_xi3 <- tmp_fuelexcum/xi3
    tmp_fuelexcum_xi3[is.na(tmp_fuelexcum_xi3)] <- 0
    tmp_fuelex_xi3    <- tmp_fuelex   /xi3
    tmp_fuelex_xi3[is.na(tmp_fuelex_xi3)]  <- 0
    
    # Compute average extraction costs
    o_aec <- dimSums(( xi1 +                                          # minimum extraction cost
                         (xi2-xi1) * (tmp_fuelexcum_xi3) +               # contribution of already extracted FF (in previous timesteps) to cost increase
                         (xi2-xi1) * (tmp_fuelex_xi3) * (tmp_ts/2.0)     # contribution of extracted FF in current timestep to cost increase
    ) * tmp_fuelex, dim=3) /                         # total costs of extracting FF in current timestep
      dimSums(tmp_fuelex, dim=3) *                   # average costs of extracting FF in current timestep
      1000                                           # convert from T$/EJ to $/GJ
    o_aec[is.na(o_aec)] <- 0
    # Compute and add global values (as a mean over regions)
    o_aec <- mbind(o_aec, dimSums(o_aec,dim=1)/length(getRegions(tmp_fuelex)))
    
    return(o_aec)
  }
  
  calcPeProductionNet <- function(i_fuelex, i_cint, i_fuExtrOwnCons, i_pe) {
    
    ownCons <- collapseNames(mselect(i_fuExtrOwnCons,all_enty=i_pe))
    getSets(ownCons)  <- gsub("all_enty1","all_enty",getSets(ownCons))
    
    tmp <-   ownCons * dimSums(i_fuelex[,,magclass::getNames(mselect(i_fuExtrOwnCons,all_enty=i_pe),dim=2)],dim=3.2)
    
    o_ppn <-  dimSums(i_fuelex[,,i_pe] + i_cint[,,i_pe] * i_fuelex[,,i_pe], dim=3) - dimSums(tmp,dim=3 )
    
    return(o_ppn)
  }
  
  
  ####### calculate reporting parameters ############
  # Resource Extraction
  peQtys <- mbind(
    setNames(dimSums(fuExtr[,,"pecoal"]+p_cint[,,"pecoal"]*fuExtr[,,"pecoal"],dim=3),  "Res|Extraction|Coal (EJ/yr)"),
    setNames(dimSums(fuExtr[,,"peoil"]+p_cint[,,"peoil"]*fuExtr[,,"peoil"],dim=3),     "Res|Extraction|Oil (EJ/yr)"),
    setNames(dimSums(fuExtr[,,"pegas"]+p_cint[,,"pegas"]*fuExtr[,,"pegas"],dim=3),     "Res|Extraction|Gas (EJ/yr)"),
    setNames(fuExtr[,,"pecoal"]+p_cint[,,"pecoal"]*fuExtr[,,"pecoal"],     paste("Res|Extraction|Coal Grade", 1:12,"(EJ/yr)")),
    setNames(fuExtr[,,"peoil"]+p_cint[,,"peoil"]*fuExtr[,,"peoil"],        paste("Res|Extraction|Oil Grade", 1:12,"(EJ/yr)")),
    setNames(fuExtr[,,"pegas"]+p_cint[,,"pegas"]*fuExtr[,,"pegas"],        paste("Res|Extraction|Gas Grade", 1:12,"(EJ/yr)")),
    setNames(dimSums(fuExtrCum[,t,"pecoal"],dim=3),         "Res|Extraction|Cumulative Coal (EJ)"),
    setNames(dimSums(fuExtrCum[,t,"peoil"],dim=3),          "Res|Extraction|Cumulative Oil (EJ)"),
    setNames(dimSums(fuExtrCum[,t,"pegas"],dim=3),          "Res|Extraction|Cumulative Gas (EJ)"),
    setNames(fuExtrCum[,t,"pecoal"],                        paste("Res|Extraction|Cumulative Coal Grade", 1:12,"(EJ/yr)")),
    setNames(fuExtrCum[,t,"peoil"],                         paste("Res|Extraction|Cumulative Oil Grade", 1:12,"(EJ/yr)")),
    setNames(fuExtrCum[,t,"pegas"],                         paste("Res|Extraction|Cumulative Gas Grade", 1:12,"(EJ/yr)")),
    setNames(prodPe[,t,"pecoal"],                           "PE|Coal (EJ/yr)"),
    setNames(prodPe[,t,"peoil"],                            "PE|Oil (EJ/yr)"),
    setNames(prodPe[,t,"pegas"],                            "PE|Gas (EJ/yr)")
  )
  ####### add global value #############
  peQtys <- mbind(peQtys,dimSums(peQtys,dim=1))
  
  # PE|Production based on extraction
  if (!is.null(pm_fuExtrOwnCons)) {
    peNetQtys <- mbind(
    setNames(calcPeProductionNet(fuExtr,p_cint,pm_fuExtrOwnCons,i_pe="pecoal"),  "PE|Production|Net|Coal (EJ/yr)"),
    setNames(calcPeProductionNet(fuExtr,p_cint,pm_fuExtrOwnCons,i_pe="peoil"),   "PE|Production|Net|Oil (EJ/yr)"),
    setNames(calcPeProductionNet(fuExtr,p_cint,pm_fuExtrOwnCons,i_pe="pegas"),   "PE|Production|Net|Gas (EJ/yr)")
  )
  ####### add global value #############
  peNetQtys <- mbind(peNetQtys,dimSums(peNetQtys,dim=1))
  }else  peNetQtys <- NULL
  
  # Average supply costs
  peSupplyCosts <- mbind(
    setNames(dimSums(costFuEx,dim=3),                    "Extraction Costs|Total Fossil (tril$US)"),
    setNames(dimSums(costFuEx[,,"pecoal"],dim=3),        "Extraction Costs|Coal (tril$US)"),
    setNames(dimSums(costFuEx[,,"peoil"],dim=3),         "Extraction Costs|Oil (tril$US)"),
    setNames(dimSums(costFuEx[,,"pegas"],dim=3),         "Extraction Costs|Gas (tril$US)"),
    setNames(dimSums(cost_per_GJ,dim=3),                 "Extraction Unit Costs|Total Fossil (US$2015/GJ)"),
    setNames(cost_per_GJ[,,"pecoal"],     "Extraction Unit Costs|+|Coal (US$2015/GJ)"),
    setNames(cost_per_GJ[,,"peoil"],      "Extraction Unit Costs|+|Oil (US$2015/GJ)"),
    setNames(cost_per_GJ[,,"pegas"],      "Extraction Unit Costs|+|Gas (US$2015/GJ)")
  )
  ####### add global value #############
  peSupplyCosts <- mbind(peSupplyCosts,dimSums(peSupplyCosts,dim=1))
  
  
  # Average extraction costs
  peExtrCosts <- mbind(  
    setNames(calcAvgExtractionCosts("pecoal", fuExtr, fuExtrCum[,c(2000,t),], grades),               "Res|Average Extraction Costs|Coal (US$2015/GJ)"),
    setNames(calcAvgExtractionCosts("peoil",  fuExtr, fuExtrCum[,c(2000,t),], grades),               "Res|Average Extraction Costs|Oil (US$2015/GJ)"),
    setNames(calcAvgExtractionCosts("pegas",  fuExtr, fuExtrCum[,c(2000,t),], grades),               "Res|Average Extraction Costs|Gas (US$2015/GJ)")
  )
  
  
  # Trade volumes
  peFossilTrade <- NULL  
  peFossilTrade <- mbind(peFossilTrade,setNames(trade_net[,,"pecoal"],                                "Trade|Coal (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames((1-p_costsPEtradeMp[,,"pecoal"]) * Mport[,,"pecoal"], "Trade|Imports|Coal (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames(Xport[,,"pecoal"] ,                                   "Trade|Exports|Coal (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames(trade_net[,,"pegas"],                                 "Trade|Gas (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames((1-p_costsPEtradeMp[,,"pegas"]) * Mport[,,"pegas"],   "Trade|Imports|Gas (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames(Xport[,,"pegas"],                                     "Trade|Exports|Gas (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames(trade_net[,,"peoil"],                                 "Trade|Oil (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames((1-p_costsPEtradeMp[,,"peoil"]) * Mport[,,"peoil"],   "Trade|Imports|Oil (EJ/yr)"))
  peFossilTrade <- mbind(peFossilTrade,setNames(Xport[,,"peoil"],                                     "Trade|Exports|Oil (EJ/yr)"))
  # add global values
  peFossilTrade   <- mbind(peFossilTrade,dimSums(peFossilTrade,dim=1))
  trade <- mbind(trade,dimSums(trade,dim=1))
  
  # Fossil Fuel Prices
  peFossilPrices <- NULL
  peFossilPrices <- mbind(peFossilPrices,setNames(fuel2pe.m[,,"pecoal"]/(budget.m+1e-10) * tdptwyr2dpgj,   "Price|Coal|Primary Level (US$2005/GJ)"))
  peFossilPrices <- mbind(peFossilPrices,setNames(fuel2pe.m[,,"peoil"]/(budget.m+1e-10) * tdptwyr2dpgj,    "Price|Crude Oil|Primary Level (US$2005/GJ)"))
  peFossilPrices <- mbind(peFossilPrices,setNames(fuel2pe.m[,,"pegas"]/(budget.m+1e-10) * tdptwyr2dpgj,    "Price|Natural Gas|Primary Level (US$2005/GJ)"))
#  peFossilPrices <- mbind(peFossilPrices,setNames(lowpass(fuel2pe.m[,,"pegas"]/(budget.m+1e-10), fix="both", altFilter=match(2010,t)) * tdptwyr2dpgj,    "Price|Natural Gas|Primary Level|Moving Avg (US$2005/GJ)"))
  # mapping of weights for the variables for global aggregation
  int2ext <- c(
    "Price|Coal|Primary Level (US$2005/GJ)"                   = "PE|Coal (EJ/yr)",
    "Price|Crude Oil|Primary Level (US$2005/GJ)"              = "PE|Oil (EJ/yr)",
    "Price|Natural Gas|Primary Level (US$2005/GJ)"            = "PE|Gas (EJ/yr)"
#    "Price|Natural Gas|Primary Level|Moving Avg (US$2005/GJ)" = "PE|Production|Net|Gas (EJ/yr)"
  )
  map <- data.frame(region=getRegions(peFossilPrices),world="GLO",stringsAsFactors=FALSE)
  peFossilPrices_GLO <- new.magpie("GLO",getYears(peFossilPrices),magclass::getNames(peFossilPrices),fill=0)
  
  for (i2e in names(int2ext)){ 
    peFossilPrices_GLO["GLO",,i2e] <- speed_aggregate(peFossilPrices[,,i2e],map,weight=peQtys[map$region,,int2ext[i2e]])
    for(t in getYears(peFossilPrices)){
      if(all(peQtys[map$region,t,int2ext[i2e]]==0)){
        peFossilPrices_GLO["GLO",t,i2e] <- NA
      }
    }
  }
  peFossilPrices <- mbind(peFossilPrices,peFossilPrices_GLO)
  
    
  # Global Market FF Prices
  peFossilGloPrices <- NULL
  glob_price <- new.magpie(getRegions(peQtys),getYears(peQtys),fill=0)
  for(i in getRegions(glob_price)) glob_price[i,,] <- tradebal.m[,,"peoil"] * tdptwyr2dpgj
  peFossilGloPrices <- mbind(peFossilGloPrices,setNames(glob_price,                                       "Price|Oil|World Market (US$2005/GJ)")) 
  for(i in getRegions(glob_price)) glob_price[i,,] <- tradebal.m[,,"pegas"] * tdptwyr2dpgj
  peFossilGloPrices <- mbind(peFossilGloPrices,setNames(glob_price,                                       "Price|Gas|World Market (US$2005/GJ)"))
  for(i in getRegions(glob_price)) glob_price[i,,] <- tradebal.m[,,"pecoal"] * tdptwyr2dpgj
  peFossilGloPrices <- mbind(peFossilGloPrices,setNames(glob_price,                                       "Price|Coal|World Market (US$2005/GJ)"))
  for(i in getRegions(glob_price)) glob_price[i,,] <- pm_pvp[,,"peoil"]*1000
  peFossilGloPrices <- mbind(peFossilGloPrices,setNames(glob_price,                             "PVP1|Oil (billionDpTWyr)"))
  for(i in getRegions(glob_price)) glob_price[i,,] <- pm_pvp[,,"pegas"]*1000
  peFossilGloPrices <- mbind(peFossilGloPrices,setNames(glob_price,                             "PVP1|Gas (billionDpTWyr)"))
  for(i in getRegions(glob_price)) glob_price[i,,] <- pm_pvp[,,"pecoal"]*1000
  peFossilGloPrices <- mbind(peFossilGloPrices,setNames(glob_price,                             "PVP1|Coal (billionDpTWyr)"))

  
  output <- mbind(peQtys,peNetQtys,peSupplyCosts,peFossilPrices,peFossilGloPrices,peFossilTrade,peExtrCosts)
  getSets(output)[3] <- "variable"
  output <- add_dimension(output,dim=3.1,add = "model",nm = "REMIND")
  output <- add_dimension(output,dim=3.1,add = "scenario",nm = scenario)
  
  ##-----------------------------------------------PLOTTING ROUTINES-----------------------------------------------##
  
  ## Convert to quitte objects
  fuExtrCum <- as.quitte(fuExtrCum)
  fuExtr <- as.quitte(fuExtr)
  costFuEx <- as.quitte(costFuEx)
  prodPe <- as.quitte(prodPe)
  
  ## Generate ggplot objects
  glo_extr_grade <-
    ggplot(fuExtr%>%filter(rlf %in% 1:6),aes(x=period,y=value,fill=rlf,group=interaction(region,rlf))) + 
    labs(y="Extraction (EJ)",x="year",title="Global Fossil Extraction by Cost Grade") +
    geom_area(position=position_stack(reverse = TRUE)) + 
    scale_fill_brewer(palette = "Dark2",name="grade") + 
    facet_grid(all_enty ~ .)
  
  cost_enty_regi <- 
    ggplot(costFuEx, aes(x=period,y=value,fill=all_enty,color=all_enty,group=region)) +
    geom_line() + 
    facet_grid(region ~ all_enty) + 
    theme_minimal()
  
  extr_enty_regi <-
    ggplot(fuExtr,aes(x=period, y = value, group=rlf, fill=rlf)) + 
    geom_col(position = "stack") + 
    facet_grid(region ~ all_enty)
  
  cumExtr_enty_regi <- 
    ggplot(fuExtrCum%>%filter(period>2000,period <= 2100),aes(x=region,y=value,group=rlf,fill=rlf)) +
    geom_col() + 
    facet_grid(all_enty ~ .)
  
  
  
  if(!is.null(file)) {
    write.report(output,file=file,ndigit=7)
  } else {
    return(output)
  }  

}
