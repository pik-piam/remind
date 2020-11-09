#' Compute the reporting values of the extraction sector
#' 
#' @param gdx A gdx object
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @description This function returns a magpie object containing the reporting values of the extraction sector. Values include quantities extracted, average and supply costs for coal, oil, gas and uranium.
#' @return A magpie object 
#' @author Jerome Hilaire, Lavinia Baumstark
#' @examples
#' 
#'   \dontrun{
#'     reportExtraction(gdx)
#'   }
#' @importFrom quitte calcCumulatedDiscount
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind dimSums mselect getRegions new.magpie getYears<- getYears setNames getSets getSets<- as.magpie
#' @importFrom dplyr %>% filter_ mutate_
#' @importFrom tidyr extract

reportExtraction <- function(gdx,regionSubsetList=NULL){
  
  ####### conversion factors ##########
  TWa_2_EJ      <- as.numeric(1/readGDX(gdx, c("sm_EJ_2_TWa","sm_ej2twyr"), format="first_found"))
  TDpTWa_2_DpGJ <- as.numeric(1/31.71)   #TerraDollar per TWyear to Dollar per GJ
  uranium_conv  <- 4.43 # tonnes of Uranium to GJ 
  
  ####### read in needed data #########
  ## sets
  petyex       <- readGDX(gdx,c("peEx","petyex"), format="first_found")
  enty2rlf     <- readGDX(gdx,c("pe2rlf","enty2rlf"),format="first_found")
  t            <- as.numeric(readGDX(gdx,"ttot"))
  t            <- t[t>=2005]
  ts           <- readGDX(gdx,"pm_ts")[,t,]
  emi2fuelMine <- readGDX(gdx,c("emi2fuelMine"), format="first_found")
  ## parameters
  grades            <- readGDX(gdx,name=c("p31_grades"), restore_zeros=FALSE, format="first_found", react="silent")
  if(!is.null(grades)) {grades[is.na(grades)] <- 0  }  # substitute na by 0 
  datarog           <- readGDX(gdx,name=c("p31_costExPoly","p31_datarog"), format="first_found")
  datarog2          <- readGDX(gdx,name=c("p31_ffPolyCoeffs"), format="first_found")
  pebiolc_demandmag <- readGDX(gdx,name=c("p30_pebiolc_demandmag"),  format="first_found")
  p_cint            <- readGDX(gdx,name=c("pm_cint","p_cint"), format="first_found", react = "silent")
  fuExtrOwnCons     <- readGDX(gdx,name=c("pm_fuExtrOwnCons"), format="first_found")
  ## variables
  fuelex     <- readGDX(gdx,name=c("vm_fuExtr","vm_fuelex"),      field="l", restore_zeros=FALSE, format="first_found")
  fuelex_cum <- readGDX(gdx,name=c("v31_fuExtrCum","v31_fuelex_cum"), field="l", restore_zeros=FALSE, format="first_found")
  fuelex_cum[is.na(fuelex_cum)] <- 0   # overwrite NA by 0
  costfu_ex  <- readGDX(gdx,name=c("vm_costFuEx","vm_costfu_ex"),   field="l", restore_zeros=FALSE, format="first_found")  
  
  ####### select relevant items #####
  if(!is.null(grades)) {
    mappegrad <- c(dimnames(grades[,,"xi1",drop=TRUE])$all_enty.rlf, "peur.1")
  } else {
    mappegrad <- enty2rlf[enty2rlf$all_enty %in% petyex,]
    mappegrad <- paste(mappegrad$all_enty,mappegrad$rlf,sep=".")
  }  
  
  fuelex_bio <- fuelex[,t,c("pebiolc","pebios","pebioil")]
  fuelex     <- fuelex[,t,mappegrad]
  fuelex_cum <- fuelex_cum[,,mappegrad]
  if(!is.null(grades)) { grades     <- grades[,t,mappegrad[which(mappegrad != "peur.1")]] }
  costfu_ex  <- costfu_ex[,t,petyex]
  pebiolc_demandmag <- pebiolc_demandmag[,t,]
  p_cint          <- collapseNames(mselect(p_cint,all_enty="co2"))
  getSets(p_cint) <- gsub("all_enty1","all_enty",getSets(p_cint))
  p_cint          <- p_cint[,,mappegrad]
  if (!is.null(fuExtrOwnCons)) {
    fuExtrOwnCons <- mselect(fuExtrOwnCons, 
                             all_enty  = gsub("\\.[0-9]","",mappegrad),
                             all_enty1 = gsub("\\.[0-9]","",mappegrad))
  }
  
  ####### internal functions for reporting ###########
  calcAvgSupplyCosts <- function(i_pe, i_fuelex, i_costfuex) {
    
    o_asc <- mselect(i_costfuex, all_enty = i_pe)[,,,drop=TRUE] / 
      dimSums(i_fuelex[,,i_pe], dim=3) * 
      TDpTWa_2_DpGJ * ifelse(i_pe=="peur",1/uranium_conv,1)
    
    return(o_asc)
  }
  
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
      dimSums(tmp_fuelex, dim=3) /                             # average costs of extracting FF in current timestep
      TDpTWa_2_DpGJ                                            # convert from T$/TWa to $/GJ
    o_aec[is.na(o_aec)] <- 0
    # Compute and add global values (as a mean over regions)
    o_aec <- mbind(o_aec, dimSums(o_aec,dim=1)/length(getRegions(tmp_fuelex)))
    
    return(o_aec)
  }
  
  calcAvgExtractionCostsGradeUranium <- function(i_fuelexcum, i_datarog, i_fuelex, TDpTWa_2_DpGJ, uranium_conv=NULL,igrade) {
    # Select regional information because parameters for polynomials are only defined for regions
    # and not at the global level
    tmp_fuelexcum <- i_fuelexcum[which(getRegions(i_fuelexcum) != "GLO"),,]
    tmp_fuelex    <- i_fuelex[which(getRegions(i_fuelex)!= "GLO"),,igrade]
    
    o_aec <- 
      (
        (
          i_datarog %>% mselect(xirog = "xi1", all_enty = igrade) 
          + i_datarog %>% mselect(xirog = "xi2", all_enty = igrade) * tmp_fuelexcum[,,igrade][,,"1"]
          + i_datarog %>% mselect(xirog = "xi3", all_enty = igrade) * tmp_fuelexcum[,,igrade][,,"1"]^2
          + i_datarog %>% mselect(xirog = "xi4", all_enty = igrade) * tmp_fuelexcum[,,igrade][,,"1"]^3
        ) * setNames(tmp_fuelex[,,"1"] / dimSums(tmp_fuelex,dim=3), NULL)
      ) / TDpTWa_2_DpGJ
    o_aec[is.na(o_aec)] <- 0
    # uranium conversion for peur
    if(!is.null(uranium_conv)) { o_aec <- o_aec / uranium_conv }
    # Compute and add global values (as a mean over regions)
    o_aec <- mbind(o_aec, dimSums(o_aec,dim=1)/length(getRegions(tmp_fuelexcum)))
    
    return(o_aec)
  }
  
  calcAvgExtractionCostsGradeFossilFuels <- function(i_fuelexcum, i_datarog, i_fuelex, TDpTWa_2_DpGJ, igrade) {
    # Select regional information because parameters for polynomials are only defined for regions
    # and not at the global level
    tmp_fuelexcum <- i_fuelexcum[which(getRegions(i_fuelexcum) != "GLO"),,]
    tmp_fuelex    <- i_fuelex[which(getRegions(i_fuelex)!= "GLO"),,igrade]
    
    o_aec <- 
      (
        (
          i_datarog %>% mselect(polyCoeffCost = "0", all_enty = igrade) 
          + i_datarog %>% mselect(polyCoeffCost = "1", all_enty = igrade) * tmp_fuelexcum[,,igrade][,,"1"]
          + i_datarog %>% mselect(polyCoeffCost = "2", all_enty = igrade) * tmp_fuelexcum[,,igrade][,,"1"]^2
          + i_datarog %>% mselect(polyCoeffCost = "3", all_enty = igrade) * tmp_fuelexcum[,,igrade][,,"1"]^3
        ) * setNames(tmp_fuelex[,,"1"] / dimSums(tmp_fuelex,dim=3), NULL)
      ) / TDpTWa_2_DpGJ
    o_aec[is.na(o_aec)] <- 0
    
    # Compute and add global values (as a mean over regions)
    o_aec <- mbind(o_aec, dimSums(o_aec,dim=1)/length(getRegions(tmp_fuelexcum)))
    
    return(o_aec)
  }
  
  calcPeProductionNet <- function(i_fuelex, i_cint, i_fuExtrOwnCons, i_pe) {
    
    ownCons <- collapseNames(mselect(fuExtrOwnCons,all_enty=i_pe))
    getSets(ownCons)  <- gsub("all_enty1","all_enty",getSets(ownCons))
    
    tmp <-   ownCons * dimSums(fuelex[,,magclass::getNames(mselect(fuExtrOwnCons,all_enty=i_pe),dim=2)],dim=3.2)
    
    o_ppn <-  dimSums(i_fuelex[,,i_pe] + i_cint[,,i_pe] * i_fuelex[,,i_pe], dim=3) - dimSums(tmp,dim=3 )
    
    return(o_ppn)
  }
  
  
  ####### calculate reporting values ############
  
  # Resource extraction
  tmp1 <- NULL 
  tmp1 <- mbind(
    setNames(dimSums(fuelex[,,"pecoal"] + p_cint[,,"pecoal"] * fuelex[,,"pecoal"], dim=3)*TWa_2_EJ,            "Res|Extraction|Coal (EJ/yr)"),
    setNames(dimSums(fuelex[,,"peoil"]  + p_cint[,,"peoil"]  * fuelex[,,"peoil"], dim=3)*TWa_2_EJ,             "Res|Extraction|Oil (EJ/yr)"),
    setNames(dimSums(fuelex[,,"pegas"]  + p_cint[,,"pegas"]  * fuelex[,,"pegas"], dim=3)*TWa_2_EJ,             "Res|Extraction|Gas (EJ/yr)"),
    setNames(dimSums(fuelex[,,"peur"]   + p_cint[,,"peur"]   * fuelex[,,"peur"], dim=3)*TWa_2_EJ*uranium_conv, "Res|Extraction|Uranium [Energy] (EJ/yr)"),
    setNames(dimSums(fuelex[,,"peur"]   + p_cint[,,"peur"]   * fuelex[,,"peur"], dim=3)*1000,                  "Res|Extraction|Uranium (ktU/yr)"))
  
  # Average extraction costs
  tmp2 <- NULL 
  if(!is.null(grades)) {
    tmp2 <- mbind(  
      setNames(calcAvgExtractionCosts("pecoal", fuelex, fuelex_cum[,c(2000,t),], grades),               "Res|Average Extraction Costs|Coal ($/GJ)"),
      setNames(calcAvgExtractionCosts("peoil",  fuelex, fuelex_cum[,c(2000,t),], grades),               "Res|Average Extraction Costs|Oil ($/GJ)"),
      setNames(calcAvgExtractionCosts("pegas",  fuelex, fuelex_cum[,c(2000,t),], grades),               "Res|Average Extraction Costs|Gas ($/GJ)"),
      setNames(calcAvgExtractionCostsGradeUranium(fuelex_cum[,t,],datarog,fuelex,TDpTWa_2_DpGJ,uranium_conv=uranium_conv,igrade="peur"),"Res|Average Extraction Costs|Uranium ($/GJ)"))  
  } else{
    tmp2 <- mbind(  
      setNames(calcAvgExtractionCostsGradeFossilFuels(fuelex_cum[,t,],datarog2,fuelex,TDpTWa_2_DpGJ,igrade="pecoal"),"Res|Average Extraction Costs|Coal ($/GJ)"),
      setNames(calcAvgExtractionCostsGradeFossilFuels(fuelex_cum[,t,],datarog2,fuelex,TDpTWa_2_DpGJ,igrade="peoil"),"Res|Average Extraction Costs|Oil ($/GJ)"),
      setNames(calcAvgExtractionCostsGradeFossilFuels(fuelex_cum[,t,],datarog2,fuelex,TDpTWa_2_DpGJ,igrade="pegas"),"Res|Average Extraction Costs|Gas ($/GJ)"),
      setNames(calcAvgExtractionCostsGradeUranium(fuelex_cum[,t,],datarog,fuelex,TDpTWa_2_DpGJ,uranium_conv=uranium_conv,igrade="peur"),"Res|Average Extraction Costs|Uranium ($/GJ)"))
  }  
  # Average supply costs
  tmp3 <- NULL 
  tmp3 <- mbind(  
    setNames(calcAvgSupplyCosts("pecoal", fuelex, costfu_ex) * 1000, "Res|Average Supply Costs|Coal ($/GJ)"),
    setNames(calcAvgSupplyCosts("peoil",  fuelex, costfu_ex) * 1000, "Res|Average Supply Costs|Oil ($/GJ)"),
    setNames(calcAvgSupplyCosts("pegas",  fuelex, costfu_ex) * 1000, "Res|Average Supply Costs|Gas ($/GJ)"),
    setNames(calcAvgSupplyCosts("peur",   fuelex, costfu_ex) * 1000, "Res|Average Supply Costs|Uranium ($/GJ)"))
  
  # Biomass prod. (Energy crops)
  tmp4 <- NULL
  tmp4 <- mbind(  
    setNames(dimSums(fuelex_bio[,,"pebiolc.1"], dim=3) * TWa_2_EJ, "Primary Energy Production|Biomass|Energy Crops (EJ/yr)"),
    
    setNames(dimSums(fuelex_bio,                          dim=3) * TWa_2_EJ, "PE|Production|Biomass (EJ/yr)"),
    setNames(dimSums(fuelex_bio[,,"pebiolc"],             dim=3) * TWa_2_EJ, "PE|Production|Biomass|+|Lignocellulosic (EJ/yr)"),
    setNames(dimSums(fuelex_bio[,,"pebiolc.1"],           dim=3) * TWa_2_EJ, "PE|Production|Biomass|Lignocellulosic|+|Energy Crops (EJ/yr)"),
    setNames(dimSums(fuelex_bio[,,"pebiolc.2"],           dim=3) * TWa_2_EJ, "PE|Production|Biomass|Lignocellulosic|+|Residues (EJ/yr)"), 
    setNames(dimSums(fuelex_bio[,,c("pebios","pebioil")], dim=3) * TWa_2_EJ, "PE|Production|Biomass|+|1st Generation (EJ/yr)"),
    setNames(dimSums(fuelex_bio[,,"pebios"],              dim=3) * TWa_2_EJ, "PE|Production|Biomass|1st Generation|+|SugarAndStarch (EJ/yr)"),
    setNames(dimSums(fuelex_bio[,,"pebioil"],             dim=3) * TWa_2_EJ, "PE|Production|Biomass|1st Generation|+|Sunflowers_PalmOil_others (EJ/yr)")
  )
  
  # Biomass prod. (Energy crops - MAgPIE)
  tmp5 <- NULL
  tmp5 <- setNames(pebiolc_demandmag * TWa_2_EJ,
                   "Primary Energy Production|Biomass|Energy Crops MAgPIE (EJ/yr)")
  
  # Cumulative values
  getSets(tmp1)[3] <- "variable"
  tmp6 <- quitte::as.quitte(tmp1)
  mylist <- lapply(levels(tmp6$variable), function(x) {
    calcCumulatedDiscount(data = tmp6 %>% 
                            filter_(~variable == x) ,
                          nameVar = x, 
                          discount = 0.0) %>% 
      mutate_(variable = ~paste0(gsub(" \\(.*\\)", "", x), 
                                 "|Cumulated ", 
                                 gsub("/yr", "",  # remove /yr
                                      substr(x, 
                                             regexec("(\\(.*\\))", x)[[1]][1],
                                             regexec("(\\(.*\\))", x)[[1]][1] + 
                                               attributes(regexec("(\\(.*\\))", x)[[1]])$match.length[1])))) 
  })
  
  tmp6 <- do.call('rbind', mylist)
  tmp6 <- as.magpie(quitte::as.quitte(tmp6))
  magclass::getNames(tmp6) <- paste0(magclass::getNames(tmp6)," (NA)")
  
  # PE|Production based on extraction
  tmp7 <- tmp1
  magclass::getNames(tmp7) <- gsub("Res\\|Extraction","PE\\|Production\\|Gross",magclass::getNames(tmp7))
  tmp8 <- NULL 
  if (!is.null(fuExtrOwnCons)) {
    tmp8 <- mbind(
      setNames(calcPeProductionNet(fuelex,p_cint,fuExtrOwnCons,i_pe="pecoal")* TWa_2_EJ,  "PE|Production|Net|Coal (EJ/yr)"),
      setNames(calcPeProductionNet(fuelex,p_cint,fuExtrOwnCons,i_pe="peoil") * TWa_2_EJ,   "PE|Production|Net|Oil (EJ/yr)"),
      setNames(calcPeProductionNet(fuelex,p_cint,fuExtrOwnCons,i_pe="pegas") * TWa_2_EJ,   "PE|Production|Net|Gas (EJ/yr)"),
      setNames(calcPeProductionNet(fuelex,p_cint,fuExtrOwnCons,i_pe="peur")  * TWa_2_EJ*uranium_conv, "PE|Production|Net|Uranium [Energy] (EJ/yr)"),
      setNames(calcPeProductionNet(fuelex,p_cint,fuExtrOwnCons,i_pe="peur")   * 1000,                  "PE|Production|Net|Uranium (ktU/yr)"))
  }
  
  
  ####### put all together #############
  out <- mbind(tmp1,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8)
  ####### add global value #############
  out <- mbind(out,dimSums(out,dim=1))
  
  out <- mbind(out,tmp2)
  
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))
  
  return(out)
}