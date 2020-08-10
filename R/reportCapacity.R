#' Read in GDX and calculate capacities, used in convGDX2MIF.R for the reporting
#' 
#' Read in capacity information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @return MAgPIE object - contains the capacity variables
#' @author Lavinia Baumstark, Christoph Bertram
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportCapacity(gdx)}
#' @importFrom quitte calcCumulatedDiscount
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames dimSums getSets getSets<- as.magpie
#' @importFrom dplyr %>% filter_ mutate_

reportCapacity <- function(gdx,regionSubsetList=NULL) {
  
  # read sets
  teall2rlf   <- readGDX(gdx,name=c("te2rlf","teall2rlf"),format="first_found")
  possibleRefineries <- c("refped","refdip","refliq")
  refineries <- intersect(teall2rlf[,1],possibleRefineries)
  ttot        <- readGDX(gdx,name="ttot")
  # read variables
  vm_cap      <- readGDX(gdx,name=c("vm_cap"),field="l",format="first_found") * 1000
  vm_deltaCap <- readGDX(gdx,name=c("vm_deltaCap"),field="l",format="first_found") * 1000
  v_earlyreti <- readGDX(gdx,name=c("vm_capEarlyReti","v_capEarlyReti","v_earlyreti"),field="l",format="first_found")
  
  # data preparation
  ttot <- as.numeric(as.vector(ttot))
  vm_cap      <- vm_cap[teall2rlf]
  vm_cap      <- vm_cap[,ttot,]
  vm_deltaCap <- vm_deltaCap[teall2rlf]
  vm_deltaCap <- vm_deltaCap[,ttot,]
  v_earlyreti <-   v_earlyreti[,ttot,] 
  t2005 <- ttot[ttot>2004]
  # build reporting 
  tmp1 <- NULL
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,c("tnrs","fnrs")],dim=3),"Cap|Electricity|Nuclear (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,c("spv","csp")],dim=3),  "Cap|Electricity|Solar (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,"wind"],dim=3),          "Cap|Electricity|Wind (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,"hydro"],dim=3),         "Cap|Electricity|Hydro (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,"dot"],dim=3),           "Cap|Electricity|Oil (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,c("igcc","pc","coalchp","igccc","pco","pcc")],dim=3),"Cap|Electricity|Coal (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,c("ngcc","ngt","gaschp","ngccc")],dim=3),     "Cap|Electricity|Gas (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,c("bioigccc","biochp","bioigcc")],dim=3),            "Cap|Electricity|Biomass (GW)"))
  tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,"geohdr"],dim=3),        "Cap|Electricity|Geothermal (GW)"))
  if ("h2turb" %in% magclass::getNames(vm_cap,dim=1) ) {tmp1 <- mbind(tmp1,setNames(dimSums(vm_cap[,,"h2turb"],dim=3), "Cap|Electricity|Hydrogen (GW)"))  }
  tmp1 <- mbind(tmp1,setNames(dimSums(tmp1,dim=3),                      "Cap|Electricity (GW)"))
  

  tmp <- NULL
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"igccc"],dim=3),               "Cap|Electricity|Coal|IGCC|w/ CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"igcc"],dim=3),                "Cap|Electricity|Coal|IGCC|w/o CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,c("igccc","pco","pcc")],dim=3),"Cap|Electricity|Coal|w/ CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"ngccc"],dim=3),               "Cap|Electricity|Gas|CC|w/ CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"ngcc"],dim=3),                "Cap|Electricity|Gas|CC|w/o CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"ngccc"],dim=3),               "Cap|Electricity|Gas|w/ CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,c("igcc","pc","coalchp")],dim=3),"Cap|Electricity|Coal|w/o CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,c("ngcc","ngt","gaschp")],dim=3),"Cap|Electricity|Gas|w/o CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,c("bioigccc")],dim=3),         "Cap|Electricity|Biomass|w/ CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,c("biochp","bioigcc")],dim=3), "Cap|Electricity|Biomass|w/o CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"dot"],dim=3),           "Cap|Electricity|Oil|w/o CCS (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"spv"],dim=3),                 "Cap|Electricity|Solar|PV (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"csp"],dim=3),                 "Cap|Electricity|Solar|CSP (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"wind"],dim=3),                "Cap|Electricity|Wind|Onshore (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"storspv"],dim=3) * 3,         "Cap|Electricity|Storage|Battery|For PV (GW)"))
  tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"storwind"],dim=3) * 0.3,      "Cap|Electricity|Storage|Battery|For Wind (GW)"))
  
  
  
  # Newly built capacities electricity (Should all go into tmp2, so that this can be used for calculating cumulated values in tmp5 below)
  tmp2 <- NULL
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("ngcc","ngt","gaschp","ngccc")],dim=3),            "New Cap|Electricity|Gas (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("igccc","pco","pcc","igcc","pc","coalchp")],dim=3), "New Cap|Electricity|Coal (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("bioigccc","biochp","bioigcc")],dim=3),             "New Cap|Electricity|Biomass (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("geohdr","hydro","spv","csp","wind")],dim=3),       "New Cap|Electricity|Non-Biomass Renewables (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("tnrs","fnrs")],dim=3),  "New Cap|Electricity|Nuclear (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"dot"],dim=3),                      "New Cap|Electricity|Oil (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(tmp2,dim=3),              "New Cap|Electricity (GW)"))
  
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"geohdr"],dim=3),         "New Cap|Electricity|Geothermal (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"igccc"],dim=3),          "New Cap|Electricity|Coal|IGCC|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"igcc"],dim=3),           "New Cap|Electricity|Coal|IGCC|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"ngccc"],dim=3),          "New Cap|Electricity|Gas|CC|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"ngcc"],dim=3),           "New Cap|Electricity|Gas|CC|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"spv"],dim=3),            "New Cap|Electricity|Solar|PV (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"csp"],dim=3),            "New Cap|Electricity|Solar|CSP (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"wind"],dim=3),           "New Cap|Electricity|Wind (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"hydro"],dim=3),         "New Cap|Electricity|Hydro (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"ngccc"],dim=3),          "New Cap|Electricity|Gas|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("ngcc","ngt","gaschp")],dim=3), "New Cap|Electricity|Gas|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"ngt"],dim=3),            "New Cap|Electricity|Gas|CT (GW)"))
  if ("h2turb" %in% magclass::getNames(vm_cap,dim=1) ) {tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"h2turb"],dim=3),            "New Cap|Electricity|Hydrogen (GW)"))}
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("igccc","pco","pcc")],dim=3),   "New Cap|Electricity|Coal|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("igcc","pc","coalchp")],dim=3), "New Cap|Electricity|Coal|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"bioigccc"],dim=3),               "New Cap|Electricity|Biomass|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("biochp","bioigcc")],dim=3),    "New Cap|Electricity|Biomass|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("spv","csp")],dim=3),           "New Cap|Electricity|Solar (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"wind"],dim=3),                   "New Cap|Electricity|Wind|Onshore (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"storspv"],dim=3) * 3,            "New Cap|Electricity|Storage|Battery|For PV (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"storwind"],dim=3) * 0.3,         "New Cap|Electricity|Storage|Battery|For Wind (GW)"))
  # Newly built capacities hydrogen
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("bioh2c","bioh2")],dim=3),   "New Cap|Hydrogen|Biomass (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"elh2"],dim=3),                "New Cap|Hydrogen|Electricity (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("gash2c","coalh2c","gash2","coalh2")],dim=3), "New Cap|Hydrogen|Fossil (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("bioh2c","bioh2","elh2","gash2c","coalh2c","gash2","coalh2")],dim=3),"New Cap|Hydrogen (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"bioh2c"],dim=3),              "New Cap|Hydrogen|Biomass|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,"bioh2"],dim=3),               "New Cap|Hydrogen|Biomass|w/o CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("gash2c","coalh2c")],dim=3), "New Cap|Hydrogen|Fossil|w/ CCS (GW)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c("gash2","coalh2")],dim=3),   "New Cap|Hydrogen|Fossil|w/o CCS (GW)"))
  # Newly built capacities liquids
  tmp2 <- mbind(tmp2,setNames(dimSums(vm_deltaCap[,,c(refineries,"coalftrec","coalftcrec","bioftrec","bioftcrec","biodiesel","bioeths","bioethl")],dim=3), 
                              "New Cap|Liquids (GW)"))

  # add terms calculated from previously calculated capacity values
  tmp_aux <- NULL
  tmp_aux <- mbind(tmp_aux,setNames(dimSums(tmp[,,c("Cap|Electricity|Storage|Battery|For PV (GW)","Cap|Electricity|Storage|Battery|For Wind (GW)")],dim=3),   "Cap|Electricity|Storage|Battery (GW)"))
  tmp <- mbind(tmp,tmp_aux)
  
  tmp_aux <- NULL
  names_capacities = c("Cap|Electricity|Gas (GW)",
                       "Cap|Electricity|Nuclear (GW)",
                       "Cap|Electricity|Coal (GW)",
                       "Cap|Electricity|Biomass (GW)",
                       "Cap|Electricity|Hydrogen (GW)",
                       "Cap|Electricity|Geothermal (GW)",
                       "Cap|Electricity|Oil (GW)")
  names_capacities = intersect(names_capacities,getNames(tmp1))
  
  tmp_aux <- mbind(tmp_aux,setNames(dimSums(tmp1[,,names_capacities],dim=3) + 0.6 * tmp1[,,"Cap|Electricity|Hydro (GW)"] + tmp[,,"Cap|Electricity|Storage|Battery (GW)"], 
                                    "Cap|Electricity|Estimated firm capacity counting hydro at 0p6 (GW)"))
  tmp1 <- mbind(tmp1,tmp_aux)
  
  
  tmp_aux <- NULL
  tmp_aux <- mbind(tmp_aux,setNames(dimSums(tmp2[,,c("New Cap|Electricity|Storage|Battery|For PV (GW)","New Cap|Electricity|Storage|Battery|For Wind (GW)")],dim=3),   "New Cap|Electricity|Storage|Battery (GW)"))
  tmp2 <- mbind(tmp2,tmp_aux)
  
  # Idle capacities and Total (sum of operating and idle)
  tmp4 <- NULL
  tmp4 <- mbind(tmp4,setNames(dimSums(vm_cap[,,"igcc"],dim=3)*v_earlyreti[,,"igcc"]/(1-v_earlyreti[,,"igcc"])+
                                dimSums(vm_cap[,,"coalchp"],dim=3)*v_earlyreti[,,"coalchp"]/(1-v_earlyreti[,,"coalchp"])+
                                dimSums(vm_cap[,,"pc"],dim=3)*v_earlyreti[,,"pc"]/(1-v_earlyreti[,,"pc"]),
                               "Idle Cap|Electricity|Coal|w/o CCS (GW)")) 
  tmp4 <- mbind(tmp4,setNames(dimSums(vm_cap[,,"ngcc"],dim=3)*v_earlyreti[,,"ngcc"]/(1-v_earlyreti[,,"ngcc"])+
                                dimSums(vm_cap[,,"gaschp"],dim=3)*v_earlyreti[,,"gaschp"]/(1-v_earlyreti[,,"gaschp"])+
                                dimSums(vm_cap[,,"ngt"],dim=3)*v_earlyreti[,,"ngt"]/(1-v_earlyreti[,,"ngt"]),
                              "Idle Cap|Electricity|Gas|w/o CCS (GW)")) 
  tmp4 <- mbind(tmp4,setNames(dimSums(vm_cap[,,"dot"],dim=3)*v_earlyreti[,,"dot"]/(1-v_earlyreti[,,"dot"]),
                              "Idle Cap|Electricity|Oil|w/o CCS (GW)")) 
  tmp4 <- mbind(tmp4,setNames(tmp4[,,"Idle Cap|Electricity|Coal|w/o CCS (GW)"]+tmp[,,"Cap|Electricity|Coal|w/o CCS (GW)"],
                              "Total Cap|Electricity|Coal|w/o CCS (GW)"))
  tmp4 <- mbind(tmp4,setNames(tmp4[,,"Idle Cap|Electricity|Gas|w/o CCS (GW)"]+tmp[,,"Cap|Electricity|Gas|w/o CCS (GW)"],
                            "Total Cap|Electricity|Gas|w/o CCS (GW)"))
  #Cumulate things on extensive time set
  tmp <- mbind(tmp,tmp1,tmp2,tmp4)

  # Cumulative capacities = cumulating new capacities, starting with 0 in 2005
  tmp6 <- tmp2[,t2005,]
  getSets(tmp6)[3] <- "variable"
  tmp6 <- quitte::as.quitte(tmp6)
  mylist <- lapply(levels(tmp6$variable), function(x) {
    calcCumulatedDiscount(data = tmp6 %>%
                          filter_(~variable == x) ,
                          nameVar = x,
                          discount = 0.0) %>%
      mutate_(variable = ~gsub("New",replacement="Cumulative",x))
  })
  
  tmp6 <- do.call('rbind', mylist)
  tmp6 <- as.magpie(quitte::as.quitte(tmp6))
  magclass::getNames(tmp6) <- paste0(magclass::getNames(tmp6)," (GW)")
 
  tmp <- mbind(tmp[,t2005,],tmp6)
  # add global values
  tmp <- mbind(tmp,dimSums(tmp,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    tmp <- mbind(tmp,do.call("mbind",lapply(names(regionSubsetList), function(x) { result <- dimSums(tmp[regionSubsetList[[x]],,],dim=1); getRegions(result) <- x ; return(result) })))
  
  return(tmp)
}
  
