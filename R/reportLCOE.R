#' Read in GDX and LCOE, used in convGDX2MIF.R for the reporting
#' 
#' Read in LCOE information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @return MAgPIE object - contains LCOE cost parts per pe2se technology
#' @author Felix Schreyer, Robert Pietzcker, Lavinia Baumstark
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportLCOE(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie dimSums getRegions getYears getNames setNames clean_magpie dimReduce as.magpie magpie_expand
#' @importFrom dplyr %>% mutate select rename group_by ungroup right_join filter
#' @importFrom quitte as.quitte overwrite
#' @importFrom tidyr spread


reportLCOE <- function(gdx){

  ####### read in needed data #######################
  
  ## conversion factors
  s_twa2mwh <- readGDX(gdx,c("sm_TWa_2_MWh","s_TWa_2_MWh","s_twa2mwh"),format="first_found")
  ## sets
  opTimeYr <- readGDX(gdx,"opTimeYr")
  ttot     <- as.numeric(readGDX(gdx,"ttot"))   
  ttot_before2005 <- paste0("y",ttot[which(ttot <= 2000)])
  ttot_from2005 <- paste0("y",ttot[which(ttot >= 2005)])
  opTimeYr2te   <- readGDX(gdx,"opTimeYr2te")
  temapse  <- readGDX(gdx,"en2se")   
  temapall <- readGDX(gdx,c("en2en","temapall"),format="first_found")
  teall2rlf <- readGDX(gdx,c("te2rlf","teall2rlf"),format="first_found")
  te        <- readGDX(gdx,"te")  
  te2stor   <- readGDX(gdx,"VRE2teStor")
  te2grid   <- readGDX(gdx,"VRE2teGrid") 
  teVRE   <- readGDX(gdx,"teVRE") 
  se2fe     <- readGDX(gdx,"se2fe") 
  pe2se     <- readGDX(gdx,"pe2se")
  teCCS     <- readGDX(gdx,"teCCS")
  teNoCCS   <- readGDX(gdx,"teNoCCS")
  techp     <- readGDX(gdx,c("teChp","techp"),format="first_found")
  teReNoBio <- readGDX(gdx,"teReNoBio")
  
  # switches
  module2realisation <- readGDX(gdx, "module2realisation")
  
  ## parameter
  p_omeg  <- readGDX(gdx,c("pm_omeg","p_omeg"),format="first_found")     
  p_omeg  <- p_omeg[opTimeYr2te]
  pm_ts   <- readGDX(gdx,"pm_ts")
  pm_data <- readGDX(gdx,"pm_data")
  pm_emifac <- readGDX(gdx,"pm_emifac", restore_zeros=F) # emission factor per technology
  pm_priceCO2 <- readGDX(gdx,"pm_priceCO2") # co2 price
  pm_eta_conv <- readGDX(gdx,"pm_eta_conv", restore_zeros=F) # efficiency oftechnologies with time-dependent eta
  pm_dataeta <- readGDX(gdx,"pm_dataeta", restore_zeros=F)# efficiency of technologies with time-independent eta



  ## variables
  v_directteinv <- readGDX(gdx,name=c("v_costInvTeDir","vm_costInvTeDir","v_directteinv"),field="l",format="first_found")[,ttot,]  
  vm_capEarlyReti <- readGDX(gdx,name=c("vm_capEarlyReti"),field="l",format="first_found")[,ttot,]  
  vm_deltaCap   <- readGDX(gdx,name=c("vm_deltaCap"),field="l",format="first_found")[,ttot,] 
  vm_demPe      <- readGDX(gdx,name=c("vm_demPe","v_pedem"),field="l",restore_zeros=FALSE,format="first_found")
  vm_prodSe     <- readGDX(gdx,name=c("vm_prodSe"),field="l",restore_zeros=FALSE,format="first_found")
  v_investcost  <- readGDX(gdx,name=c("vm_costTeCapital","v_costTeCapital","v_investcost"),field="l",format="first_found")[,ttot,] 
  vm_cap        <- readGDX(gdx,name=c("vm_cap"),field="l",format="first_found")  
  vm_prodFe     <- readGDX(gdx,name=c("vm_prodFe"),field="l",restore_zeros=FALSE,format="first_found")
  v_emiTeDetail <- readGDX(gdx,name=c("vm_emiTeDetail","v_emiTeDetail"),field="l",restore_zeros=FALSE,format="first_found")
  
  ## equations
  qm_pebal  <- readGDX(gdx,name=c("q_balPe"),field="m",format="first_found")
  qm_budget <- readGDX(gdx,name=c("qm_budget"),field="m",format="first_found")


  # module-specific
  if (any(module2realisation[,2] == "RLDC")) {
    v32_curt <- readGDX(gdx,name=c("v32_curt"),field="l",restore_zeros=FALSE,format="first_found")
    } else {
    v32_curt <- 0
    }
  
  # sensitivites (only for investment cost sensitivity runs)
  p_costFac  <- readGDX(gdx,name=c("p_costFac"),react = "silent") # sensitivity factor
  if (is.null(p_costFac)) {
    p_costFac <- new.magpie(getRegions(v_directteinv),getYears(v_directteinv),getNames(p_omeg,dim=2), fill=1)
  }
  



  ####### calculate needed parameters ###############
  
  # calculates annuity cost: 
  # annuity cost = 1/ sum_t (p_omeg(t) / 1.06^t)  * direct investment cost 
  # t is in T which is the lifetime of the technology
  # direct investment cost = directteinv or for past values (before 2005) (v_investcost * deltaCap) 
  # annuity represents (total investment cost + interest over lifetime) distributed equally over all years of lifetime
  
  # quick fix for h22ch4 problem
  te <- te[te!="h22ch4"]
  

  te_annuity <- new.magpie("GLO",names=magclass::getNames(p_omeg,dim=2))
  for(a in magclass::getNames(p_omeg["EUR",,],dim=2)){
   te_annuity[,,a] <- 1/dimSums(p_omeg["EUR",,a]/1.06**as.numeric(magclass::getNames(p_omeg["EUR",,a],dim=1)),dim=3.1)    
  }

  te_inv_annuity <- 1e+12 * te_annuity[,,te] * 
    mbind( 
      v_investcost[,ttot_before2005,te] * dimSums(vm_deltaCap[teall2rlf][,ttot_before2005,te],dim=3.2),
      v_directteinv[,ttot_from2005,te] * p_costFac[,,te]
    )
  
  
  ########## calculation of LCOE of standing system #######
  ######## (old annuities included) ######################

  
 #  calculate sub-parts of "COSTS" 
  
  # LCOE calculation only for pe2se technologies so far!
  
  ####### 1. sub-part: Investment Cost #################################
  
  # AnnualInvCost(t) = sum_(td) [annuity(td) * p_pmeg(t-td+1) * deltaT], 
  
  # where t is the year for which the investment cost are calculatet and
  # td is the year where the investment took place
  # so: annuity cost incurred by investment 20 years back is weighted with the still available capacities after 20 years
  # annuity cost = discounted investment cost spread over lifetime
  
  
  # here: static LCOE (standing system)
  # in future posssibly: forward-looking (dynamic) LCOE (see new plant LCOE as approximation below)
  # in future posssibly: discounting engogenous from REMIND?
  
  
  # take 74 tech from p_omeg, although in v_direct_in 114 in total
 te_annual_inv_cost <- new.magpie(getRegions(te_inv_annuity[,ttot,]),getYears(te_inv_annuity[,ttot,]),magclass::getNames(te_inv_annuity[,ttot,]))
   # loop over ttot
  for(t0 in ttot){
    for(a in magclass::getNames(te_inv_annuity) ) {
      # all ttot before t0
      t_id <- ttot[ttot<=t0] 
      # only the time (t_id) within the opTimeYr of a specific technology a
      t_id <- t_id[t_id >= (t0 - max(as.numeric(opTimeYr2te$opTimeYr[opTimeYr2te$all_te==a]))+1)]  
      p_omeg_h <- new.magpie(getRegions(p_omeg),years=t_id,names=a)
      for(t_id0 in t_id){
        p_omeg_h[,t_id0,a] <- p_omeg[,,a][,,t0-t_id0 +1]  
      }
      te_annual_inv_cost[,t0,a] <- dimSums(pm_ts[,t_id,] * te_inv_annuity[,t_id,a] * p_omeg_h[,t_id,a] ,dim=2)  
    } # a
  }  # t0
  ####### 2. sub-part: fuel cost #################################

  # fuel cost = PE price * PE demand of technology 

  te_annual_fuel_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
  te_annual_fuel_cost[,,pe2se$all_te] <- setNames(1e+12 * qm_pebal[,ttot_from2005,pe2se$all_enty] / qm_budget[,ttot_from2005,] * 
            setNames(vm_demPe[,,pe2se$all_te], pe2se$all_enty), pe2se$all_te)  

  ####### 3. sub-part: OMV cost #################################

  # omv cost (from pm_data) * SE production
  
  
  temapse.names <- apply(temapse, 1, function(x) paste0(x, collapse = '.'))
  te_annual_OMV_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
  te_annual_OMV_cost[,,temapse$all_te] <- 1e+12 * collapseNames(pm_data[,,"omv"])[,,temapse$all_te] * setNames(vm_prodSe[,,temapse.names],temapse$all_te) 

  ####### 4. sub-part: OMF cost #################################
  
  # omf cost = omf (from pm_data) * investcost (trUSD/TW) * capacity
  # omf in pm_data given as share of investment cost
  
  
  te_annual_OMF_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
  
  te_annual_OMF_cost[,,getNames(te_inv_annuity)] <- 1e+12 * collapseNames(pm_data[,,"omf"])[,,getNames(te_inv_annuity)] * v_investcost[,ttot_from2005,getNames(te_inv_annuity)] *
            dimSums(vm_cap[,ttot_from2005,], dim = 3.2)[,,getNames(te_inv_annuity)]
 
  ####### 5. sub-part: storage cost (for wind, spv, csp) #################################
  
  # storage cost = investment cost + omf cost 
  # of corresponding storage technology ("storwind", "storspv", "storcsp")
  # clarify: before, they used omv cost here, but storage, grid etc. does not have omv...instead, we use omf now!
  
  te_annual_stor_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
  te_annual_stor_cost[,,te2stor$all_te] <- setNames(te_annual_inv_cost[,ttot_from2005,te2stor$teStor] + 
                                                    te_annual_OMF_cost[,,te2stor$teStor],te2stor$all_te) 
  
  ####### 6. sub-part: grid cost  #################################
  
  # same as for storage cost only with grid technologies: "gridwind", "gridspv", "gridcsp"
  
  te_annual_grid_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
  te_annual_grid_cost[,,te2grid$all_te] <- setNames(te_annual_inv_cost[,ttot_from2005,te2grid$teGrid] + 
                                                    te_annual_OMF_cost[,,te2grid$teGrid],te2grid$all_te)
  
  ####### 7. sub-part: ccs injection cost  #################################
  
  # same as for storage/grid but with ccs inejection technology 
  # distributed to technolgies according to share of total captured co2 of ccs technology
  
  # (annual ccsinje investment + annual ccsinje omf) * captured co2 (tech) / total captured co2 of all tech


  te_annual_ccsInj_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,getNames(te_inv_annuity), fill=0)
 
  # calculate total ccsinjection cost for all techs
  total_ccsInj_cost <- dimReduce(te_annual_inv_cost[getRegions(te_annual_OMF_cost),getYears(te_annual_OMF_cost),"ccsinje"] + 
                                                      te_annual_OMF_cost[,,"ccsinje"])
  # distribute ccs injection cost over techs
  te_annual_ccsInj_cost[,,getNames(v_emiTeDetail[,,"cco2"], dim = 3)] <- setNames(total_ccsInj_cost * v_emiTeDetail[,,"cco2"] /
                                                                          dimSums( v_emiTeDetail[,,"cco2"], dim = 3), 
                                                                          getNames(v_emiTeDetail[,,"cco2"], dim = 3))

  
  ####### 8. sub-part: co2 cost  #################################
  
  # co2 emission cost =  carbon price ($/tC) * emissions (GtC) * 1e9
  
  te_annual_co2_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,getNames(te_inv_annuity), fill=0)

  
  te_annual_co2_cost[,,getNames(v_emiTeDetail[,,"co2"], dim=3)] <- setNames(pm_priceCO2[,getYears(v_emiTeDetail),] * v_emiTeDetail[,,"co2"] * 1e9,
                                                                   getNames(v_emiTeDetail[,,"co2"], dim=3))

  
  
  
  ####### 9. sub-part: curtailment cost  #################################
  ########### (only relevant for RLDC version!) ##########################
  
  # note: this step can only be done after all the other parts have already been calcuated!
  
  # curtailment cost = total  annual cost / (production * (1-curt_share)) - total annual cost / production
  # total annual cost = sum of all previous annual cost 
  # curt_share = share of curtailed electricity relative to total generated electricity 
  
  te_curt_cost <- new.magpie(getRegions(te_annual_fuel_cost), getYears(te_annual_fuel_cost), getNames(te_annual_fuel_cost), fill=0)
  
  # calculate curtailment share of total generation
  pe2se.seel <- pe2se[pe2se$all_enty1 == "seel", ] 
  curt_share <- v32_curt / dimSums(vm_prodSe[,, pe2se.seel$all_te], dim = 3)
  
  # calculate total annual cost (without curtailment cost) as sum of previous parts
  te_annual_total_cost_noCurt <- new.magpie(getRegions(te_annual_fuel_cost), getYears(te_annual_fuel_cost), getNames(te_annual_fuel_cost))

  te_annual_total_cost_noCurt <- te_annual_inv_cost[,getYears(te_annual_fuel_cost),] +
    te_annual_fuel_cost +
    te_annual_OMV_cost  +
    te_annual_OMF_cost  +
    te_annual_stor_cost +
    te_annual_grid_cost +
    te_annual_ccsInj_cost +
    te_annual_co2_cost 
  
  # SE and FE production in MWh
  total_te_energy <- new.magpie(getRegions(vm_prodSe),getYears(vm_prodSe),
                                c(magclass::getNames(collapseNames(vm_prodSe[temapse],collapsedim = c(1,2))),
                                  magclass::getNames(collapseNames(vm_prodFe[se2fe],collapsedim = c(1,2)))))
  total_te_energy[,,temapse$all_te] <- s_twa2mwh * setNames(vm_prodSe[,,temapse.names], temapse$all_te)
  total_te_energy[,,se2fe$all_te]   <- s_twa2mwh * vm_prodFe[,,se2fe$all_te]
  
  # set total energy to NA if it is very small (< 100 MWh/yr),
  # this avoids very large or infinite cost parts and weird plots
  total_te_energy[total_te_energy < 100] <- NA
  
  
  # calculate curtailment cost
  te_curt_cost[,,teVRE] <- te_annual_total_cost_noCurt[,,teVRE] / (total_te_energy[,,teVRE] * (1-curt_share)) -
                                           te_annual_total_cost_noCurt[,,teVRE] / total_te_energy[,,teVRE]
  
                          
  
  # #####################################################
  # ####### calculate "COSTS" ###########################
  # #####################################################
  
  te_annual_total_cost <- te_annual_inv_cost[,getYears(te_annual_fuel_cost),] +
    te_annual_fuel_cost +
    te_annual_OMV_cost  +
    te_annual_OMF_cost[,getYears(te_annual_fuel_cost),]  +
    te_annual_stor_cost +
    te_annual_grid_cost +
    te_annual_co2_cost +
    te_annual_ccsInj_cost 
  
  
  ####################################################
  ######### calculate LCOE  ##########################
  ####################################################

  LCOE.mag <- NULL
  
  LCOE.mag <- mbind(
                setNames(te_annual_inv_cost[,getYears(te_annual_fuel_cost),pe2se$all_te]/
                           total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|Investment Cost (US$2005/MWh)")),
                setNames(te_annual_fuel_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|Fuel Cost (US$2005/MWh)")),
                setNames(te_annual_OMF_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|OMF Cost (US$2005/MWh)")),
                setNames(te_annual_OMV_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|OMV Cost (US$2005/MWh)")),
                setNames(te_annual_stor_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|Storage Cost (US$2005/MWh)")),
                setNames(te_annual_grid_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|Grid Cost (US$2005/MWh)")),
                setNames(te_annual_ccsInj_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|CCS Cost (US$2005/MWh)")),
                setNames(te_annual_co2_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|CO2 Cost (US$2005/MWh)")),
                setNames(te_curt_cost[,,pe2se$all_te],
                         paste0("LCOE|StandingSystem|",pe2se$all_te, "|Curtailment Cost (US$2005/MWh)"))
  )
  
  # # check whether sum over all cost (without curtailment cost) equals total lcoe above
  # CheckSum <- dimSums(LCOE.mag, , dim = 3.2) - LCOE.mag[,,"Curtailment Cost (US$2005/MWh)"]
  # 
  # total_lcoe_se["USA",,c("spv")]
  # CheckSum["USA",,c("spv")]
  # # correct!
  
  ################ calculation of LCOE of new plants #########################################
  
  
  ### define variables formally needed for dplyr operations
  region <- NULL
  rlf <- NULL
  all_te <- NULL
  value <- NULL
  max.rlf <- NULL
  CapFac <- NULL
  period <- NULL
  best.rlf <- NULL
  Year <- NULL
  Region <- NULL
  Data2 <- NULL
  Data1 <- NULL
  Cell <- NULL
  Type <- NULL
  Value <- NULL
  Maxprod <- NULL
  Geopot <- NULL
  
  # RE techs
  teRe2rlfDetail <- c("spv","csp","hydro", "geohe","geohdr","wind")
  
  # discount rate
  r <- 0.06
  # lifetime
  lt <- readGDX(gdx, name="fm_dataglob", restore_zeros = F)[,,"lifetime"][,,pe2se$all_te]
  
  # discout factor
  disc <- collapseNames(r * (1+r)^lt/(-1+(1+r)^lt))
  
  # capacity factor
  # load capacity factor
  vm_capFac <- readGDX(gdx, name="vm_capFac", field="l", restore_zeros = F)[,,pe2se$all_te]
  pm_dataren <- readGDX(gdx, name="pm_dataren", restore_zeros = F)[,,"nur"][,,pe2se$all_te]
  vm_capDistr <- readGDX(gdx, name="vm_capDistr", field="l",restore_zeros = F)
  
  # load marginal of RE cosntraints (maxprod constraint and area constraint)
  q_limitGeopot <- readGDX(gdx, "q_limitGeopot", field="m")[,ttot_from2005,"pesol"]
  q_limitProd <- readGDX(gdx, "q_limitProd", field="m")[,ttot_from2005,teRe2rlfDetail]
  
  
  # create dataframe ith marginal of maxprod and area constraint
  df.RE.limits <- as.data.frame(q_limitGeopot) %>% 
                    mutate( Type = "Geopot", Data1 = "spv") %>% 
                    rbind(as.data.frame(q_limitProd) %>%  mutate(Type = "Maxprod")) %>% 
                    rename( all_te = Data1, rlf = Data2, region = Region, period = Year) %>% 
                    select(-Cell) %>% 
                    spread(Type, Value)
  

  
  # calculate capacity factor of new capacity added in a specific region, period, tech
  # take capacity factor of best grade that is still not full (i.e. where marginals of area and maxprod constraint are 0)
  df.CapFac <- as.quitte(pm_dataren) %>% 
               select(region, rlf, all_te, value) %>% 
               right_join(df.RE.limits)  %>% 
              # marginals must be zero, or area marginal NA because area constraint only for spv
               filter( Maxprod == 0 & (Geopot == 0 | is.na(Geopot))) %>%     
               group_by(region, period, all_te) %>% 
               mutate( best.rlf = min(as.numeric(rlf))) %>%   
               ungroup() %>%
               # filter for capacity factor of best, not fully deployed grade
               filter( rlf == best.rlf) %>% 
               select( region, period, all_te, value)
  
  CapFac.ren <- as.magpie(df.CapFac, spatial = 1, temporal = 2, datacol=4)
  
  CapFac <- vm_capFac[,ttot_from2005,]
  CapFac[,,getNames(CapFac.ren)] <- CapFac.ren[,ttot_from2005,]
  
  # investment cost
  CAPEX <- v_investcost[,ttot_from2005,pe2se$all_te]
  
  # OMF cost
  OMF <- pm_data[,,"omf"][,,pe2se$all_te]
  
  # OMV Cost 
  OMV <- collapseNames(pm_data[,,"omv"][,,pe2se$all_te])
  
  # SE generation
  #Gen <- setNames(vm_prodSe[,,paste0(pe2se$all_enty,".",pe2se$all_enty1,".", pe2se$all_te)],pe2se$all_te)
  
  LCOE.new <- NULL
  LCOE.new <- mbind(
                setNames(CAPEX * disc/(CapFac*8760)*1e6, 
                         paste0("LCOE|NewPlant|",pe2se$all_te, "|Investment Cost (US$2005/MWh)")),
                setNames(CAPEX * OMF /(CapFac*8760)*1e6, 
                         paste0("LCOE|NewPlant|",pe2se$all_te, "|OMF Cost (US$2005/MWh)")),
                setNames(magpie_expand(OMV * 1e3, CAPEX) , 
                         paste0("LCOE|NewPlant|",pe2se$all_te, "|OMV Cost (US$2005/MWh)")))
  
  
 # bind LCOE of standing system and LCOE of new plants together in one magpie object
 LCOE.out <- mbind(LCOE.mag, LCOE.new)

  
  return(LCOE.out)
}

