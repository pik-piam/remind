#' Read in GDX and calculate LCOE reporting used in convGDX2MIF_LCOE. 
#' 
#' This function provides a post-processing calculation of LCOE based on REMIND modeling output for 
#' secondary energy generating technologies. It calculates two different types of LCOE: standing system LCOE and new plant LCOE. 
#' While the standing system LCOE reflect the total cost incurred by the technology deployment in a specific time step divided by its output,
#' the new plant LCOE represent the per-unit lifetime cost of a new plant built in that time step. 
#' 
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param extended.output, if TRUE the function returns a dataframe with the detailed calculation of the
#' new plant LCOE, useful to check how new plant LCOE were calculated
#' @return MAgPIE object - LCOE calculated by model post-processing. Two types a) standing system LCOE b) new plant LCOE. 
#' @author Felix Schreyer, Robert Pietzcker, Lavinia Baumstark
#' @seealso \code{\link{convGDX2MIF_LCOE}}
#' @examples
#' 
#' \dontrun{reportLCOE(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie dimSums getRegions getYears getNames setNames clean_magpie dimReduce as.magpie magpie_expand
#' @importFrom dplyr %>% mutate select rename group_by ungroup right_join filter full_join  arrange summarise
#' @importFrom quitte as.quitte overwrite getRegs
#' @importFrom tidyr spread gather expand


reportLCOE <- function(gdx, extended.output = F){

 #  
 #  ########################################################
 #  ### A) Calculation of standing system LCOE ############
 #  ########################################################
 #  
 #  
 #  ####### read in needed data #######################
 #  
 #  ## conversion factors
 #  s_twa2mwh <- readGDX(gdx,c("sm_TWa_2_MWh","s_TWa_2_MWh","s_twa2mwh"),format="first_found")
 #  ## sets
 #  opTimeYr <- readGDX(gdx,"opTimeYr")
 #  ttot     <- as.numeric(readGDX(gdx,"ttot"))   
 #  ttot_before2005 <- paste0("y",ttot[which(ttot <= 2000)])
 #  ttot_from2005 <- paste0("y",ttot[which(ttot >= 2005)])
 #  opTimeYr2te   <- readGDX(gdx,"opTimeYr2te")
 #  temapse  <- readGDX(gdx,"en2se")   
 #  temapall <- readGDX(gdx,c("en2en","temapall"),format="first_found")
 #  teall2rlf <- readGDX(gdx,c("te2rlf","teall2rlf"),format="first_found")
 #  te        <- readGDX(gdx,"te")  
 #  te2stor   <- readGDX(gdx,"VRE2teStor")
 #  te2grid   <- readGDX(gdx,"VRE2teGrid") 
 #  teVRE   <- readGDX(gdx,"teVRE") 
 #  se2fe     <- readGDX(gdx,"se2fe") 
 #  pe2se     <- readGDX(gdx,"pe2se")
 #  teCCS     <- readGDX(gdx,"teCCS")
 #  teNoCCS   <- readGDX(gdx,"teNoCCS")
 #  techp     <- readGDX(gdx,c("teChp","techp"),format="first_found")
 #  teReNoBio <- readGDX(gdx,"teReNoBio")
 #  
 #  # switches
 #  module2realisation <- readGDX(gdx, "module2realisation")
 #  
 #  ## parameter
 #  p_omeg  <- readGDX(gdx,c("pm_omeg","p_omeg"),format="first_found")     
 #  p_omeg  <- p_omeg[opTimeYr2te]
 #  pm_ts   <- readGDX(gdx,"pm_ts")
 #  pm_data <- readGDX(gdx,"pm_data")
 #  pm_emifac <- readGDX(gdx,"pm_emifac", restore_zeros=F) # emission factor per technology
 #  pm_priceCO2 <- readGDX(gdx,"pm_priceCO2") # co2 price
 #  pm_eta_conv <- readGDX(gdx,"pm_eta_conv", restore_zeros=F) # efficiency oftechnologies with time-dependent eta
 #  pm_dataeta <- readGDX(gdx,"pm_dataeta", restore_zeros=F)# efficiency of technologies with time-independent eta
 # 
 # 
 # 
 #  ## variables
 #  v_directteinv <- readGDX(gdx,name=c("v_costInvTeDir","vm_costInvTeDir","v_directteinv"),field="l",format="first_found")[,ttot,]  
 #  vm_capEarlyReti <- readGDX(gdx,name=c("vm_capEarlyReti"),field="l",format="first_found")[,ttot,]  
 #  vm_deltaCap   <- readGDX(gdx,name=c("vm_deltaCap"),field="l",format="first_found")[,ttot,] 
 #  vm_demPe      <- readGDX(gdx,name=c("vm_demPe","v_pedem"),field="l",restore_zeros=FALSE,format="first_found")
 #  vm_prodSe     <- readGDX(gdx,name=c("vm_prodSe"),field="l",restore_zeros=FALSE,format="first_found")
 #  v_investcost  <- readGDX(gdx,name=c("vm_costTeCapital","v_costTeCapital","v_investcost"),field="l",format="first_found")[,ttot,] 
 #  vm_cap        <- readGDX(gdx,name=c("vm_cap"),field="l",format="first_found")  
 #  vm_prodFe     <- readGDX(gdx,name=c("vm_prodFe"),field="l",restore_zeros=FALSE,format="first_found")
 #  v_emiTeDetail <- readGDX(gdx,name=c("vm_emiTeDetail","v_emiTeDetail"),field="l",restore_zeros=FALSE,format="first_found")
 #  
 #  ## equations
 #  qm_pebal  <- readGDX(gdx,name=c("q_balPe"),field="m",format="first_found")
 #  qm_budget <- readGDX(gdx,name=c("qm_budget"),field="m",format="first_found")
 # 
 # 
 #  # module-specific
 #  if (any(module2realisation[,2] == "RLDC")) {
 #    v32_curt <- readGDX(gdx,name=c("v32_curt"),field="l",restore_zeros=FALSE,format="first_found")
 #    } else {
 #    v32_curt <- 0
 #    }
 #  
 #  # # sensitivites (only for investment cost sensitivity runs)
 #  # p_costFac  <- readGDX(gdx,name=c("p_costFac"),react = "silent") # sensitivity factor
 #  # if (is.null(p_costFac)) {
 #  #   p_costFac <- new.magpie(getRegions(v_directteinv),getYears(v_directteinv),getNames(p_omeg,dim=2), fill=1)
 #  # }
 #  
 # 
 # 
 # 
 #  ####### calculate needed parameters ###############
 #  
 #  # calculates annuity cost: 
 #  # annuity cost = 1/ sum_t (p_omeg(t) / 1.06^t)  * direct investment cost 
 #  # t is in T which is the lifetime of the technology
 #  # direct investment cost = directteinv or for past values (before 2005) (v_investcost * deltaCap) 
 #  # annuity represents (total investment cost + interest over lifetime) distributed equally over all years of lifetime
 #  
 #  # quick fix for h22ch4 problem
 #  te <- te[te!="h22ch4"]
 #  
 # 
 #  te_annuity <- new.magpie("GLO",names=magclass::getNames(p_omeg,dim=2))
 #  for(a in magclass::getNames(p_omeg["EUR",,],dim=2)){
 #   te_annuity[,,a] <- 1/dimSums(p_omeg["EUR",,a]/1.06**as.numeric(magclass::getNames(p_omeg["EUR",,a],dim=1)),dim=3.1)    
 #  }
 # 
 #  te_inv_annuity <- 1e+12 * te_annuity[,,te] * 
 #    mbind( 
 #      v_investcost[,ttot_before2005,te] * dimSums(vm_deltaCap[teall2rlf][,ttot_before2005,te],dim=3.2),
 #      v_directteinv[,ttot_from2005,te] 
 #    )
 #  
 #  
 #  ########## calculation of LCOE of standing system #######
 #  ######## (old annuities included) ######################
 # 
 #  
 # #  calculate sub-parts of "COSTS" 
 #  
 #  # LCOE calculation only for pe2se technologies so far!
 #  
 #  ####### 1. sub-part: Investment Cost #################################
 #  
 #  # AnnualInvCost(t) = sum_(td) [annuity(td) * p_pmeg(t-td+1) * deltaT], 
 #  
 #  # where t is the year for which the investment cost are calculatet and
 #  # td is the year where the investment took place
 #  # so: annuity cost incurred by investment 20 years back is weighted with the still available capacities after 20 years
 #  # annuity cost = discounted investment cost spread over lifetime
 #  
 #  
 #  # here: static LCOE (standing system)
 #  # in future posssibly: forward-looking (dynamic) LCOE (see new plant LCOE as approximation below)
 #  # in future posssibly: discounting engogenous from REMIND?
 #  
 #  
 #  # take 74 tech from p_omeg, although in v_direct_in 114 in total
 # te_annual_inv_cost <- new.magpie(getRegions(te_inv_annuity[,ttot,]),getYears(te_inv_annuity[,ttot,]),magclass::getNames(te_inv_annuity[,ttot,]))
 #   # loop over ttot
 #  for(t0 in ttot){
 #    for(a in magclass::getNames(te_inv_annuity) ) {
 #      # all ttot before t0
 #      t_id <- ttot[ttot<=t0] 
 #      # only the time (t_id) within the opTimeYr of a specific technology a
 #      t_id <- t_id[t_id >= (t0 - max(as.numeric(opTimeYr2te$opTimeYr[opTimeYr2te$all_te==a]))+1)]  
 #      p_omeg_h <- new.magpie(getRegions(p_omeg),years=t_id,names=a)
 #      for(t_id0 in t_id){
 #        p_omeg_h[,t_id0,a] <- p_omeg[,,a][,,t0-t_id0 +1]  
 #      }
 #      te_annual_inv_cost[,t0,a] <- dimSums(pm_ts[,t_id,] * te_inv_annuity[,t_id,a] * p_omeg_h[,t_id,a] ,dim=2)  
 #    } # a
 #  }  # t0
 #  ####### 2. sub-part: fuel cost #################################
 # 
 #  # fuel cost = PE price * PE demand of technology 
 # 
 #  te_annual_fuel_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
 #  te_annual_fuel_cost[,,pe2se$all_te] <- setNames(1e+12 * qm_pebal[,ttot_from2005,pe2se$all_enty] / qm_budget[,ttot_from2005,] * 
 #            setNames(vm_demPe[,,pe2se$all_te], pe2se$all_enty), pe2se$all_te)  
 # 
 #  ####### 3. sub-part: OMV cost #################################
 # 
 #  # omv cost (from pm_data) * SE production
 #  
 #  
 #  temapse.names <- apply(temapse, 1, function(x) paste0(x, collapse = '.'))
 #  te_annual_OMV_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
 #  te_annual_OMV_cost[,,temapse$all_te] <- 1e+12 * collapseNames(pm_data[,,"omv"])[,,temapse$all_te] * setNames(vm_prodSe[,,temapse.names],temapse$all_te) 
 # 
 #  ####### 4. sub-part: OMF cost #################################
 #  
 #  # omf cost = omf (from pm_data) * investcost (trUSD/TW) * capacity
 #  # omf in pm_data given as share of investment cost
 #  
 #  
 #  te_annual_OMF_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
 #  
 #  te_annual_OMF_cost[,,getNames(te_inv_annuity)] <- 1e+12 * collapseNames(pm_data[,,"omf"])[,,getNames(te_inv_annuity)] * v_investcost[,ttot_from2005,getNames(te_inv_annuity)] *
 #            dimSums(vm_cap[,ttot_from2005,], dim = 3.2)[,,getNames(te_inv_annuity)]
 # 
 #  ####### 5. sub-part: storage cost (for wind, spv, csp) #################################
 #  
 #  # storage cost = investment cost + omf cost 
 #  # of corresponding storage technology ("storwind", "storspv", "storcsp")
 #  # clarify: before, they used omv cost here, but storage, grid etc. does not have omv...instead, we use omf now!
 #  
 #  te_annual_stor_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
 #  te_annual_stor_cost[,,te2stor$all_te] <- setNames(te_annual_inv_cost[,ttot_from2005,te2stor$teStor] + 
 #                                                    te_annual_OMF_cost[,,te2stor$teStor],te2stor$all_te) 
 #  
 #  ####### 6. sub-part: grid cost  #################################
 #  
 #  # same as for storage cost only with grid technologies: "gridwind", "gridspv", "gridcsp"
 #  
 #  te_annual_grid_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
 #  te_annual_grid_cost[,,te2grid$all_te] <- setNames(te_annual_inv_cost[,ttot_from2005,te2grid$teGrid] + 
 #                                                    te_annual_OMF_cost[,,te2grid$teGrid],te2grid$all_te)
 #  
 #  ####### 7. sub-part: ccs injection cost  #################################
 #  
 #  # same as for storage/grid but with ccs inejection technology 
 #  # distributed to technolgies according to share of total captured co2 of ccs technology
 #  
 #  # (annual ccsinje investment + annual ccsinje omf) * captured co2 (tech) / total captured co2 of all tech
 # 
 # 
 #  te_annual_ccsInj_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,getNames(te_inv_annuity), fill=0)
 # 
 #  # calculate total ccsinjection cost for all techs
 #  total_ccsInj_cost <- dimReduce(te_annual_inv_cost[getRegions(te_annual_OMF_cost),getYears(te_annual_OMF_cost),"ccsinje"] + 
 #                                                      te_annual_OMF_cost[,,"ccsinje"])
 #  # distribute ccs injection cost over techs
 #  te_annual_ccsInj_cost[,,getNames(v_emiTeDetail[,,"cco2"], dim = 3)] <- setNames(total_ccsInj_cost * v_emiTeDetail[,,"cco2"] /
 #                                                                          dimSums( v_emiTeDetail[,,"cco2"], dim = 3), 
 #                                                                          getNames(v_emiTeDetail[,,"cco2"], dim = 3))
 # 
 #  
 #  ####### 8. sub-part: co2 cost  #################################
 #  
 #  # co2 emission cost =  carbon price ($/tC) * emissions (GtC) * 1e9
 #  
 #  te_annual_co2_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,getNames(te_inv_annuity), fill=0)
 # 
 #  
 #  te_annual_co2_cost[,,getNames(v_emiTeDetail[,,"co2"], dim=3)] <- setNames(pm_priceCO2[,getYears(v_emiTeDetail),] * v_emiTeDetail[,,"co2"] * 1e9,
 #                                                                   getNames(v_emiTeDetail[,,"co2"], dim=3))
 # 
 #  
 #  
 #  
 #  ####### 9. sub-part: curtailment cost  #################################
 #  ########### (only relevant for RLDC version!) ##########################
 #  
 #  # note: this step can only be done after all the other parts have already been calcuated!
 #  
 #  # curtailment cost = total  annual cost / (production * (1-curt_share)) - total annual cost / production
 #  # total annual cost = sum of all previous annual cost 
 #  # curt_share = share of curtailed electricity relative to total generated electricity 
 #  
 #  te_curt_cost <- new.magpie(getRegions(te_annual_fuel_cost), getYears(te_annual_fuel_cost), getNames(te_annual_fuel_cost), fill=0)
 #  
 #  # calculate curtailment share of total generation
 #  pe2se.seel <- pe2se[pe2se$all_enty1 == "seel", ] 
 #  curt_share <- v32_curt / dimSums(vm_prodSe[,, pe2se.seel$all_te], dim = 3)
 #  
 #  # calculate total annual cost (without curtailment cost) as sum of previous parts
 #  te_annual_total_cost_noCurt <- new.magpie(getRegions(te_annual_fuel_cost), getYears(te_annual_fuel_cost), getNames(te_annual_fuel_cost))
 # 
 #  te_annual_total_cost_noCurt <- te_annual_inv_cost[,getYears(te_annual_fuel_cost),] +
 #    te_annual_fuel_cost +
 #    te_annual_OMV_cost  +
 #    te_annual_OMF_cost  +
 #    te_annual_stor_cost +
 #    te_annual_grid_cost +
 #    te_annual_ccsInj_cost +
 #    te_annual_co2_cost 
 #  
 #  # SE and FE production in MWh
 #  total_te_energy <- new.magpie(getRegions(vm_prodSe),getYears(vm_prodSe),
 #                                c(magclass::getNames(collapseNames(vm_prodSe[temapse],collapsedim = c(1,2))),
 #                                  magclass::getNames(collapseNames(vm_prodFe[se2fe],collapsedim = c(1,2)))))
 #  total_te_energy[,,temapse$all_te] <- s_twa2mwh * setNames(vm_prodSe[,,temapse.names], temapse$all_te)
 #  total_te_energy[,,se2fe$all_te]   <- s_twa2mwh * vm_prodFe[,,se2fe$all_te]
 #  
 #  # set total energy to NA if it is very small (< 100 MWh/yr),
 #  # this avoids very large or infinite cost parts and weird plots
 #  total_te_energy[total_te_energy < 100] <- NA
 #  
 #  
 #  # calculate curtailment cost
 #  te_curt_cost[,,teVRE] <- te_annual_total_cost_noCurt[,,teVRE] / (total_te_energy[,,teVRE] * (1-curt_share)) -
 #                                           te_annual_total_cost_noCurt[,,teVRE] / total_te_energy[,,teVRE]
 #  
 #                          
 #  
 #  # #####################################################
 #  # ####### calculate "COSTS" ###########################
 #  # #####################################################
 #  
 #  te_annual_total_cost <- te_annual_inv_cost[,getYears(te_annual_fuel_cost),] +
 #    te_annual_fuel_cost +
 #    te_annual_OMV_cost  +
 #    te_annual_OMF_cost[,getYears(te_annual_fuel_cost),]  +
 #    te_annual_stor_cost +
 #    te_annual_grid_cost +
 #    te_annual_co2_cost +
 #    te_annual_ccsInj_cost 
 #  
 #  
 #  ####################################################
 #  ######### calculate LCOE  ##########################
 #  ####################################################

  LCOE.mag <- NULL
  # 
  # # calculate standing system LCOE
  # # convert from USD2005/MWh to USD2015/MWh (*1.2)
  # LCOE.mag <- mbind(
  #               setNames(te_annual_inv_cost[,getYears(te_annual_fuel_cost),pe2se$all_te]/
  #                          total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|Investment Cost (US$2015/MWh)")),
  #               setNames(te_annual_fuel_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|Fuel Cost (US$2015/MWh)")),
  #               setNames(te_annual_OMF_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|OMF Cost (US$2015/MWh)")),
  #               setNames(te_annual_OMV_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|OMV Cost (US$2015/MWh)")),
  #               setNames(te_annual_stor_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|Storage Cost (US$2015/MWh)")),
  #               setNames(te_annual_grid_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|Grid Cost (US$2015/MWh)")),
  #               setNames(te_annual_ccsInj_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|CCS Cost (US$2015/MWh)")),
  #               setNames(te_annual_co2_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te], 
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|CO2 Cost (US$2015/MWh)")),
  #               setNames(te_curt_cost[,,pe2se$all_te],
  #                        paste0("LCOE|StandingSystem|",pe2se$all_te, "|Curtailment Cost (US$2015/MWh)"))
  # )*1.2

  ##############################################
  
  # variable definitions for dplyr operations in next section
  
  region <- NULL
  period <- NULL
  all_te <- NULL
  value <- NULL
  char <- NULL
  maxprod <- NULL
  CapDistr <- NULL
  nur <- NULL
  rlf <- NULL
  CAPEX <- NULL
  prodSE <- NULL
  AtBound <- NULL
  tech <- NULL
  best.rlf <- NULL
  all_enty <- NULL
  pomeg <- NULL
  fuel.price <- NULL
  pomeg.total <- NULL
  eff <- NULL
  OMF <- NULL
  OMV <- NULL
  lifetime <- NULL
  disc.fac <- NULL
  CapFac <- NULL
  fuel.price.weighted.mean <- NULL
  `Investment Cost` <- NULL
  `OMF Cost` <- NULL
  `OMV Cost` <- NULL
  `Fuel Cost` <- NULL
  `Total LCOE` <- NULL
  `cost` <- NULL
  `variable` <- NULL
  fuel <- NULL
  output <- NULL
  opTimeYr <- NULL
  co2.price <- NULL
  emiFac <- NULL
  all_enty1 <- NULL
  input <- NULL
  emiFac.se2fe <- NULL
  co2_dem <- NULL
  CO2StoreShare <- NULL
  all_enty2 <- NULL
  secfuel <- NULL
  secfuel.prod <- NULL
  grid.factor <- NULL
  grid.cost <- NULL
  vm_prodSeVRE <- NULL
  storloss <- NULL
  VREstor.cost <- NULL
  all_te1 <- NULL
  CCStax.cost <- NULL
  co2.price.weighted.mean <- NULL
  secfuel.price <- NULL
  `CO2 Tax Cost` <- NULL
  `CO2 Provision Cost` <- NULL
  `Second Fuel Cost` <- NULL
  `VRE Storage Cost` <- NULL
  `Grid Cost` <- NULL
  `CCS Tax Cost` <- NULL
  `CCS Cost` <- NULL
  type <- NULL
  
  ##############################################
  
  ########################################################
  ### B) Calculation of new plant LCOE ############
  ########################################################
   
  
  ### 1. retrieve general sets, mappings, scalars needed from gdx
  
  
  ### technologies 
  pe2se <- readGDX(gdx,"pe2se")
  se2se <- readGDX(gdx,"se2se") # hydrogen <--> electricity technologies
  se2fe <- readGDX(gdx,"se2fe")
  teStor <- readGDX(gdx, "teStor") # storage technologies for VREs
  teGrid <- readGDX(gdx, "teGrid") # grid technologies for VREs
  ccs2te <-  readGDX(gdx, "ccs2te") # ccsinje technology
  teReNoBio <- readGDX(gdx, "teReNoBio") # renewable technologies without biomass
  teCCS <- readGDX(gdx, "teCCS") # ccs technologies
  
  # get module realization
  module2realisation <- readGDX(gdx, "module2realisation")
  
  # conversion factors
  s_twa2mwh <- readGDX(gdx,c("sm_TWa_2_MWh","s_TWa_2_MWh","s_twa2mwh"),format="first_found")
  
  
  se_gen_mapping <- rbind(pe2se, se2se) 
  colnames(se_gen_mapping) <- c("fuel", "output", "tech")
  # se_gen_mapping <- rbind(se_gen_mapping, 
  #                         c("seh2","segafos", "h22ch4"), c("seh2", "seliqfos", "MeOH")) %>% 
  #   rename( fuel = all_enty, output = all_enty1, tech = all_te)
  
  
  # all technologies to calculate investment and O&M LCOE for
  te_LCOE_Inv <- c(pe2se$all_te, se2se$all_te,
                   as.vector(teStor), as.vector(teGrid), ccs2te$all_te)
  #te_LCOE_Inv <- c(pe2se$all_te, se2se$all_te, se2se_ccu39$all_te, teStor, teGrid, ccs2te$all_te,  "h22ch4", "MeOH")
  # technologies to produce SE
  te_SE_gen <- c(pe2se$all_te, se2se$all_te)
  #te_SE_gen <- c(pe2se$all_te, se2se$all_te,  "h22ch4", "MeOH")
  # auxiliary technologies to calculate other cost parts: grid cost, storage cost, carbon capture and storage
  te_aux_tech <- c( teStor, teGrid, ccs2te$all_te)
  # renewable technologies without biomass
  teReNoBio <- c(teReNoBio)
  # VRE to storage
  VRE2teStor <- readGDX(gdx, "VRE2teStor") %>% 
                  rename(tech = all_te)
  
  
  ### time steps
  ttot     <- as.numeric(readGDX(gdx,"ttot"))   
  ttot_from2005 <- paste0("y",ttot[which(ttot >= 2005)])
  
  ### conversion factors
  s_twa2mwh <- as.vector(readGDX(gdx,c("sm_TWa_2_MWh","s_TWa_2_MWh","s_twa2mwh"),format="first_found"))
  
  ### 2. retrieve investment and O&M cost
  
  # investment cost 
  vm_costTeCapital <- readGDX(gdx, "vm_costTeCapital", field = "l", restore_zeros = F)[,ttot_from2005,te_LCOE_Inv]
  
  df.CAPEX <- as.quitte(vm_costTeCapital) %>%  
    select(region, period, all_te, value) %>% 
    rename(tech = all_te, CAPEX = value) %>% 
    left_join(se_gen_mapping) %>% 
    select(region, period, tech, fuel, output, CAPEX)
  # omf cost 
  pm_data_omf <- readGDX(gdx, "pm_data", restore_zeros = F)[,,"omf"]
  
  df.OMF <- as.quitte(pm_data_omf) %>% 
    select(region, all_te, value) %>% 
    rename(tech = all_te, OMF = value) 
  
  # omv cost 
  pm_data_omv <- readGDX(gdx, "pm_data", restore_zeros = F)[,,"omv"]       
  
  df.OMV <- as.quitte(pm_data_omv) %>% 
    select(region, all_te, value) %>% 
    rename(tech = all_te, OMV = value) 
  
  # 3. retrieve/calculate capacity factors
  
  # capacity factor of non-renewables
  vm_capFac <- readGDX(gdx, "vm_capFac", field="l", restore_zeros = F)[,ttot_from2005,]
  
  
  # calculate renewable capacity factors of new plants
  vm_capDistr <- readGDX(gdx, c("vm_capDistr","v_capDistr"), field = "l", restore_zeros = F)
  pm_dataren <- readGDX(gdx, "pm_dataren", restore_zeros = F)
  
  # RE capacity distribution over grades
  df.CapDistr <- as.quitte(vm_capDistr) %>% 
    select(region, all_te, period,  rlf, value) %>% 
    rename(tech = all_te, CapDistr = value)
  
  # determine whether RE grade is full
  df.ren.atbound  <- as.quitte(pm_dataren) %>% 
    select(region, all_te, rlf, char, value) %>% 
    filter( char %in% c("nur","maxprod"), all_te %in% teReNoBio) %>% 
    spread( char, value)  %>% 
    rename( tech = all_te) %>% 
    right_join(df.CapDistr) %>% 
    # if maxprod NA -> set to 0 as there is no potential
    mutate( maxprod = ifelse(is.na(maxprod), 0, maxprod)) %>% 
    mutate( prodSE = CapDistr * nur) %>% 
    # see if grade is full: if prodSe is above 0.99 of maximum potential -> at bound
    mutate( AtBound = ifelse(prodSE >= maxprod*0.99, 1, 0))
  
  # renewable CapFac = capacity factor of best grade that is not full yet
  # if potential fully deployed -> CapFac = NA -> LCOE = NA
  df.CapFac.ren <-  df.ren.atbound %>% 
    filter( AtBound == 0) %>%  
    group_by(region, period, tech) %>% 
    mutate( best.rlf = min(as.numeric(rlf))) %>%   
    ungroup() %>% 
    filter( rlf == best.rlf) %>% 
    select(region, period, tech, nur) %>% 
    rename( CapFac = nur)
  
  # CapFac, merge renewble and non renewable Cap Facs
  df.CapFac <- as.quitte(vm_capFac) %>% 
    select(region, period, all_te, value) %>% 
    rename(tech = all_te, CapFac = value) %>% 
    filter( ! tech %in% teReNoBio ) %>%
    rbind(df.CapFac.ren) %>% 
    mutate( period = as.numeric(period))
  
  ### 4. plant lifetime and annuity factor
  
  #discount rate
  r <- 0.06
  
  lt <- readGDX(gdx, name="fm_dataglob", restore_zeros = F)[,,"lifetime"][,,te_LCOE_Inv][,,"lifetime"]
  
  df.lifetime <- as.quitte(lt) %>% 
    select(all_te, value) %>% 
    rename(tech = all_te, lifetime = value) %>% 
    mutate( disc.fac = r * (1+r)^lifetime/(-1+(1+r)^lifetime))
  
  ### note: just take LCOE investment cost formula with fix lifetime, ignoring that in REMIND capacity is drepeciating over time 
  ### actually you would need to divide CAPEX by CapFac * 8760 * p_omeg and then add a discounting. 
  ### Would need to discuss again how to add the discount in such formula. 

  ### 6. retrieve fuel price
  
  # retrieve marginal of balance equations
  qm_pebal  <- readGDX(gdx,name=c("q_balPe"),field="m",format="first_found")[,ttot_from2005,]
  qm_sebal  <- readGDX(gdx,name=c("q_balSe"),field="m",format="first_found")[,ttot_from2005,]
  qm_budget <- readGDX(gdx,name=c("qm_budget"),field="m",format="first_found")[,ttot_from2005,]
  qm_sebal.seel <- readGDX(gdx,name="q32_balSe",types="equations",field="m",format="first_found")[,ttot_from2005,]
  
  # retrieve capacity distribution over lifetime (fraction of capacity still standing in that year of plant lifetime)
  p_omeg  <- readGDX(gdx,c("pm_omeg","p_omeg"),format="first_found") 
  
  # Primary Energy Price, convert from tr USD 2005/TWa to USD2015/MWh
  Pe.Price <- qm_pebal[,ttot_from2005,unique(pe2se$all_enty)] / qm_budget*1e12/s_twa2mwh*1.2
  # Secondary Energy Electricity Price (for se2se conversions), convert from tr USD 2005/TWa to USD2015/MWh
  Se.Seel.Price <- qm_sebal.seel[,,"seel"]/(qm_budget+1e-10)*1e12/s_twa2mwh*1.2
  # Secondary Energy Hydrogen Price (for se2se conversions), convert from tr USD 2005/TWa to USD2015/MWh
  Se.H2.Price <- qm_sebal[,,"seh2"]/(qm_budget+1e-10)*1e12/s_twa2mwh*1.2
  
  Fuel.Price <- mbind(Pe.Price,Se.Seel.Price, Se.H2.Price )
  
  # Fuel price
  df.Fuel.Price <- as.quitte(Fuel.Price) %>%  
    select(region, period, all_enty, value) %>% 
    rename(fuel = all_enty, fuel.price = value, opTimeYr = period) 
  
  
  ### 7. retrieve carbon price
  pm_priceCO2 <- readGDX(gdx, "pm_priceCO2", restore_zeros = F)
  
  df.co2price <- as.quitte(pm_priceCO2) %>% 
    select(region, period, value) %>%
    # where carbon price is NA, it is zero
    # convert from USD2005/tC CO2 to USD2015/tCO2
    mutate(value = ifelse(is.na(value), 0, value*1.2/3.66)) %>% 
    rename(co2.price = value, opTimeYr = period)
  
  ### 8. calculate capacity distribution weighted prices over lifetimes of plant
  
  # capacity distribution over lifetime                  
  df.pomeg <- as.quitte(p_omeg) %>% 
    rename(tech = all_te, pomeg = value) %>% 
    # to save run time, only take p_omeg in ten year time steps only up to lifetimes of 50 years, 
    # erase region dimension, pomeg same across all regions, only se generating technologies
    filter(opTimeYr %in% c(1,seq(5,50,5)), region %in% getRegions(vm_costTeCapital)[1]
           , tech %in% te_SE_gen) %>% 
    select(opTimeYr, tech, pomeg) 
  
  # expanded df.omeg, with additional period dimension combined with opTimeYr dimensions,
  # period will be year of building the plant, 
  # opTimeYr will be year within lifetime of plant (where only a certain fraction, pomeg, of capacity is still standing)
  df.pomeg.expand <- df.pomeg %>% 
    expand(opTimeYr, period = seq(2005,2150,5)) %>%   
    full_join(df.pomeg) %>% 
    mutate( opTimeYr = as.numeric(opTimeYr)) %>% 
    mutate( opTimeYr = ifelse(opTimeYr > 1, opTimeYr + period, period)) %>% 
    left_join(se_gen_mapping) %>% 
    arrange(tech, period, opTimeYr)
  
  
  
  ### 9. weight fuel price and carbon price with capacity density over liftime
  
  # join fuel price to df.omeg and weigh fuel price by df.pomeg (pomeg = fraction of capacity still standing in that year of plant lifetime)
  df.fuel.price.weighted <- df.Fuel.Price %>% 
    full_join(df.pomeg.expand) %>% 
    # mean of fuel prices over first 50-years of plant lifetime weighted by pomeg
    group_by(region, period, fuel, tech) %>% 
    summarise( fuel.price.weighted.mean = sum(fuel.price * pomeg / sum(pomeg))) %>% 
    ungroup() 
  
  # join carbon price to df.omeg and weigh it by df.pomeg (pomeg = fraction of capacity still standing in that year of plant lifetime)
  df.co2price.weighted <- df.co2price %>% 
    full_join(df.pomeg.expand) %>%    
    filter( !is.na(tech)) %>%  
    # mean of carbon price over first 50-years of plant lifetime weighted by pomeg
    group_by(region, period, tech) %>% 
    summarise( co2.price.weighted.mean = sum(co2.price * pomeg / sum(pomeg))) %>% 
    ungroup() 
  
  
  
  # note: do we still need to discount the fuel price/carbon price before calculating the time average? This is not included. Pomeg only refers to capacity depreciation.  
  # note: el2VRE does not have p_omeg?
  # note: What about anticipated fuel prices before 2025 in BAU and NDC run...technically we would need to take the weighted average prices
  # of those years from BAU and NDC runs because of myopic decision maker
  
  
  
  ### 10. get fuel conversion efficiencies 
  
  pm_eta_conv <- readGDX(gdx,"pm_eta_conv", restore_zeros=F)[,ttot_from2005,] # efficiency oftechnologies with time-independent eta
  pm_dataeta <- readGDX(gdx,"pm_dataeta", restore_zeros=F)[,ttot_from2005,]# efficiency of technologies with time-dependent eta
  
  df.eff <- as.quitte(mbind(pm_eta_conv, pm_dataeta)) %>% 
    rename(tech = all_te, eff = value) %>% 
    select(region, period, tech, eff) 
  
  ### 11. get emission factors of technologies
  
  pm_emifac <- readGDX(gdx,"pm_emifac", restore_zeros=F)[,ttot_from2005,"co2"] # co2 emission factor per technology
  pm_emifac_cco2 <- readGDX(gdx,"pm_emifac", restore_zeros=F)[,ttot_from2005,"cco2"] # captured co2 emission factor per technology
  
  
  
  
  df.emiFac <- as.quitte(pm_emifac) %>% 
    # do not need period dimension
    filter(period == 2005) %>% 
    select(region, all_te, value) %>% 
    # convert from GtC CO2/TWa to tCo2/MWh
    mutate(value = value / s_twa2mwh * 3.66 *1e9) %>% 
    rename(tech = all_te, emiFac = value) %>% 
    # join other technologies without emission factor -> set emission factor to zero
    # (so far, only emissions of energy input captured, not of co2 input for CCU)
    full_join(data.frame(tech = te_LCOE_Inv) %>%
                expand(tech, region = getRegs(df.CAPEX))) %>%
    mutate( emiFac = ifelse(is.na(emiFac), 0, emiFac))
  
  
  # get se2fe emision factor to add to techs that produce seliqfos, segafos, sesofos
  df.emifac.se2fe <- df.emiFac %>% 
    left_join(se2fe, by=c("tech"="all_te")) %>% 
    # filter for SE fossil, take diesel emifac for all liquids
    filter(all_enty %in% c("seliqfos","segafos","sesofos")) %>% 
    filter(all_enty != "seliqfos" | all_enty1 == "fedie") %>% 
    rename(input = all_enty, emiFac.se2fe = emiFac) %>% 
    select(region, input, emiFac.se2fe) %>% 
    left_join(pe2se, by = c("input" = "all_enty1")) %>% 
    rename(tech = all_te) %>% 
    select(region, tech, emiFac.se2fe)
  
  
  
  
  ### 12. calculate CO2 capture cost
  
  # Co2 Capture price, marginal of q_balcapture,  convert from tr USD 2005/GtC to USD2015/tCO2
  qm_balcapture  <- readGDX(gdx,"q_balcapture",field="m", restore_zeros = F)
  Co2.Capt.Price <- qm_balcapture /
    (qm_budget[,getYears(qm_balcapture),]+1e-10)*1e3*1.2/3.66 ## looks weird
  # # for now, just assume 50USD/tCO2 for all times and regions (half of typical 100USD/tCO2 BECCS cost)
  Co2.Capt.Price[,,] <- 50
  
  df.Co2.Capt.Price <- as.quitte(Co2.Capt.Price) %>% 
    rename(Co2.Capt.Price = value) %>% 
    select(region, period, Co2.Capt.Price) 
  
  
  # CO2 required per unit output (for CCU technologies)
  if( module2realisation$`*`[module2realisation$modules=="CCU"] == "on") {
    p39_co2_dem <- readGDX(gdx, c("p39_co2_dem","p39_ratioCtoH"), restore_zeros = F)[,,]
  } else {
    # some dummy data, only needed to create the following data frame if CCU is off
    p39_co2_dem <- new.magpie(cells_and_regions = "USA", years = "y2005", fill=0) %>% 
                    add_dimension(dim=3.2, add = "all_te", nm = "biochp")
      }

  df.co2_dem <- as.quitte(p39_co2_dem) %>% 
    rename(co2_dem = value, tech = all_te) %>% 
    select(region, period, tech, co2_dem) %>% 
    # from GtC CO2/TWa(H2) to tCO2/MWh(H2)
    mutate( co2_dem = co2_dem * 3.66 / s_twa2mwh * 1e9)
  
  ### 13. calculate share stored carbon from capture carbon
  vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = F)
  vm_co2capture <- readGDX(gdx, "vm_co2capture", field = "l", restore_zeros = F)
  
  p_share_carbonCapture_stor <- (
    vm_co2CCS[,,"cco2.ico2.ccsinje.1"]
    / dimSums(mselect(vm_co2capture, all_enty = "cco2"), dim = 3)
  )
  p_share_carbonCapture_stor[is.na(p_share_carbonCapture_stor)] <- 1
  
  df.CO2StoreShare <- as.quitte(p_share_carbonCapture_stor) %>% 
    rename( CO2StoreShare = value) %>% 
    select(region, period, CO2StoreShare)
  
  # Note: This is assuming that this share stays constant over time. 
  # Still to do: calculate average over lifetime of plant
  
  ### 14. calculate cost of second fuel (if required) 
  # for technologies with coupled production 
  # (pm_prodCouple: negative values mean own consumption, positive values mean coupled product)
  
  # mapping of SE technologies that require second fuel input (own consumption)
  pc2te <- readGDX(gdx, "pc2te") 
  # second fuel production per unit output of technology
  pm_prodCouple <- readGDX(gdx, "pm_prodCouple", restore_zeros = F)
  
  # secfuel.prod (share of coupled production per unit output), 
  # secfuel.price (price of coupled product)
  df.secfuel <- as.quitte(pm_prodCouple) %>% 
    rename(tech = all_te, fuel = all_enty, secfuel = all_enty2, secfuel.prod = value) %>% 
    select(region, tech, fuel, secfuel, secfuel.prod) %>%   
    right_join(df.fuel.price.weighted) %>% 
    rename(secfuel.price = fuel.price.weighted.mean)
  
  
  ### 15. calculate grid cost of VRE technologies
  
  teVRE.grid <- data.frame(tech=c("spv","csp","wind"), gridtech = c("gridwind",
                                                                    "gridwind","gridwind") )
  
  p32_grid_factor <- readGDX(gdx, "p32_grid_factor")
  
  df.gridfactor <- as.quitte(p32_grid_factor) %>% 
                    rename(grid.factor = value) %>% 
                    select(region, grid.factor)
  
  df.gridcost <- df.CAPEX %>% 
                    left_join(df.OMF) %>% 
                    left_join(df.lifetime) %>% 
                    left_join(df.gridfactor) %>% 
                    filter(tech == "gridwind") %>% 
                    rename(gridtech = tech) %>% 
                    mutate(`Investment Cost` = CAPEX *1e12*1.2/s_twa2mwh * disc.fac) %>% 
                    mutate(`OMF Cost` = CAPEX*1e12*1.2/s_twa2mwh * OMF) %>% 
                    mutate(grid.cost = (`Investment Cost` + `OMF Cost`) / grid.factor) %>% 
                    full_join(teVRE.grid) %>%
                    # wind requires 1.5 times the grid capacity than spv and csp
                    mutate(grid.cost = ifelse(tech == "wind", 1.5*grid.cost, grid.cost)) %>% 
                    select(region, period, tech, grid.cost)
  
  
  
  # 16. VRE electricity storage cost
  
  v32_storloss <- readGDX(gdx, "v32_storloss", field = "l")
  # VRE production
  vm_prodSe <- readGDX(gdx, "vm_prodSe", field = "l", restore_zeros = F)[,,c("spv","wind","csp")]
  
  df.prodSeVRE <- as.quitte(vm_prodSe) %>% 
                      rename(tech = all_te, vm_prodSeVRE = value) %>%
                      select(region, period, tech, vm_prodSeVRE)
  
  df.storloss <- as.quitte(v32_storloss) %>% 
                  rename(storloss = value, tech = all_te) %>% 
                  select(region, period, tech, storloss) 
                  
  
  # calculation following q32_limitCapTeStor
  df.VREstorcost <- df.CAPEX %>% 
                      filter(tech %in% VRE2teStor$teStor) %>% 
                      left_join(df.OMF) %>% 
                      left_join(df.lifetime) %>% 
                      rename(teStor = tech) %>% 
                      full_join(VRE2teStor) %>% 
                      left_join(df.storloss) %>% 
                      left_join(df.prodSeVRE) %>% 
                      left_join(df.eff, by=c("region"="region","period"="period","teStor"="tech")) %>% 
                      left_join(df.CapFac, by=c("region"="region","period"="period","teStor"="tech")) %>% 
                      # cost per MWh v32_storloss                  
                      mutate(`Investment Cost` = CAPEX *1e12*1.2/s_twa2mwh * disc.fac) %>% 
                      mutate(`OMF Cost` = CAPEX*1e12*1.2/s_twa2mwh * OMF) %>% 
                      # cost per MWh VRE, 
                      #assuming that for one additional unit of VRE ratio of 
                      #v32_storloss/vm_prodSe constant
                      mutate(VREstor.cost = (`Investment Cost` + `OMF Cost`) / 
                               CapFac / eff * (1-eff) * storloss / vm_prodSeVRE) %>% 
                      mutate( VREstor.cost = ifelse(is.na(VREstor.cost), 0, VREstor.cost)) %>% 
                      select(region, period, tech, VREstor.cost)
  
  
  
    ### 17. CCS tax
    # following q21_taxrevCCS
    vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = F)
    sm_ccsinjecrate <- readGDX(gdx, "sm_ccsinjecrate")
    pm_dataccs <- readGDX(gdx, "pm_dataccs", restore_zeros = F)
    
    
    # calculate storage share of captured CO2,
    # for now take the storage share of the construction year of plant, it will not change much over time 
    # (if CCS, then no CCU and v_capturevalve is mostly small)
    vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = F)
    vm_co2capture <- readGDX(gdx, "vm_co2capture", field = "l", restore_zeros = F)
    
    p_share_carbonCapture_stor <- (
      vm_co2CCS[,,"cco2.ico2.ccsinje.1"]
      / dimSums(mselect(vm_co2capture, all_enty = "cco2"), dim = 3)
    )
    p_share_carbonCapture_stor[is.na(p_share_carbonCapture_stor)] <- 1
    
    # calculate stored CO2 per output of capture technology (GtC/TWa)
    pm_eff <- mbind(pm_eta_conv, pm_dataeta)
    vm_co2CCS_m <- pm_emifac_cco2/pm_eff[,,getNames(pm_emifac_cco2, dim=3)]*collapseNames(p_share_carbonCapture_stor)
    
      # calculate CCS tax markup following q21_taxrevCCS, convert to USD2015/MWh
    CCStax <- dimReduce(pm_data_omf[,,"ccsinje"]*vm_costTeCapital[,,"ccsinje"]*vm_co2CCS_m^2/pm_dataccs[,,"quan.1"]/sm_ccsinjecrate/s_twa2mwh*1e12*1.2)
                
                              
    df.CCStax <- as.quitte(CCStax) %>% 
                  rename(tech = all_te1, CCStax.cost = value) %>% 
                  select(region, period, tech, CCStax.cost)
    
    
    ### CO2 Storage Cost
    p_teAnnuity <- readGDX(gdx, "p_teAnnuity", restore_zeros = F)
    # adding annualized capital cost and omf cost of ccsinje technology, 
    # multiply with vm_co2CCS_m (stored CO2 of CCS technology in GtC/TWa(output)), 
    # convert to USD2015/MWh
    CCSCost <- vm_co2CCS_m*dimReduce(vm_costTeCapital[,,"ccsinje"]/vm_capFac[,,"ccsinje"]*(p_teAnnuity[,,"ccsinje"]+pm_data_omf[,,"ccsinje"]))*1.2*1e12/s_twa2mwh
  
    df.CCSCost <- as.quitte(CCSCost) %>% 
                    rename(tech = all_te, CCSCost = value) %>% 
                    select(region, period, tech, CCSCost)
  
  
  
  ####################### LCOE calculation (New plant/marginal) ########################
  
  
  ### LCOE calculation: Investment cost and O&M cost ###
  
  ### create table with all parmeters needed for LCOE calculation                      
  df.LCOE <- df.CAPEX %>% 
    left_join(df.OMF) %>% 
    left_join(df.OMV) %>% 
    left_join(df.CapFac) %>% 
    left_join(df.lifetime) %>%   
    left_join(df.fuel.price.weighted) %>%  
    left_join(df.co2price.weighted) %>% 
    left_join(df.eff) %>% 
    left_join(df.emiFac) %>% 
    left_join(df.emifac.se2fe) %>% 
    left_join(df.Co2.Capt.Price) %>% 
    left_join(df.co2_dem) %>% 
    left_join(df.CO2StoreShare) %>% 
    left_join(df.secfuel) %>% 
    left_join(df.gridcost) %>% 
    left_join(df.VREstorcost) %>% 
    left_join(df.CCStax) %>% 
    left_join(df.CCSCost) %>% 
    # only LCOE for SE generating technologies for now
    filter( tech %in% c(te_SE_gen)) 
  
  
  # replace NA by 0 in certain columns
  # columns where NA should be replaced by 0
  col.NA.zero <- c("OMF","OMV", "co2.price.weighted.mean", "co2_dem","emiFac.se2fe","Co2.Capt.Price",
                   "secfuel.prod", "secfuel.price", "grid.cost","VREstor.cost", "CCStax.cost","CCSCost")
  df.LCOE[,col.NA.zero][is.na(df.LCOE[,col.NA.zero])] <- 0
  
  #
  
  
  ### data preparation before LCOE calculation
  df.LCOE <- df.LCOE %>% 
    # unit conversions for CAPEX and OMV cost
    # conversion from tr USD 2005/TW to USD2015/kW
    mutate(CAPEX = CAPEX *1.2 * 1e3) %>% 
    # conversion from tr USD 2005/TWa to USD2015/MWh
    mutate(OMV = OMV * 1.2 / s_twa2mwh * 1e12) %>% 
    # share of stored carbon from captured carbon is only relevant for CCS technologies, others -> 1
    mutate( CO2StoreShare = ifelse(tech %in% teCCS, CO2StoreShare, 1)) %>% 
    # calculate discount factor for investment cost 
    mutate( disc.fac = r * (1+r)^lifetime/(-1+(1+r)^lifetime))  
  
  #### calculate LCOE
  df.LCOE <- df.LCOE %>% 
    # investment cost LCOE in USD/MWh
    mutate( `Investment Cost` = CAPEX * disc.fac / (CapFac*8760)*1e3) %>% 
    # OMF cost LCOE in USD/MWh
    mutate( `OMF Cost` = CAPEX * OMF / (CapFac*8760)*1e3) %>% 
    mutate( `OMV Cost` = OMV) %>% 
    mutate( `Fuel Cost` = fuel.price.weighted.mean / eff) %>% 
    mutate( `CO2 Tax Cost` = co2.price.weighted.mean * (emiFac / eff + emiFac.se2fe) * CO2StoreShare) %>% 
    mutate( `CO2 Provision Cost` = Co2.Capt.Price * co2_dem) %>% 
    mutate( `Second Fuel Cost` = -(secfuel.prod * secfuel.price)) %>% 
    mutate( `VRE Storage Cost` = VREstor.cost, `Grid Cost` = grid.cost) %>% 
    mutate( `CCS Tax Cost` = CCStax.cost, `CCS Cost` = CCSCost) %>% 
    mutate( `Total LCOE` = `Investment Cost` + `OMF Cost` + `OMV Cost` + `Fuel Cost` + `CO2 Tax Cost` + 
              `CO2 Provision Cost` + `Second Fuel Cost` + `VRE Storage Cost` + `Grid Cost` + `CCS Tax Cost` + `CCS Cost`)
  
  
  
  # if extended.output = T 
  # -> output is LCOE calculation table, 
  # useful to check LCOE calculation, see calculation details per technology
  if (extended.output) {
    return(df.LCOE)
  # if extended.output = F -> standard mif format LCOE output
  } else {
    # reduce to mif output table
    df.LCOE.out <- df.LCOE %>% 
      select(region, period, tech, output, `Investment Cost`, `OMF Cost`, `OMV Cost`, `Fuel Cost` ,
             `CO2 Tax Cost`,`CO2 Provision Cost`,`Second Fuel Cost`,`VRE Storage Cost` ,`Grid Cost`,
             `CCS Tax Cost`, `CCS Cost`,`Total LCOE`) %>% 
      gather(cost, value, -region, -period, -tech, -output) %>% 
      mutate(unit = "US$2015/MWh", type="New Plant") %>% 
      select(region, period, type, tech, output, unit, cost, value)
    
    LCOE.out <- as.magpie(df.LCOE.out, spatial = 1, temporal = 2, datacol=8)
    
    # bind standing system and new plant LCOE to one magpie object
    LCOE.out <- mbind(LCOE.mag, LCOE.out)
    
    return(LCOE.out)
  }
  
}
  
  
  
  
 

