#' Read in GDX and calculate LCOE reporting used in convGDX2MIF_LCOE. 
#' 
#' This function provides a post-processing calculation of LCOE (Levelized Cost of Energy) for energy conversion technologies in REMIND. 
#' In incluldes most technologies that generate secondary energy and the distribution technologies which convert secondary energy to final energy. 
#' This script calculates two different types of LCOE: average LCOE (standing system) and marginal LCOE (new plant). 
#' The average LCOE reflect the total cost incurred by the technology deployment in a specific time step divided by its energy output. 
#' The marginal LCOE estimate the per-unit lifetime cost of the output if the model added another capacity of that technology in the respective time step.  
#' 
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param output.type string to determine which output shall be produced. 
#' Can be either "average" (returns only average LCOE),
#' "marginal" (returns only marginal LCOE), "both" (returns marginal and average LCOE) and
#' and "marginal detail" (returns table to trace back how marginal LCOE are calculated). 
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
#' @importFrom zoo na.locf


reportLCOE <- function(gdx, output.type = "both"){
  
 # test whether output.type defined
 if (!output.type %in% c("marginal", "average", "both", "marginal detail")) {
   print("Unknown output type. Please choose either marginal, average, both or marginal detail.")
   return(new.magpie(cells_and_regions = "GLO", 
                     years = c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)))
 }
  
 # check whether key variables are there  
 # LCOE reporting does not make sense for old gdx 
 # where variables are missing and model structure is different
  
 vm_capFac <- readGDX(gdx, "vm_capFac", field = "l", restore_zeros = F)
 qm_balcapture  <- readGDX(gdx,"q_balcapture",field="m", restore_zeros = F)
 vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = F)
 pm_emifac <- readGDX(gdx, "pm_emiFac", field = "l", restore_zeros = F)
 v32_storloss <- readGDX(gdx, "v32_storloss", field = "l")
 
 if (is.null(vm_capFac) | is.null(qm_balcapture) | is.null(vm_co2CCS) | 
     is.null(pm_emifac) | is.null(v32_storloss)) {
   print("The gdx file is too old for generating a LCOE reporting...returning NULL")
   return(new.magpie(cells_and_regions = "GLO", 
                     years = c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)))
 }

 #initialize output array
 LCOE.out <- NULL
 
 ########################################################
 ### A) Calculation of average (standing system) LCOE ###
 ########################################################
 if (output.type %in% c("both", "average")) {

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

 # # sensitivites (only for investment cost sensitivity runs)
 # p_costFac  <- readGDX(gdx,name=c("p_costFac"),react = "silent") # sensitivity factor
 # if (is.null(p_costFac)) {
 #   p_costFac <- new.magpie(getRegions(v_directteinv),getYears(v_directteinv),getNames(p_omeg,dim=2), fill=1)
 # }




 ####### calculate needed parameters ###############

 # calculates annuity cost:
 # annuity cost = 1/ sum_t (p_omeg(t) / 1.06^t)  * direct investment cost
 # t is in T which is the lifetime of the technology
 # direct investment cost = directteinv or for past values (before 2005) (v_investcost * deltaCap)
 # annuity represents (total investment cost + interest over lifetime) distributed equally over all years of lifetime

 # # quick fix for h22ch4 problem
 # if (! "h22ch4" %in% magclass::getNames(p_omeg,dim=2)) {
 # te <- te[te!="h22ch4"]
 # }

 # get a representative region
 reg1 <- getRegions(vm_prodSe)[1]

 te_annuity <- new.magpie("GLO",names=magclass::getNames(p_omeg,dim=2))
 for(a in magclass::getNames(p_omeg[reg1,,],dim=2)){
  te_annuity[,,a] <- 1/dimSums(p_omeg[reg1,,a]/1.06**as.numeric(magclass::getNames(p_omeg[reg1,,a],dim=1)),dim=3.1)
 }

 te_inv_annuity <- 1e+12 * te_annuity[,,te] *
   mbind(
     v_investcost[,ttot_before2005,te] * dimSums(vm_deltaCap[teall2rlf][,ttot_before2005,te],dim=3.2),
     v_directteinv[,ttot_from2005,te]
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
 # only "gridwind" technology active, wind requires 1.5 * the gridwind capacities as spv and csp
 
 grid_factor_tech <- new.magpie(names=te2grid$all_te, fill=1)
 getSets(grid_factor_tech)[3] <- "all_te"
 grid_factor_tech[,,"wind"] <- 1.5

 te_annual_grid_cost <- new.magpie(getRegions(te_inv_annuity),ttot_from2005,magclass::getNames(te_inv_annuity), fill=0)
 vm_VRE_prodSe_grid <- dimSums(grid_factor_tech*vm_prodSe[,,te2grid$all_te])
 
 
 te_annual_grid_cost[,,te2grid$all_te] <- collapseNames(te_annual_inv_cost[,ttot_from2005,"gridwind"] +
                                           te_annual_OMF_cost[,,"gridwind"]) * 
                                           grid_factor_tech * vm_prodSe[,,te2grid$all_te] / 
                                           vm_VRE_prodSe_grid
 
 
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
 LCOE.avg <- NULL

# calculate standing system LCOE
# convert from USD2005/MWh to USD2015/MWh (*1.2)
 LCOE.avg <- mbind(
              setNames(te_annual_inv_cost[,getYears(te_annual_fuel_cost),pe2se$all_te]/
                         total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te,"|supply-side", "|Investment Cost")),
              setNames(te_annual_fuel_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te,"|supply-side", "|Fuel Cost")),
              setNames(te_annual_OMF_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te, "|supply-side","|OMF Cost")),
              setNames(te_annual_OMV_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te, "|supply-side","|OMV Cost")),
              setNames(te_annual_stor_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te, "|supply-side","|Storage Cost")),
              setNames(te_annual_grid_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te, "|supply-side","|Grid Cost")),
              setNames(te_annual_ccsInj_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te, "|supply-side","|CCS Cost")),
              setNames(te_annual_co2_cost[,,pe2se$all_te]/total_te_energy[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te, "|supply-side","|CO2 Cost")),
              setNames(te_curt_cost[,,pe2se$all_te],
                       paste0("LCOE|average|",pe2se$all_enty1,"|",pe2se$all_te, "|supply-side","|Curtailment Cost"))
)*1.2

 # convert to better dimensional format
 df.lcoe.avg <- as.quitte(LCOE.avg) %>% 
                  select(region, period, data, value) %>% 
                  rename(variable = data)
 
 
 # extract further dimensions from variable name
 df.lcoe.avg$type <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
 df.lcoe.avg$type <- sapply(df.lcoe.avg$type, "[[", 2)

 df.lcoe.avg$output <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
 df.lcoe.avg$output <- sapply(df.lcoe.avg$output, "[[", 3)

 df.lcoe.avg$tech <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
 df.lcoe.avg$tech <- sapply(df.lcoe.avg$tech, "[[", 4)
 
 df.lcoe.avg$sector <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
 df.lcoe.avg$sector <- sapply(df.lcoe.avg$sector, "[[", 5)

 df.lcoe.avg$cost <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
 df.lcoe.avg$cost <- sapply(df.lcoe.avg$cost, "[[", 6)
 
 df.lcoe.avg <- df.lcoe.avg %>% 
                  mutate( unit = "US$2015/MWh") %>% 
                  select(region, period, type, output, tech, sector, unit, cost, value)
 
 # reconvert to magpie object
 LCOE.avg.out <- as.magpie(df.lcoe.avg, spatial=1, temporal=2, datacol=9) 
 # bind to output file
 LCOE.out <- mbind(LCOE.out, LCOE.avg.out)
 }

 ##############################################
 
 ########################################################
 ### B) Calculation of marginal (new plant) LCOE ########
 ########################################################
 
 if (output.type %in% c("marginal", "both", "marginal detail")) {

# variable definitions for dplyr operations in the following section
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
  FlexTax <- NULL
  data <- NULL
  sector <- NULL
  NotBuilt <- NULL
  FlexPriceShare <- NULL
  all_in <- NULL
  FEtax <- NULL
  `Flex Tax` <- NULL
  `FE Tax` <- NULL
  
  
  ##############################################
  

  
  ##### Prepare data for LCOE calculation #######
  
  
  ### 1. read variables, parameters sets, mappings needed from gdx
  
  
  ### technologies
  pe2se <- readGDX(gdx,"pe2se") # pe2se technology mappings
  se2se <- readGDX(gdx,"se2se") # hydrogen <--> electricity technologies
  se2fe <- readGDX(gdx,"se2fe") # se2fe technology mappings
  teStor <- readGDX(gdx, "teStor") # storage technologies for VREs
  teGrid <- readGDX(gdx, "teGrid") # grid technologies for VREs
  ccs2te <-  readGDX(gdx, "ccs2te") # ccsinje technology
  teReNoBio <- readGDX(gdx, "teReNoBio") # renewable technologies without biomass
  teCCS <- readGDX(gdx, "teCCS") # ccs technologies
  teReNoBio <- c(teReNoBio) # renewables without biomass
  
  # energy carriers
  entyPe <- readGDX(gdx, "entyPe")
  entySe <- readGDX(gdx, "entySe")
  entyFe <- readGDX(gdx, "entyFe")
  
  
  # get module realization
  module2realisation <- readGDX(gdx, "module2realisation")
  
  # conversion factors
  s_twa2mwh <- readGDX(gdx,c("sm_TWa_2_MWh","s_TWa_2_MWh","s_twa2mwh"),format="first_found")
  
  
  # all technologies to calculate LCOE for
  te_LCOE <- c(pe2se$all_te, se2se$all_te,se2fe$all_te,"dac")
  
  # all technologies to calculate investment and O&M LCOE for (needed for CCS, storage, grid cost)
  te_LCOE_Inv <- c(te_LCOE, as.vector(teStor), as.vector(teGrid), ccs2te$all_te)
 
  # technologies to produce SE
  te_SE_gen <- c(pe2se$all_te, se2se$all_te)
  
  # auxiliary technologies to calculate other cost parts: grid cost, storage cost, carbon capture and storage
  te_aux_tech <- c( teStor, teGrid, ccs2te$all_te)

  # mappings
  se_gen_mapping <- rbind(pe2se, se2se) 
  colnames(se_gen_mapping) <- c("fuel", "output", "tech")
  
  # all energy system technologies mapping
  en2en <- readGDX(gdx, "en2en") %>% 
              filter(all_te %in% te_LCOE)
  colnames(en2en) <- c("fuel", "output", "tech")
  
  
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
    left_join(en2en) %>% 
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
  
  
  # determine worst grade that has non-zero capacities 
  # (this is the grade where we assume the new plant is built as the best grades are built first)
  # if no capcaities at any grade (full potential reached) -> CF of grade 9
  df.ren.atbound  <- df.CapDistr %>% 
                        group_by(region, period, tech) %>% 
                        mutate( NotBuilt = ifelse(all(CapDistr < 1e-6), 1,0)) %>%  
                        ungroup() %>% 
                        mutate( CapDistr = ifelse(NotBuilt == 1 & as.numeric(rlf) == 9 ,
                                                  1e-5, CapDistr )) %>% 
                        filter(CapDistr >= 1e-6, as.numeric(rlf) <= 9) %>%   
                        group_by(region, period, tech) %>% 
                        summarise(last.grade = max(as.numeric(rlf))) %>% 
                        ungroup()
  

  df.CapFac.ren <- as.quitte(pm_dataren) %>% 
    select(region, all_te, rlf, char, value) %>% 
    filter( char %in% c("nur"), all_te %in% teReNoBio) %>%  
    spread( char, value)  %>%
    rename( tech = all_te, CapFac = nur) %>%
    mutate( rlf = as.numeric(rlf)) %>% 
    right_join(df.ren.atbound, by=c("rlf"="last.grade", "region"="region",
                                     "tech"="tech")) %>% 
    select(region, period, tech, CapFac)
  
  
  
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
  
  # read lifetime of tecnology 
  # calculate annuity factor to annuitize CAPEX and OMF (annuity factor labeled "disc.fac")
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
  Pe.Price <- qm_pebal[,ttot_from2005,unique(pe2se$all_enty)] / (qm_budget+1e-10)*1e12/s_twa2mwh*1.2
  # Secondary Energy Electricity Price (for se2se and se2fe conversions), convert from tr USD 2005/TWa to USD2015/MWh
  Se.Seel.Price <- qm_sebal.seel[,,"seel"]/(qm_budget+1e-10)*1e12/s_twa2mwh*1.2
  # Secondary Energy Price (for se2se and se2fe conversions), convert from tr USD 2005/TWa to USD2015/MWh
  Se.Price <- qm_sebal[,,as.vector(entySe)[as.vector(entySe) != "seel"]]/(qm_budget+1e-10)*1e12/s_twa2mwh*1.2 
  
  Fuel.Price <- mbind(Pe.Price,Se.Seel.Price, Se.Price )
  
  # Fuel price
  df.Fuel.Price <- as.quitte(Fuel.Price) %>%  
    select(region, period, all_enty, value) %>% 
    rename(fuel = all_enty, fuel.price = value, opTimeYr = period) %>% 
    # replace zeros by last value of time series (marginals are sometimes zero if there are other constriants)
    mutate( fuel.price = ifelse(fuel.price == 0, NA, fuel.price)) %>% 
    group_by(region, fuel) %>% 
    na.locf() %>% 
    ungroup()
  
  
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
           , tech %in% te_LCOE) %>% 
    select(opTimeYr, tech, pomeg) 
  
  # expanded df.omeg, with additional period dimension combined with opTimeYr dimensions,
  # period will be year of building the plant, 
  # opTimeYr will be year within lifetime of plant (where only a certain fraction, pomeg, of capacity is still standing)
  df.pomeg.expand <- df.pomeg %>% 
    expand(opTimeYr, period = seq(2005,2150,5)) %>%   
    full_join(df.pomeg) %>% 
    mutate( opTimeYr = as.numeric(opTimeYr)) %>% 
    mutate( opTimeYr = ifelse(opTimeYr > 1, opTimeYr + period, period)) %>% 
    left_join(en2en) %>% 
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
  
  
  
  # Note: Discuss whether we still need to discount fuel price/carbon price over time. 
  # Pomeg only refers to capacity depreciation.  
  # Note: Anticipated prices in NDC and BAU runs before 2025 are different. 
  # This calculation only anticipates prices within one scenario. No myopic view included. 
  
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
  
  
  # Note: co2 capture cost calculation needs to be checked
  # set to 0 meanwhile
  df.Co2.Capt.Price <- df.Co2.Capt.Price %>% 
                          mutate( Co2.Capt.Price = 0)
  
  
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
  
  # Note: This is assuming that the CO2 capture to storage share stays constant over time. 
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
  
 #  ### VRE integration cost
 #  
 #  # first calculate LCOE of VREs before storage losses (Investment Cost + OMF cost) 
 #  # (will be done below again, 
 #  # this is only to calculate the integration cost which are added in the end)
 #  # also caculate LCOE (investment and OMF cost) of grid (only gridwind used)
 #  # and storage technologies
 #  df.LCOE.VRE.preloss <-  df.CAPEX %>% 
 #                              left_join(df.OMF) %>%
 #                              left_join(df.CapFac) %>% 
 #                              left_join(df.lifetime) %>%  
 #                              filter( tech %in% c(VRE2teStor[,1],  VRE2teStor[,2], "gridwind")) %>% 
 #                              # conversion from tr USD 2005/TW to USD2015/kW
 #                              mutate(CAPEX = CAPEX *1.2 * 1e3) %>% 
 #                              # calculate annuity factor for investment cost 
 #                              mutate( disc.fac = r * (1+r)^lifetime/(-1+(1+r)^lifetime)) %>%  
 #                              # investment cost LCOE in USD/MWh
 #                              mutate( `Investment Cost` = CAPEX * disc.fac / (CapFac*8760)*1e3) %>% 
 #                              # OMF cost LCOE in USD/MWh
 #                              mutate( `OMF Cost` = CAPEX * OMF / (CapFac*8760)*1e3) %>% 
 #                              # calculate pre-loss VRE LCOE as investment cost + OMF cost LCOE
 #                              mutate( `VRE LCOE preloss` = `Investment Cost` + `OMF Cost`) %>% 
 #                              select(region, period, tech, CapFac, `VRE LCOE preloss`)
 #  
 # 
 #  
 # # calculate marginal storloss:
 # # (Marginal amount of energy that is lost in storage when 
 # # one more MWh of 'usable seel' (='upgraded seel that is balanced by storage') 
 # # is produced. Unit: MWh. Can be larger than 1!)
 #  
 #  # generation share of technology
 #  df.shSeEl <- read.gdx(gdx, "v32_shSeEl", fields = "l") %>% 
 #                  rename(shSeEl = value, tech = all_te, period = ttot, region = all_regi)
 #  # usable (after storage loss) SE per technology
 #  df.usableSeTe <- read.gdx(gdx, "vm_usableSeTe", fields = "l") %>% 
 #                      rename(usableSeTe = value, tech = all_te, period = ttot, region = all_regi) %>% 
 #                      select(-entySe)
 #  # usable (after storage loss) SE
 #  df.usableSe <- read.gdx(gdx, "vm_usableSe", fields = "l") %>% 
 #                    rename(usableSe = value, period = ttot, region = all_regi) %>% 
 #                    select(-entySe)
 #  # storage exponent
 #  df.storexp <- read.gdx(gdx, "p32_storexp") %>%   
 #                  rename(storexp = value, tech = all_te, region = all_regi) 
 #  
 #  
 #  # questions: !!
 #  ## marginal storloss of usable SE partly very high (30 for wind EUR), which unit does this have
 #  # it is a share, right? Or does it have energy units?
 #  
 #  # calculate marginal storloss
 #  df.marg.storloss <- df.LCOE.VRE.preloss %>% 
 #                        filter( tech %in% VRE2teStor[,1]) %>% 
 #                        left_join(df.shSeEl) %>% 
 #                        left_join(df.usableSeTe) %>% 
 #                        left_join(df.usableSe) %>% 
 #                        left_join(df.eff %>% 
 #                                    right_join(VRE2teStor) %>% 
 #                                    select(-teStor) ) %>% 
 #                        left_join(df.storexp) %>% 
 #                        mutate( loss = 1-eff) %>% 
 #                        # formula derived by RP:
 #                        # This formula is an approximation of the marginal change of storage loss 
 #                        # that was calculated outside ReMIND by hand.
 #                        mutate( first.term = (loss * (shSeEl^storexp) + 
 #                                                (usableSeTe * shSeEl^(storexp-1) * 
 #                                                  (1/usableSe - usableSeTe/(usableSe^2)))) /
 #                                              ((1-eff)*shSeEl^storexp)) %>% 
 #                        mutate( second.term = ( loss^2 * shSeEl^storexp * storexp * shSeEl^(storexp-1) *
 #                                                  (1/usableSe - usableSeTe/(usableSe^2))) /
 #                                                    (((1-eff)*shSeEl^storexp)^2)) %>%
 #                        mutate( marg.storloss = first.term + second.term) %>% 
 #                        mutate( marg.storloss = ifelse(loss == 0, 0, marg.storloss)) %>% 
 #                        # convert storage loss from usable SE to total SE level
 #                        mutate(marg.storloss.SE = marg.storloss / (1+marg.storloss)) %>% 
 #                        # calculate marginal LCOE of storloss
 #                        mutate( loss.MLCOE = `VRE LCOE preloss` * marg.storloss.SE /
 #                                  ( 1 - marg.storloss.SE)) %>% 
 #                        # calculate marginal LCOE postloss
 #                        mutate( postloss.LCOE = `VRE LCOE preloss` /
 #                                  ( 1 - marg.storloss.SE)) %>% 
 #                        # calculate postloss - preloss LCOE -> VRE Storage Loss Cost
 #                        mutate( storloss.MLCOE = postloss.LCOE-`VRE LCOE preloss`)
 #  
 #  # VRE grid weight: how much grid capacities the respective VRE technology needs
 #  df.VRE2teGrid <- data.frame(tech = c("spv","csp","wind"), GridReq = c(1,1,1.5))
 #  
 # 
 #  # calculate marginal integration cost (cost of building storage and grid for the marginal VRE unit)
 #  df.IntCost <- df.marg.storloss %>%
 #                  select(region, period, tech, marg.storloss) %>% 
 #                  # join with MCLOE for storage and grid technologies
 #                  # add grid capacity MLCOE
 #                  left_join(df.LCOE.VRE.preloss %>% 
 #                              filter(tech == "gridwind") %>%
 #                              select(-tech) %>% 
 #                              rename(gridCap.MLCOE = `VRE LCOE preloss`)) %>% 
 #                  # add storage capacity MLCOE
 #                  left_join(df.LCOE.VRE.preloss %>%
 #                              filter(tech %in% VRE2teStor[,2]) %>% 
 #                              rename( teStor = tech, StorCap.MLCOE = `VRE LCOE preloss`) %>% 
 #                              left_join(VRE2teStor) ) %>% 
 #                  # add grid requirement factor, current IntC impl.: wind needs 1.5*gridwind capacity than solar 
 #                  left_join(df.VRE2teGrid) %>%
 #                  # add storage efficiencies 
 #                  left_join(df.eff %>% 
 #                              filter(tech %in% VRE2teStor[,2]) %>% 
 #                              rename(teStor = tech)) %>% 
 #                  # calculate marginal grid and storage capacity cost per unit of new VRE
 #                  mutate( MLCOE.grid = marg.storloss / CapFac * GridReq * gridCap.MLCOE,
 #                          MLCOE.stor = marg.storloss * eff / (1-eff) / CapFac * StorCap.MLCOE)
 #                  
 #    
 #  
 #                      
 #                                                
 #  
 #  # o_te_marg_storloss_usable(ttot,regi,teReNoBio)       !!RP: This formula is an approximation of the marginal change of storage loss that was calculated outside ReMIND by hand.
 #  # =  p_aux_loss(teReNoBio) 
 #  # *   (p_aux_shareseel ** p_storexp(regi,teReNoBio)
 #  #      +   (
 #  #        p_aux_usablese_te * p_aux_shareseel ** (p_storexp(regi,teReNoBio) -1)
 #  #        * ( 1 / p_aux_usablese - p_aux_usablese_te/(p_aux_usablese ** 2) )
 #  #      )
 #  # )
 #  # / (1 - p_aux_loss(teReNoBio) * p_aux_shareseel ** p_storexp(regi,teReNoBio) )
 #  # +   ( 
 #  #   p_aux_loss(teReNoBio) ** 2  * p_aux_shareseel ** p_storexp(regi,teReNoBio) * p_storexp(regi,teReNoBio) * p_aux_shareseel ** (p_storexp(regi,teReNoBio) -1)
 #  #   * ( 1 / p_aux_usablese - p_aux_usablese_te/(p_aux_usablese ** 2) )
 #  # )
 #  # / ( 
 #  #   (1 - p_aux_loss(teReNoBio) * p_aux_shareseel ** p_storexp(regi,teReNoBio) ) 
 #  #   ** 2
 #  # );
 #                              
 # 
 #  
 #  #### calculate LCOE
 #  df.LCOE <- df.LCOE %>% 
 #    # investment cost LCOE in USD/MWh
 #    mutate( `Investment Cost` = CAPEX * disc.fac / (CapFac*8760)*1e3) %>% 
 #    # OMF cost LCOE in USD/MWh
 #    mutate( `OMF Cost` = CAPEX * OMF / (CapFac*8760)*1e3) %>% 
 #    mutate( `OMV Cost` = OMV) %>% 
  
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
  
  
  # Note: marginal grid cost calculation needs to be checked, see Robert's old version above,
  # set to 0 meanwhile
  df.gridcost <- df.gridcost %>% 
                    mutate( grid.cost = 0)
  
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
  
    # Note: marginal storage cost calculation needs to be checked, see Robert's old version above,
    # set to 0 meanwhile
    df.VREstorcost <- df.VREstorcost %>% 
                        mutate( VREstor.cost = 0)
  
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
    

    # calculate stored CO2 per output of capture technology (GtC/TWa)
    pm_eff <- mbind(pm_eta_conv, pm_dataeta)
    vm_co2CCS_m <- pm_emifac_cco2/pm_eff[,,getNames(pm_emifac_cco2, dim=3)]*collapseNames(p_share_carbonCapture_stor)
    
      # calculate CCS tax markup following q21_taxrevCCS, convert to USD2015/MWh
    CCStax <- dimReduce(pm_data_omf[,,"ccsinje"]*vm_costTeCapital[,,"ccsinje"]*vm_co2CCS_m^2/pm_dataccs[,,"quan.1"]/sm_ccsinjecrate/s_twa2mwh*1e12*1.2)
                
                              
    df.CCStax <- as.quitte(CCStax) %>% 
                  rename(tech = all_te1, CCStax.cost = value) %>% 
                  select(region, period, tech, CCStax.cost)
    
    # Note: CCS tax still to fix, set temporarily to 0
    df.CCStax <- df.CCStax %>% 
                  mutate( CCStax.cost = 0)

    
    
    ### 18. CO2 Storage Cost
    p_teAnnuity <- readGDX(gdx, "p_teAnnuity", restore_zeros = F)
    # adding annualized capital cost and omf cost of ccsinje technology, 
    # multiply with vm_co2CCS_m (stored CO2 of CCS technology in GtC/TWa(output)), 
    # convert to USD2015/MWh
    CCSCost <- vm_co2CCS_m*dimReduce(vm_costTeCapital[,,"ccsinje"]/vm_capFac[,,"ccsinje"]*(p_teAnnuity[,,"ccsinje"]+pm_data_omf[,,"ccsinje"]))*1.2*1e12/s_twa2mwh
  
    df.CCSCost <- as.quitte(CCSCost) %>% 
                    rename(tech = all_te, CCSCost = value) %>% 
                    select(region, period, tech, CCSCost)
    
    # Note: CCS cost still to fix, set temporarily to 0
    df.CCSCost <- df.CCSCost %>% 
                    mutate( CCSCost = 0)
    
    ### 19. Flexibility Tax
    cm_FlexTax <- readGDX(gdx, "cm_flex_tax")
    v32_flexPriceShare <- readGDX(gdx, "v32_flexPriceShare", field = "l", restore_zeros = F)
    if (is.null(v32_flexPriceShare) | is.null(cm_FlexTax)) {
      v32_flexPriceShare <- vm_costTeCapital
      v32_flexPriceShare[,,] <- 1
    } else {
      if (cm_FlexTax == 0) {
        v32_flexPriceShare <- vm_costTeCapital
        v32_flexPriceShare[,,] <- 1
      }
    }

    df.flexPriceShare <- as.quitte(v32_flexPriceShare) %>% 
                          rename(tech = all_te, FlexPriceShare = value) %>% 
                          select(region, period, tech, FlexPriceShare) 
    
    ### 20. Final Energy Taxes
    
    # read fe to ppfen mapping (stationary)
    # read fe to ue mapping (transport)
    fe2ppfEn <- readGDX(gdx, "fe2ppfEn") %>% 
      rename(output = all_enty) 
    
    fe2ue <- readGDX(gdx, "fe2ue") %>% 
                rename(output = all_enty, all_in = all_enty1) %>% 
                select(output, all_in)

    
    #read FE tax/subsidy levels
    p21_tau_fe_tax_bit_st <- readGDX(gdx, "p21_tau_fe_tax_bit_st")[,ttot_from2005,unique(fe2ppfEn$all_in)]
    p21_tau_fe_sub_bit_st <- readGDX(gdx, "p21_tau_fe_sub_bit_st")[,ttot_from2005,unique(fe2ppfEn$all_in)]
    p21_tau_fe_tax_transport <- readGDX(gdx, "p21_tau_fe_tax_transport")[,ttot_from2005,unique(fe2ue$output)]
    p21_tau_fe_sub_transport <- readGDX(gdx, "p21_tau_fe_sub_transport")[,ttot_from2005,unique(fe2ue$output)]
    
    FE_tax_level <- mbind((p21_tau_fe_tax_bit_st+p21_tau_fe_sub_bit_st),
                          (p21_tau_fe_tax_transport+p21_tau_fe_sub_transport))
    
    df.FEtax <- as.quitte(FE_tax_level) %>%   
                  left_join(fe2ppfEn) %>% 
                  mutate( output = ifelse(is.na(output), all_in, output)) %>% 
                  # add sector column for industry, buildings, transport FE tax
                  mutate(sector = ifelse(substr(as.character(all_in), 
                                                nchar(as.character(all_in)),
                                                nchar(as.character(all_in))) == "i",
                                         "industry", 
                                         ifelse(substr(as.character(all_in), 
                                                       nchar(as.character(all_in)),
                                                       nchar(as.character(all_in))) == "b",
                                                "buildings", "transport"))) %>% 
                  rename( FEtax = value) %>% 
                  # convert from trUSD2005/TWa to USD2015/MWh
                  mutate(FEtax = FEtax * 1.2 / s_twa2mwh * 1e12 ) %>% 
                  select(region, period, output, sector, FEtax)
    
    
    # Note, this should still be checked: the stationary FE tax currently 
    # is on the vm_cesIO (first CES level prodcution factor) and not on FE demand.
    # Should be checked whether there are losses between FE and CES factor. 
    
  ####################### LCOE calculation (New plant/marginal) ########################
  

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
    left_join(df.flexPriceShare) %>%
    left_join(df.FEtax) %>% 
    # filter to only have LCOE technologies
    filter( tech %in% c(te_LCOE)) 
  
  
  # replace NA by 0 in certain columns
  # columns where NA should be replaced by 0
  col.NA.zero <- c("OMF","OMV", "co2.price.weighted.mean", "fuel.price.weighted.mean","co2_dem","emiFac.se2fe","Co2.Capt.Price",
                   "secfuel.prod", "secfuel.price", "grid.cost","VREstor.cost", "CCStax.cost","CCSCost","FEtax")
  df.LCOE[,col.NA.zero][is.na(df.LCOE[,col.NA.zero])] <- 0
  
  # replace NA by 1 in certain columns
  # columns where NA should be replaced by 1
  col.NA.one <- c("FlexPriceShare")
  df.LCOE[,col.NA.one][is.na(df.LCOE[,col.NA.one])] <- 1
  
  # replace NA for sectors by "supply-side" (for all SE generating technologies)
  col.NA.sec <- c("sector")
  df.LCOE[,col.NA.sec][is.na(df.LCOE[,col.NA.sec])] <- "supply-side"
  
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
    mutate( `Flex Tax` = -(1-FlexPriceShare) * `Fuel Cost`) %>% 
    mutate( `FE Tax` = FEtax) %>% 
    mutate( `Total LCOE` = `Investment Cost` + `OMF Cost` + `OMV Cost` + `Fuel Cost` + `CO2 Tax Cost` + 
              `CO2 Provision Cost` + `Second Fuel Cost` + `VRE Storage Cost` + `Grid Cost` + `CCS Tax Cost` + `CCS Cost` + `Flex Tax` + `FE Tax`)
  
    
  ### DAC: calculate Levelized Cost of CO2 from direct air capture
  # DAC energy demand per unit captured CO2 (EJ/GtC)
  p33_dac_fedem <- readGDX(gdx, "p33_dac_fedem", restore_zeros = F)
  LCOD <- new.magpie(getRegions(vm_costTeCapital), getYears(vm_costTeCapital), 
                     c("Investment Cost","OMF Cost","Electricity Cost","Heat Cost","Total LCOE"))
  # capital cost in trUSD2005/GtC -> convert to USD2015/tCO2
  LCOD[,,"Investment Cost"] <- vm_costTeCapital[,,"dac"] * 1.2 / 3.66 /vm_capFac[,,"dac"]*p_teAnnuity[,,"dac"]*1e3
  LCOD[,,"OMF Cost"] <-  pm_data_omf[,,"dac"]*vm_costTeCapital[,,"dac"] * 1.2 / 3.66 /vm_capFac[,,"dac"]*1e3
  # elecitricty cost (convert DAC FE demand to GJ/tCO2 and fuel price to USD/GJ)
  LCOD[,,"Electricity Cost"] <- p33_dac_fedem[,,"feels"] / 3.66 * Fuel.Price[,,"seel"] / 3.66
  # conversion as above, assume for now that heat is always supplied by H2
  LCOD[,,"Heat Cost"] <- p33_dac_fedem[,,"feh2s"] / 3.66 * Fuel.Price[,,"seh2"]  / 3.66
  LCOD[,,"Total LCOE"] <- LCOD[,,"Investment Cost"]+LCOD[,,"OMF Cost"]+LCOD[,,"Electricity Cost"]+LCOD[,,"Heat Cost"]
  
  # add dimensions to fit to other tech LCOE
  LCOD <- LCOD %>% 
            add_dimension(add = "unit", nm = "US$2015/tCO2") %>%   
            add_dimension(add = "tech", nm = "dac") %>%  
            add_dimension(add = "output", nm = "cco2") %>% 
            add_dimension(add = "type", nm = "marginal") %>% 
            add_dimension(add = "sector", dim=3.4, nm = "supply-side")
  
  # transform marginal LCOE to mif output format
    df.LCOE.out <- df.LCOE %>% 
      select(region, period, tech, output, sector, `Investment Cost`, `OMF Cost`, `OMV Cost`, `Fuel Cost` ,
             `CO2 Tax Cost`,`CO2 Provision Cost`,`Second Fuel Cost`,`VRE Storage Cost` ,`Grid Cost`,
             `CCS Tax Cost`, `CCS Cost`,`Flex Tax`,`FE Tax`,`Total LCOE`) %>% 
      gather(cost, value, -region, -period, -tech, -output, -sector) %>% 
      mutate(unit = "US$2015/MWh", type="marginal") %>% 
      select(region, period, type, output, tech, sector, unit, cost, value)
    
    LCOE.mar.out <- as.magpie(df.LCOE.out, spatial = 1, temporal = 2, datacol=9) 
    # add DAC levelized cost
    LCOE.mar.out <- mbind(LCOE.mar.out, LCOD)
    # bind to previous calculations (if there are)
    LCOE.out <- mbind(LCOE.out,LCOE.mar.out)
  
 }
 
 if (output.type %in% c("marginal detail")) {
   return(df.LCOE)
 } else {
   return(LCOE.out)
 }
 
 return(LCOE.out)
}
  
  
  
  
 

