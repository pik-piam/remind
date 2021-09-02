#' Computes the employment values (jobs) across different sectors 
#' @param gdx A gdx file output from a REMIND run
#' @param type either "created" or "per_year". The former outputs jobs created in that year whereas the latter outputs total jobs in that year
#' @param improvements Either "None", "CEEW", "Dias", "Rutovitz_aus","Solar_found" or "All". Use "All" for all improvements.
#' @param subtype Subtype for how shares of solar rooftop, wind offshore, and small hydro are assumed in the future. Options "current", "irena", and "expert". See calcDspvShare for more information.
#' @param share Either "current" or "local". Current implies current shares of world manufacture remain same until 2050, current means that in 2050 all countries manufacture required components locally.
#' @param multiplier controls how the regional multiplier for non-oecd countries changes with time.
#' @param decline How should the employment factors change over time? "capcosts" means according to capital costs. "static" means it doesn't change
#' @description This function returns a magpie object containing the reporting the jobs for different technologies
#' @return A magpie object 
#' @author Aman Malik
#' @examples
#' 
#'   \dontrun{
#'     reportEmployment(gdx,improvements="All",multiplier="own",subtype="expert",share="local",decline="capcosts")
#'   }
#' @importFrom madrat calcOutput 
#' @importFrom magclass getNames add_columns getItems
#' @export

reportEmployment <- function(gdx,improvements,multiplier,subtype,share,decline,type){
  
  # employment factors
  x <- madrat::calcOutput("Employmentfactors",improvements=improvements,multiplier=multiplier)
  x <- x[,,"HP",pmatch=T,invert=T] # excluding (at the moment) jobs in combined heat and power plants
  
  ## DECLINE FACTORS------------------------
  if (decline=="capcosts"){
    # capital costs and OM fixed costs evolution over time for different techs, used to calculated the decline factor
    report_tech <- reportTechnology(gdx)
    report_tech <- collapseNames(report_tech)
    
    var <- c("Tech|Electricity|Coal|PC|w/o CCS|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Gas|CC|w/o CCS|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Hydro|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Nuclear|Capital Costs (US$2005/kW)" ,
             "Tech|Electricity|Solar|PV|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Solar|CSP|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Wind|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Biomass|IGCC|w/o CCS|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Storage|Battery|For PV|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Oil|DOT|Capital Costs (US$2005/kW)",
             "Tech|Electricity|Geothermal|Capital Costs (US$2005/kW)",
             
             "Tech|Electricity|Coal|PC|w/o CCS|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Gas|CC|w/o CCS|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Hydro|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Nuclear|OM Cost|fixed (US$2005/kW/yr)" ,
             "Tech|Electricity|Solar|PV|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Solar|CSP|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Wind|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Biomass|IGCC|w/o CCS|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Oil|DOT|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Geothermal|OM Cost|fixed (US$2005/kW/yr)",
             "Tech|Electricity|Storage|Battery|For PV|OM Cost|fixed (US$2005/kW/yr)",NULL
    )
    # only capital costs
    cap_costs <- report_tech[,seq(2015,2050,5),grep(x = var,pattern = "Capital",value = T)]
    cap_costs <- cap_costs["GLO",,invert=T]# removing GLO
    
    cap_costs <- cap_costs[,,]/setYears(cap_costs[,"y2020",],NULL)# costs relative to 2015
    
    # the job intensity decreases with capital costs for techs, relative to 2015
    var_in_x <- getItems(x,dim=3.1) # variables from EF magpie object
    var_in_rem <- c("Coal","Gas","Nuclear","Biomass","Hydro","Hydro","Wind","Wind","Solar|PV",
                    "Geothermal","Solar|CSP","Oil","Solar|PV","Storage") # variables from remind
    var_com <- paste(var_in_x,var_in_rem,sep = ".") # concatenating the two for looping to work
    
    for (i in var_com){
      x[,,sub("\\..*", "", i)][,,c("CI","Manf")] <- x[,,sub("\\..*", "", i)][,,c("CI","Manf")]*setNames(cap_costs[,getYears(x),sub(".*\\.", "", i),pmatch=T],NULL)
    }
    # OM fixed costs used to calculate  decline factors for O&M employment factors
    fixed_costs <- report_tech[,seq(2015,2050,5),grep(x = var,pattern = "fixed",value = T)]
    fixed_costs <- fixed_costs["GLO",,invert=T]# removing GLO
    
    fixed_costs <- fixed_costs[,,]/setYears(fixed_costs[,"y2020",],NULL)# costs relative to 2015
    for (i in var_com){
      x[,,sub("\\..*", "", i)][,,"OM"] <- x[,,sub("\\..*", "", i)][,,"OM"]*setNames(fixed_costs[,getYears(x),sub(".*\\.", "", i),pmatch=T],NULL)
    }
    
    
    
    # decline factors for coal fuel supply until 2050 are projected depending on 
    # historical patterns and convergence between some countries. 
    # See calcLabourProductivity for more information.
    coal_ef <- calcOutput("CoalLabourProductivity",subtype = "Employment_factor")
    coal_ef <- coal_ef/setYears(coal_ef[,2020,],NULL)
    coal_ef[which(coal_ef=="NaN")] <- 0
    x[,getYears(x),"Coal.Fuel_supply"] <- x[,getYears(x),"Coal.Fuel_supply"]*setNames(coal_ef[,getYears(x),],NULL)
  }
  # if (decline=="capcosts_50"){
  #   report_tech <- reportTechnology(gdx)
  #   report_tech <- collapseNames(report_tech)
  #   
  #   var <- c("Tech|Electricity|Coal|PC|w/o CCS|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Gas|CC|w/o CCS|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Hydro|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Nuclear|Capital Costs (US$2005/kW)" ,
  #            "Tech|Electricity|Solar|PV|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Solar|CSP|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Wind|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Biomass|IGCC|w/o CCS|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Storage|Battery|For PV|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Oil|DOT|Capital Costs (US$2005/kW)",
  #            "Tech|Electricity|Geothermal|Capital Costs (US$2005/kW)",
  #            
  #            "Tech|Electricity|Coal|PC|w/o CCS|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Gas|CC|w/o CCS|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Hydro|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Nuclear|OM Cost|fixed (US$2005/kW/yr)" ,
  #            "Tech|Electricity|Solar|PV|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Solar|CSP|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Wind|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Biomass|IGCC|w/o CCS|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Oil|DOT|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Geothermal|OM Cost|fixed (US$2005/kW/yr)",
  #            "Tech|Electricity|Storage|Battery|For PV|OM Cost|fixed (US$2005/kW/yr)",NULL
  #   )
  #   # only capital costs
  #   cap_costs <- report_tech[,seq(2015,2050,5),grep(x = var,pattern = "Capital",value = T)]
  #   cap_costs <- cap_costs["GLO",,invert=T]# removing GLO
  #   
  #   cap_costs <- cap_costs[,,]/setYears(cap_costs[,"y2020",],NULL)# costs relative to 2020
  #   cap_costs[,seq(2025,2050,5),] <- cap_costs[,seq(2025,2050,5),]*1.50
  #   
  #   # the job intensity decreases with capital costs for techs, relative to 2020
  #   var_in_x <- getItems(x,dim=3.1) # variables from EF magpie object
  #   var_in_rem <- c("Coal","Gas","Nuclear","Biomass","Hydro","Hydro","Wind","Wind","Solar|PV",
  #                   "Geothermal","Solar|CSP","Oil","Solar|PV","Storage") # variables from remind
  #   var_com <- paste(var_in_x,var_in_rem,sep = ".") # concatenating the two for looping to work
  #   
  #   for (i in var_com){
  #     x[,,sub("\\..*", "", i)][,,c("CI","Manf")] <- x[,,sub("\\..*", "", i)][,,c("CI","Manf")]*setNames(cap_costs[,getYears(x),sub(".*\\.", "", i),pmatch=T],NULL)
  #   }
  #   # OM fixed costs used to calculate  decline factors for O&M employment factors
  #   fixed_costs <- report_tech[,seq(2015,2050,5),grep(x = var,pattern = "fixed",value = T)]
  #   fixed_costs <- fixed_costs["GLO",,invert=T]# removing GLO
  #   
  #   fixed_costs <- fixed_costs[,,]/setYears(fixed_costs[,"y2020",],NULL)# costs relative to 2015
  #   for (i in var_com){
  #     x[,,sub("\\..*", "", i)][,,"OM"] <- x[,,sub("\\..*", "", i)][,,"OM"]*setNames(fixed_costs[,getYears(x),sub(".*\\.", "", i),pmatch=T],NULL)
  #   }
  #   
  #   
  #   
  #   # decline factors for coal fuel supply
  #   coal_ef <- calcOutput("CoalLabourProductivity",subtype = "Employment_factor")
  #   coal_ef <- coal_ef/setYears(coal_ef[,2020,],NULL)
  #   coal_ef[which(coal_ef=="NaN")] <- 0
  #   x[,getYears(coal_ef),"Coal.Fuel_supply"] <- x[,getYears(coal_ef),"Coal.Fuel_supply"]*setNames(coal_ef[,,],NULL)  
  #   
  #   
  # }
  #  }
  if (decline=="static"){
    # do nothing, as employment factors have already been read
  }
  
  # the above loops are based on the expanded code below (2 shown for example)- 
  
  # x[,,"Coal"][,,c("CI","Manf")] <- x[,,"Coal"][,,c("CI","Manf")]*setNames(cap_costs[,getYears(x),"Coal",pmatch=T],NULL)
  # x[,,"Gas"][,,c("CI","Manf")] <- x[,,"Gas"][,,c("CI","Manf")]*setNames(cap_costs[,getYears(x),"Gas",pmatch=T],NULL)
  ##--------------------------------------------------------
  
  ## Other external parameters--------------------------
  #  share of exports to world installed capacity
  prod_share <- calcOutput("ProdShares")
  prod_share <- prod_share[,"y2019",] # use 2019 as 2018 values have NAs for important countries
  cols_to_add <- c("Solar|PV-utility","Solar|PV-rooftop","Wind offshore","Wind onshore")
  prod_share  <- add_columns(prod_share,addnm = cols_to_add,dim=3.1)
  prod_share[,,c("Solar|PV-utility","Solar|PV-rooftop")] <- prod_share[,,"spv"]
  prod_share[,,c("Wind offshore","Wind onshore")] <- prod_share[,,"wind"]
  
  # share of rooftop in total spv, wind offshore in total wind, and hydro small in total hydro
  share <- calcOutput("DspvShare")
  
  ##----------------------------------------------
  
  ## New and Absolute capacity from REMIND---------------------------------
  # filtering required capacity variables i.e., only new and existing coal capacities
  remind_output <- reportCapacity(gdx)
  rem_filter <- remind_output[,,c("Cumulative","Idle","Total Cap","CC","Hydrogen","Liquids",
                                  "Estimated","Non-Biomass","For Wind","For PV","Onshore","CT"),pmatch=T,invert=T]# removing cumulative and idle variables in capacity
  rem_filter <- rem_filter[,,c("Cap|Electricity (GW)",# total sum of all techs and not needed
                               "New Cap|Electricity (GW)",
                               "Cap|Electricity|Solar (GW)",# is sum of pv and csp and not needed
                               "New Cap|Electricity|Solar (GW)"),invert=T]
  # selecting variables only related to power or electricity
  rem_filter <- rem_filter[,,][,,"Electricity",pmatch=T]
  
  # filtering required regions
  regions <- setdiff(getRegions(remind_output),"GLO")# all regions except world
  rem_filter <- rem_filter[regions,seq(2015,2050,5),]
  
  ## disaggregating certain variables/adding new variables
  cols_to_add <- c("Solar|PV-utility","Solar|PV-rooftop","Hydro-large","Hydro-small",
                   "Wind offshore","Wind onshore")
  for (j in cols_to_add){
    rem_filter <- add_columns(rem_filter,addnm = paste0("New Cap|Electricity|",j," (GW)"),dim = 3.1)
    rem_filter <- add_columns(rem_filter,addnm = paste0("Cap|Electricity|",j," (GW)"),dim = 3.1)
  }
  
  # adding values to the new variables
  
  for (p in c("New Cap|Electricity|","Cap|Electricity|")){
    rem_filter[,,paste0(p,"Solar|PV-rooftop"," (GW)")] <- rem_filter[,,paste0(p,"Solar|PV"," (GW)")]*share[,,"spv"]
    rem_filter[,,paste0(p,"Solar|PV-utility"," (GW)")] <- rem_filter[,,paste0(p,"Solar|PV"," (GW)")]*(1-share[,,"spv"])
    rem_filter[,,paste0(p,"Hydro-small"," (GW)")] <- rem_filter[,,paste0(p,"Hydro"," (GW)")]*share[,,"hydro"]
    rem_filter[,,paste0(p,"Hydro-large"," (GW)")] <- rem_filter[,,paste0(p,"Hydro"," (GW)")]*(1-share[,,"hydro"])
    rem_filter[,,paste0(p,"Wind offshore"," (GW)")] <-   rem_filter[,,paste0(p,"Wind"," (GW)")]*share[,,"wind"]
    rem_filter[,,paste0(p,"Wind onshore"," (GW)")] <-   rem_filter[,,paste0(p,"Wind"," (GW)")]*(1-share[,,"wind"])
    
  }
  
  
  # added/installed capacity from REMIND, for years > 2010
  added_cap <- rem_filter[regions,getYears(rem_filter)>2010,"New Cap",pmatch=T]# only new capacities
  
  added_cap_sum <- dimSums(added_cap,dim = 1) # world added capacity for each tech
  
  # existing/operating capacity from REMIND
  cap_tot <- rem_filter[,,"New Cap",invert=T,pmatch=T]# only existing capacities
  
  ##-----------------------------------------------------
  
  # share of world capacity addition by region
  shr_GLO_add <- added_cap[,,cols_to_add,pmatch=T]/added_cap_sum[,,cols_to_add,pmatch=T]
  prod_share_tmp <- new.magpie(regions,c(2015,2020,2050),names = c("Solar|PV-utility","Solar|PV-rooftop","Wind offshore","Wind onshore")) 
  
  if (share=="local"||share=="current"){
    ## prod shares complete local manufacture of wind and solar components in 2050. Change is linear.
    # assigning 2050 regional share values as prod shares
    prod_share_tmp[,"y2050",] <- as.numeric(shr_GLO_add[,"y2050",getNames(prod_share_tmp),pmatch=T])
    # 2015 and 2020 values same as 2019 values
    prod_share_tmp[,c(2015,2020),] <- prod_share[,,getNames(prod_share_tmp)]
    # from 2020 to 2050, linearly interpolate
    prod_share <- time_interpolate(prod_share_tmp,seq(2025,2045,5),integrate_interpolated_years = T)
    if (share=="current"){
      prod_share[,,] <- prod_share[,2020,] # shares remain same throughout = 2020
    }
  }
  # Obtaining fuel supply variables
  remind_prod <- reportExtraction(gdx)["GLO",,,invert=T] # remove GLO
  vars <- c("PE|Production|Biomass (EJ/yr)",
            "PE|Production|Gross|Coal (EJ/yr)" ,                                       
            "PE|Production|Gross|Oil (EJ/yr)" ,                                        
            "PE|Production|Gross|Gas (EJ/yr)" ,
            "PE|Production|Gross|Uranium [Energy] (EJ/yr)"
  )
  remind_prod <- remind_prod[regions,getYears(x),vars]# period >2010 and only select variables
  
  pe <- reportPE(gdx)[regions,getYears(x),"PE|Biomass|Traditional (EJ/yr)"]
  
  # removing the traditional biomass component, where biomass isn't sold, thus no employment
  # in conventional sense
  remind_prod[,,"PE|Production|Biomass (EJ/yr)"] <-  remind_prod[,,"PE|Production|Biomass (EJ/yr)"] - setNames(pe[,,],NULL)
  
  #getNames(remind_prod) <- gsub(pattern = "\\[Energy\\]",replacement = "",x = getNames(remind_prod))
  getNames(remind_prod) <- gsub(pattern = "  \\(EJ\\/yr\\)",replacement = " \\(EJ\\/yr\\)",x = getNames(remind_prod))# removing space
  getNames(remind_prod) <- gsub(pattern = "Uranium",replacement = "Nuclear",x = getNames(remind_prod))# removing space
  
  ## Calculating jobs--------------------------------
  techs <- c("Solar|PV-utility","Solar|PV-rooftop","Hydro-large","Hydro-small",
             "Wind offshore","Wind onshore","Coal","Biomass","Gas","Storage","Oil","Nuclear","Geothermal","Solar|CSP")
  techs_exp <-  c("Solar|PV-utility","Solar|PV-rooftop","Wind offshore","Wind onshore") # technologies with export component
  
  # cons <- new.magpie(getRegions(x),names = techs)
  # average construction time for different technologies
  # cons[] <- rep(c(1,1,4,3,4,2,5,2,2,1,2,10,2,2),each=12)
  
  # 1. Manufacturing jobs for techs with export component.
  
  # 1a) Total manf jobs (per region) for these techs are equal to total world addition X share of 
  # world exports (for that region)
  if (type=="created"){
    jobs_manf <- new.magpie(regions,getYears(x),techs,fill=0)
    for (i in techs_exp){
      #manf <- paste0(i,".","Manf")
      jobs_manf[,,i] <- added_cap_sum[,getYears(x),i,pmatch=T]*1000 
      jobs_manf[,getYears(x),i] <-  jobs_manf[,getYears(x),i] * setNames(x[,,i][,,"Manf"],NULL) * prod_share[,,i] 
    }
    
    for (i in setdiff(techs,techs_exp)){
      # manf <- paste0(i,".","Manf")
      jobs_manf[,,i] <- added_cap[,getYears(x),i,pmatch=T]*1000 
      jobs_manf[,getYears(x),i] <- jobs_manf[,getYears(x),i] *  setNames(x[,,i,pmatch=T][,,"Manf"],NULL)
    }
    
    jobs_manf <- add_dimension(jobs_manf,dim=3.2,add="type",nm = "Manf")
    getSets(jobs_manf) <- c("region","year","variable","type")
    
    # 2. Construction and Installation jobs
    jobs_ci <- new.magpie(regions,getYears(x),techs)
    
    
    for (i in techs){
      #inst <- paste0(i,".","CI")
      jobs_ci[,,i] <- added_cap[,getYears(x),i,pmatch=T]*1000 
      jobs_ci[,,i] <-  jobs_ci[,getYears(x),i] *  setNames(x[,,i,pmatch=T][,,"CI"],NULL)
    }
    
    jobs_ci <- add_dimension(jobs_ci,dim=3.2,add="type",nm = "CI")
    getSets(jobs_ci) <- c("region","year","variable","type")
    
  }
  
  if (type=="per_year"){
    jobs_manf <- new.magpie(regions,getYears(x),techs,fill=0)
    for (i in techs_exp){
      #manf <- paste0(i,".","Manf")
      jobs_manf[,,i] <- added_cap_sum[,getYears(x),i,pmatch=T]*1000 
      jobs_manf[,getYears(x),i] <-  (jobs_manf[,getYears(x),i] * setNames(x[,,i][,,"Manf"],NULL) * prod_share[,,i])
    }
    
    for (i in setdiff(techs,techs_exp)){
      # manf <- paste0(i,".","Manf")
      jobs_manf[,,i] <- added_cap[,getYears(x),i,pmatch=T]*1000 
      jobs_manf[,getYears(x),i] <- (jobs_manf[,getYears(x),i] *  setNames(x[,getYears(x),i,pmatch=T][,,"Manf"],NULL))
    }
    
    jobs_manf <- add_dimension(jobs_manf,dim=3.2,add="type",nm = "Manf")
    getSets(jobs_manf) <- c("region","year","variable","type")
    
    # 2. Construction and Installation jobs
    jobs_ci <- new.magpie(regions,getYears(x),techs,fill = 0)
    
    
    for (i in techs){
      #inst <- paste0(i,".","CI")
      jobs_ci[,,i] <- added_cap[,getYears(x),i,pmatch=T]*1000 
      #jobs_ci[,,i] <-  (jobs_ci[,getYears(x),i] *  setNames(x[,,i,pmatch=T][,,"CI"],NULL))
      jobs_ci[,,i] <-  jobs_ci[,getYears(x),i] *  setNames(x[,,i,pmatch=T][,,"CI"],nm = NULL)
      
    }
    
    jobs_ci <- add_dimension(jobs_ci,dim=3.2,add="type",nm = "CI")
    getSets(jobs_ci) <- c("region","year","variable","type")
    
  }
  
  
  ## 3. Jobs in O & M
  # jobs = existing capacity per tech * emp factor per tech
  jobs_om <- new.magpie(regions,getYears(x),techs)
  for (i in techs){
    # inst <- paste0(i,".","OM")
    jobs_om[,,i] <- cap_tot[,getYears(x),i,pmatch=T]*1000 
    jobs_om[,,i] <-  jobs_om[,,i] *  setNames(x[,,i,pmatch=T][,,"OM"],NULL)
  }
  jobs_om <- add_dimension(jobs_om,dim=3.2,add="type",nm = "OM")
  getSets(jobs_om) <- c("region","year","variable","type")
  
  ## 4. Jobs in Fuel supply 
  #jobs = total production (in EJ/yr) per fuel * employment factor per fuel (Jobs/PJ)
  fuels <- c("Coal","Gas","Biomass","Oil")
  jobs_prod <- new.magpie(regions,getYears(x),techs,fill=0)
  for (i in fuels){
    #fs <- paste0(i,".","Fuel_supply")
    jobs_prod[,,i] <- remind_prod[,getYears(x),i,pmatch=T] * 1000 
    jobs_prod[,,i] <-  jobs_prod[,,i] * setNames(x[,,i][,,"Fuel_supply"],NULL)
  }
  jobs_prod <- add_dimension(jobs_prod,dim=3.2,add="type",nm = "Fuel_supply")
  getSets(jobs_prod) <- c("region","year","variable","type")
  
  ## JObs in Fuel supply, nuclear which are given in terms of SE
  rem_se_nuc <- reportSE(gdx)[regions,getYears(x),"Nuclear",pmatch=T]*277777.778 # EJ to GWh
  jobs_prod[,,"Nuclear.Fuel_supply"] <- setNames(rem_se_nuc[,,],NULL)*setNames(x[,,"Nuclear.Fuel_supply"],NULL)
  
  ####### put all together #############
  
  jobs <- mbind(jobs_ci,jobs_om,jobs_prod,jobs_manf)
  
  return (jobs)
  
}



