#' Computes the employment values (jobs) across different sectors 
#' @param gdx A gdx file output from a REMIND run
#' @param improvements source of employment factor data, options "None", "Dias","CEEW", "Dias+CEEW"
#' @description This function returns a magpie object containing the reporting the jobs for different technologies
#' @return A magpie object 
#' @author Aman Malik
#' @examples
#' 
#'   \dontrun{
#'     reportEmployment(gdx,improvements="Dias")
#'   }
#' @importFrom madrat calcOutput 
#' @importFrom magclass getNames add_columns

reportEmployment <- function(gdx,improvements){
  ###### input data
  techs <- c("Solar|PV","Solar|CSP","Wind","Biomass","Coal","Gas",
             "Nuclear","Hydro")
  techs_exp <-  c("Solar|PV","Wind","Hydro") # technologies with export component
  # employment factors
  x <- calcOutput("Employmentfactors",improvements=improvements)
  
  # capital costs evolution over time for different techs
  cap_costs <- reportTechnology(gdx)
  # use a mif file until reportTechnology() is not resolved
  cap_costs <- collapseNames(cap_costs)
  var <- c("Tech|Electricity|Coal|PC|w/o CCS|Capital Costs (US$2005/kW)",
           "Tech|Electricity|Gas|CC|w/o CCS|Capital Costs (US$2005/kW)",
           "Tech|Electricity|Hydro|Capital Costs (US$2005/kW)",
           "Tech|Electricity|Nuclear|Capital Costs (US$2005/kW)" ,
           "Tech|Electricity|Solar|PV|Capital Costs (US$2005/kW)",
           "Tech|Electricity|Solar|CSP|Capital Costs (US$2005/kW)",
           "Tech|Electricity|Wind|Capital Costs (US$2005/kW)",
           "Tech|Electricity|Biomass|IGCC|w/o CCS|Capital Costs (US$2005/kW)",NULL
           
           )
  cap_costs <- cap_costs[,seq(2015,2050,5),var]
  cap_costs <- cap_costs["GLO",,invert=T]
  
  cap_costs <- cap_costs[,,]/setYears(cap_costs[,"y2015",],NULL)# costs relative to 2015
  # the job intensity decreases with capital costs for techs, relative to 2015
  x[,,"Coal"] <- x[,,"Coal"]*setNames(cap_costs[,getYears(x),"Coal",pmatch=T],NULL)
  x[,,"Gas"] <- x[,,"Gas"]*setNames(cap_costs[,getYears(x),"Gas",pmatch=T],NULL)
  x[,,"Hydro"] <- x[,,"Hydro"]*setNames(cap_costs[,getYears(x),"Hydro",pmatch=T],NULL)
  x[,,"Nuclear"] <- x[,,"Nuclear"]*setNames(cap_costs[,getYears(x),"Nuclear",pmatch=T],NULL)
  x[,,"Solar|PV"] <- x[,,"Solar|PV"]*setNames(cap_costs[,getYears(x),"Solar|PV",pmatch=T],NULL)
  x[,,"Wind"] <- x[,,"Wind"]*setNames(cap_costs[,getYears(x),"Wind",pmatch=T],NULL)
  x[,,"Solar|CSP"] <- x[,,"Solar|CSP"]*setNames(cap_costs[,getYears(x),"Solar|CSP",pmatch=T],NULL)
  x[,,"Biomass"] <- x[,,"Biomass"]*setNames(cap_costs[,getYears(x),"Biomass",pmatch=T],NULL)
  
  # # share of exports to world installed capacity
  export_share <- calcOutput("Exportshare")
  export_share <- export_share[,"y2017",] # use 2017 as 2018 values have NAs for important countries
  getNames(export_share) <- gsub(pattern = "Solar",replacement = "Solar|PV",x = getNames(export_share))
  getNames(export_share) <- gsub("Hydropower",replacement = "Hydro",x = getNames(export_share))
 
  ## gdx from REMIND
  # filtering required capacity variables i.e., only new and existing coal capacities
  remind_output <- remind::reportCapacity(gdx)
  vars <- "\\Cap\\|Electricity|New Cap\\|Electricity" # only interested in new and current capacities for different techs
  r_vars <- grep(vars,getNames(remind_output),value = T)
  rem_filter <- remind_output[,,r_vars]
  
  # filtering required regions
  regions <- setdiff(getRegions(remind_output),"GLO")# all regions except world
  rem_filter <- rem_filter[regions,,]
  
  # filtering required technologies
  # only need specific instances of technologies
  tech_add <- c(techs,"Solar\\|PV","Solar\\|CSP","Oil\\|w\\/o CCS") # for cases with a "|"
  tech_mod <- paste(paste0(tech_add," ","\\(GW\\)"),collapse = "|") # adding special cases to normal cases
  new_techs <- grep(tech_mod,getNames(rem_filter),value=T)# instances only with tech_mod
  new_techs <- grep("Storage",new_techs,value=T,invert = T)# removing "storage"
  rem_filter <- rem_filter[,,new_techs] 
  getNames(rem_filter) <- gsub(pattern = "\\|w\\/o CCS",replacement = "",x = getNames(rem_filter)) # removing the CCS entry
  rem_filter <- rem_filter[,,c("Cumulative","Idle"),pmatch=T,invert=T]# removing cumulative and idle variables in capaicty
  
  # added capacity from REMIND
  added_cap <- rem_filter[regions,,"New Cap",pmatch=T]# only new capacities
  added_cap <- add_columns(added_cap,addnm = "New Cap|Electricity|Oil (GW)",dim = 3.1) # adding Oil variable as it doesn't exist
  added_cap[,,"Oil",pmatch=T] <- 0
  added_cap <- added_cap[,getYears(rem_filter)[getYears(rem_filter,as.integer = T)>2010],]
  added_cap_sum <- dimSums(added_cap,dim = 1) # world added capacity
  
  # existing capacity from REMIND
  cap_tot <- rem_filter[,,"New",invert=T,pmatch=T]# only existing capacities
  cap_tot <- cap_tot[,getYears(rem_filter)[getYears(rem_filter,as.integer = T)>2010],]
  
  # seperating fuel supply variables
  remind_prod <- remind::reportExtraction(gdx)
  vars <- c("PE|Production|Biomass (EJ/yr)",
            "PE|Production|Gross|Coal (EJ/yr)" ,                                       
            "PE|Production|Gross|Oil (EJ/yr)" ,                                        
            "PE|Production|Gross|Gas (EJ/yr)" ,
            "PE|Production|Gross|Uranium [Energy] (EJ/yr)"
  )
  remind_prod <- remind_prod[,,vars,]
  remind_prod <- remind_prod["GLO",,,invert=T]
  remind_prod <- remind_prod[,getYears(rem_filter)[getYears(rem_filter,as.integer = T)>2010],]
  #getNames(remind_prod) <- gsub(pattern = "\\[Energy\\]",replacement = "",x = getNames(remind_prod))
  getNames(remind_prod) <- gsub(pattern = "  \\(EJ\\/yr\\)",replacement = " \\(EJ\\/yr\\)",x = getNames(remind_prod))# removing space
  getNames(remind_prod) <- gsub(pattern = "Uranium",replacement = "Nuclear",x = getNames(remind_prod))# removing space
  
  ## Calculating jobs

  # 1. Manufacturing jobs for techs with export component.
  
  # 1a) Total manf jobs (per region) for these techs are equal to total world addition X share of 
  # world exports (for that region)
  jobs_manf_exp <- new.magpie(regions,getYears(added_cap),techs,fill=0)
  for (i in techs_exp){
    manf <- paste0(i,".","Manf")
    jobs_manf_exp[,,i] <- added_cap_sum[,,i,pmatch=T]*1000 
    jobs_manf_exp[,getYears(x),i] <-  jobs_manf_exp[,getYears(x),i] * setNames(x[,,manf],NULL) * setYears(export_share[,,i],nm = NULL) 
  }
  
  jobs_manf_exp <- add_dimension(jobs_manf_exp,dim=3.2,add="type",nm = "Manf_exp")
  getSets(jobs_manf_exp) <- c("region","year","variable","type")
  
  # 1b) manufacturing jobs for techs completely manufactured domestically. These are
  # fossil fuel, biomass, hydro and nuclear. 
  jobs_manf_dom <- new.magpie(regions,getYears(added_cap),techs,fill=0)
  for (i in setdiff(techs,techs_exp)){
    manf <- paste0(i,".","Manf")
    jobs_manf_dom[,,i] <- added_cap[,,i,pmatch=T]*1000 
    jobs_manf_dom[,getYears(x),i] <-  jobs_manf_dom[,getYears(x),i] *  setNames(x[,,manf],NULL)
  }
  jobs_manf_dom <- add_dimension(jobs_manf_dom,dim=3.2,add="type",nm = "Manf_dom")
  getSets(jobs_manf_dom) <- c("region","year","variable","type")
  
  # Overall Manufacturing jobs
  jobs_manf <- jobs_manf_dom[,,] + jobs_manf_exp[,,]
  jobs_manf <- collapseNames(jobs_manf)
  jobs_manf<- add_dimension(jobs_manf,dim=3.2,add="type",nm = "Manf")

  # 2. Construction and Installation jobs
  
  jobs_ci <- new.magpie(regions,getYears(added_cap),techs)
  
  for (i in techs[techs!="Solar|PV"]){
      inst <- paste0(i,".","CI")
      jobs_ci[,,i] <- added_cap[,,i,pmatch=T]*1000 
      jobs_ci[,getYears(x),i] <-  jobs_ci[,getYears(x),i] *  setNames(x[,,inst],NULL)
  }
  # special case for solar PV - dividing into rooftop and utility
  jobs_ci[,,"Solar|PV"] <- added_cap[,,"Solar|PV",pmatch=T]*1000 
  
  share_solar = 0.7 # share of  utility-scale installations in total PV installations, i.e. utility+rooftop
  factor_solar = 8 # factor by which  EF/CI for rooftop should be multiplied compared to utility EF/CI. Number comes from India CEEW study
  # Note: right now both the above factors are constant and should change with region and time.
  jobs_ci[,getYears(x),"Solar|PV"] <-  jobs_ci[,getYears(x),"Solar|PV"] * setNames(x[,,"Solar|PV.CI"],NULL) * share_solar +
                          jobs_ci[,getYears(x),"Solar|PV"] * (1-share_solar) * setNames(x[,,"Solar|PV.CI"],NULL) * factor_solar
  
  jobs_ci <- add_dimension(jobs_ci,dim=3.2,add="type",nm = "CI")
  getSets(jobs_ci) <- c("region","year","variable","type")
   
  ## 3. Jobs in O & M
  # jobs = existing capacity per tech * emp factor per tech
  jobs_om <- new.magpie(regions,getYears(added_cap),techs)
  for (i in techs){
    inst <- paste0(i,".","OM")
    jobs_om[,,i] <- cap_tot[,,i,pmatch=T]*1000 
    jobs_om[,getYears(x),i] <-  jobs_om[,getYears(x),i] *  setNames(x[,,inst],NULL)
  }
  jobs_om <- add_dimension(jobs_om,dim=3.2,add="type",nm = "OM")
  getSets(jobs_om) <- c("region","year","variable","type")
  
  ## 4. Jobs in Fuel supply (coal)
  #jobs = total production (in EJ/yr) per fuel * employment factor per fuel
  fuels <- c("Coal","Gas","Biomass","Nuclear")
  jobs_prod <- new.magpie(regions,getYears(added_cap),techs,fill=0)
    for (i in fuels){
      fs <- paste0(i,".","Fuel_supply")
      jobs_prod[,,i] <- remind_prod[,,i,pmatch=T] * 1000 
      jobs_prod[,getYears(x),i] <-  jobs_prod[,getYears(x),i] * setNames(x[,,fs],NULL)
    }
  jobs_prod <- add_dimension(jobs_prod,dim=3.2,add="type",nm = "Fuel_supply")
  getSets(jobs_prod) <- c("region","year","variable","type")
  
  
  ####### put all together #############

  jobs <- mbind(jobs_ci,jobs_om,jobs_prod,jobs_manf)

  return (jobs)
  
  }

  
  
