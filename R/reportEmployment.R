#' Computes the employment values (jobs) across different sectors
#' @param gdx A gdx object
#' @description This function returns a magpie object containing the reporting the jobs for different technologies
#' @return A magpie object 
#' @author Aman Malik
#' @examples
#' 
#'   \dontrun{
#'     reportEmployment(gdx)
#'   }
#' @importFrom gdx readGDX
#' @importFrom madrat calcOutput
#' @importFrom magclass getNames add_columns


reportEmployment <- function(gdx){
  ###### input data
  techs <- c("Solar|PV","Solar|CSP","Wind","Hydro","Biomass","Coal","Gas","Nuclear","Oil")
  # employment factors
  x <- calcOutput(type = "Employmentfactors")
  
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
  
  
  # added capacity
  added_cap <- rem_filter[regions,,"New Cap",pmatch=T]# only new capacities
  added_cap <- add_columns(added_cap,addnm = "New Cap|Electricity|Oil (GW)",dim = 3.1) # adding Oil variable as it doesn't exist
  added_cap[,,"Oil",pmatch=T] <- 0
  added_cap <- added_cap[,getYears(rem_filter)[getYears(rem_filter,as.integer = T)>2010],]
  
  # existing capacity 
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
  
  ## calculating jobs
  
  # manufacturing jobs
  jobs_manf <- new.magpie(regions,getYears(added_cap),techs)
  for (i in techs){
    inst <- paste0(i,".","Manf")
    jobs_manf[,,i] <- added_cap[,,i,pmatch=T]*1000 
    jobs_manf[,,i] <-  jobs_manf[,,i] *  x[,,inst]
  }
  
  # installation jobs
  jobs_inst <- new.magpie(regions,getYears(added_cap),techs)
  for (i in techs){
      inst <- paste0(i,".","CI")
      jobs_inst[,,i] <- added_cap[,,i,pmatch=T]*1000 
      jobs_inst[,,i] <-  jobs_inst[,,i] *  x[,,inst]
    }
   
  
  
  ## . Jobs in O & M
  # jobs = existing capacity per tech * emp factor per tech
  jobs_om <- new.magpie(regions,getYears(added_cap),techs)
  for (i in techs){
    inst <- paste0(i,".","OM")
    jobs_om[,,i] <- cap_tot[,,i,pmatch=T]*1000 
    jobs_om[,,i] <-  jobs_om[,,i] *  x[,,inst]
  }
  
  ##5. Jobs in Fuel supply (coal)
  #jobs = total production (in EJ/yr) per fuel * employment factor per fuel
  fuels <- c("Coal","Oil","Gas","Biomass","Nuclear")
  jobs_prod <- new.magpie(regions,getYears(added_cap),techs,fill=0)
    for (i in fuels){
      fs <- paste0(i,".","Fuel_supply")
      jobs_prod[,,i] <- remind_prod[,,i,pmatch=T] * 1000 
      jobs_prod[,,i] <-  jobs_prod[,,i] * x[,,fs]
    }
  
  ####### put all together #############
  jobs <- jobs_inst + jobs_om +  jobs_prod +  jobs_manf
  getSets(jobs) <- c("region","year","variable")

  return (jobs)
  
  }
 
  
    

