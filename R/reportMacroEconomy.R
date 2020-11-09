#' Read in GDX and calculate macro economy values, used in convGDX2MIF.R for
#' the reporting
#' 
#' Read in macro economy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @author Lavinia Baumstark, Anselm Schultes
#' @examples
#' 
#' \dontrun{reportMacroEconomy(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass setNames mbind getYears dimSums new.magpie getRegions setYears

reportMacroEconomy <- function(gdx,regionSubsetList=NULL){
  
  #---- Functions ----
  find_real_module <- function(module_set, module_name){
    return(module_set[module_set$modules == module_name,2])
  }
  
  ####### conversion factors ##########
  TWa_2_EJ     <- 31.536
  #####################################
  t2005to2150        <- c("y2005", "y2010", "y2015", "y2020" ,"y2025", "y2030", "y2035", "y2040", "y2045","y2050","y2055", "y2060", "y2070", "y2080", "y2090", "y2100", "y2110" ,"y2130" ,"y2150")
  
  module2realisation <- readGDX(gdx, "module2realisation", react = "silent")
  
  vm_cesIO   <- readGDX(gdx,c("vm_cesIO","v_vari"),field="l",format="first_found")
  
  vm_invMacro <- readGDX(gdx,c("vm_invMacro","v_invest"),field="l",format="first_found")
  pm_pvp     <- readGDX(gdx,name=c("pm_pvp","p80_pvp"),format="first_found")[,,"good"]
  ppfen_stat <- readGDX(gdx,c("ppfen_stationary_dyn38","ppfen_stationary_dyn28","ppfen_stationary"),format="first_found", react = "silent")
  
  p36_floorspace <- readGDX(gdx, c("p36_floorspace"), react = "silent")

  if (length(ppfen_stat) == 0) ppfen_stat = NULL
  ppfen_build <- readGDX(gdx,c("ppfen_buildings_dyn36","ppfen_buildings_dyn28","ppfen_buildings"),format="first_found", react = "silent")
  ppfen_ind <- readGDX(gdx,c("ppfen_industry_dyn37","ppfen_industry_dyn28","ppfen_industry"),format="first_found", react = "silent")
  ppfkap_ind <- readGDX(gdx, 'ppfKap_industry_dyn37', react = 'silent')
  
  # Realisation of the different modules
  if ( !is.null(module2realisation) & (!"CES_structure" %in% module2realisation[, 1] )){
    stat_mod = find_real_module(module2realisation,"stationary")
    tran_mod = find_real_module(module2realisation,"transport")
    indu_mod = find_real_module(module2realisation,"industry")
    buil_mod = find_real_module(module2realisation,"buildings")
  } else {                                                       
    if ( !is.null(ppfen_stat)){   # In case the set module2realisation did not exist, find out whether it was stationary or buildings-industry
      stat_mod = "simple"
      indu_mod = "off"
      buil_mod = "off"
    } else {
      stat_mod = "off"
      indu_mod = "fixed_shares"
      buil_mod = "simple"
    }
  }
  #####################################
  #choose the CES entries names for transport
  name_trsp=c("fepet","ueLDVt","fedie","ueHDVt","feelt","ueelTt")
  name_trsp=name_trsp[name_trsp%in%getNames(vm_cesIO)]
  #####################################
  
  cons     <- setNames(readGDX(gdx,name='vm_cons',field="l",format="first_found")[,t2005to2150,]*1000,
                  "Consumption (billion US$2005/yr)")
  gdp      <- setNames(vm_cesIO[,,"inco"]*1000,        "GDP|MER (billion US$2005/yr)") 
  shPPPMER <- readGDX(gdx,c("pm_shPPPMER","p_ratio_ppp"),format="first_found")
  gdp_ppp  <- setNames(gdp /shPPPMER, "GDP|PPP (billion US$2005/yr)")
  invE     <- setNames(readGDX(gdx,name=c("v_costInv","v_costin"),field="l",format="first_found")*1000,
                  "Energy Investments (billion US$2005/yr)")
  pop      <- setNames(readGDX(gdx,name=c("pm_pop","pm_datapop"),format="first_found")[,t2005to2150,]*1000,
                  "Population (million)") 
  
  
  damageFactor      <- setNames(readGDX(gdx,name=c("vm_damageFactor","vm_damage"),field='l',format="first_found"),
                       "Damage factor (1)") 
  
  ies                     <- readGDX(gdx,c("pm_ies","p_ies"),format="first_found")
  prtp                    <- readGDX(gdx,"pm_prtp")
  c_damage                <- readGDX(gdx,"cm_damage","c_damage",format="first_found")
  forcOs                  <- readGDX(gdx,"vm_forcOs",field="l")[,t2005to2150,]
  inconvPen               <- readGDX(gdx,c("v02_inconvPen","v_inconvPen"),field="l")[,t2005to2150,]
  inconvPenCoalSolids     <- readGDX(gdx,c("v02_inconvPenCoalSolids","v_inconvPenCoalSolids"),field="l")[,t2005to2150,]
  
  welf <- cons
  getNames(welf) <- "Welfare|Real and undiscounted|Yearly (arbitrary unit/yr)"
  for (regi in getRegions(cons)){
    if (ies[regi,,] == 1){
      welf[regi,,] = setNames(pop[regi,,] * log(  1000 * cons[regi,,] * (1- ( c_damage * forcOs ) ) / pop[regi,,] ), "Welfare|Real and undiscounted|Yearly (arbitrary unit/yr)")
      # in REMIND (without factor 1000, which was added here to prevent negative numbers):   (log((vm_cons(ttot,regi)*(1-c_damage*vm_forcOs(ttot)*vm_forcOs(ttot))) / pm_pop(ttot,regi)))$(pm_ies(regi) eq 1)
    } else {
      welf[regi,,] = setNames(pop[regi,,] *
                        ( ( 1000 * cons[regi,,] * (1- ( c_damage * forcOs ) ) / pop[regi,,] ) ** ( 1 - 1/ies[regi,,] ) - 1 ) / ( 1-1/ies[regi,,] ), "Welfare|Real and undiscounted|Yearly (arbitrary unit/yr)")
      # in REMIND (without factor 1000, which was added here to prevent negative numbers):   ((( (vm_cons(ttot,regi)*(1-c_damage*vm_forcOs(ttot)*vm_forcOs(ttot)))/pm_pop(ttot,regi))**(1-1/pm_ies(regi))-1)/(1-1/pm_ies(regi)) )$(pm_ies(regi) ne 1)
    }
  }
  
  if (buil_mod %in% c("services_putty","services_with_capital")) {
    #-Capital Stocks
    cap = mbind(
                setNames(vm_cesIO[ , ,"kaphc"]*1000,        "Capital Stock|Non-ESM|Space Conditioning (billion US$2005)"),
                setNames(vm_cesIO[ , ,"kapal"]*1000,        "Capital Stock|Non-ESM|Appliances and Light (billion US$2005)"),
                setNames(vm_cesIO[ , ,"kapsc"]*1000,        "Capital Stock|Non-ESM|Space Cooling (billion US$2005)"),
                setNames(vm_cesIO[ , ,"kap"]*1000,        "Capital Stock|Non-ESM|Macro (billion US$2005)"))
    cap = mbind(cap,
                setNames(cap[,,"Capital Stock|Non-ESM|Space Conditioning (billion US$2005)"]
                         +cap[,,"Capital Stock|Non-ESM|Appliances and Light (billion US$2005)"]
                         +cap[,,"Capital Stock|Non-ESM|Space Cooling (billion US$2005)"],
                "Capital Stock|Non-ESM|End-use (billion US$2005)"))
    cap = mbind(cap,
                setNames(cap[,,"Capital Stock|Non-ESM|End-use (billion US$2005)"]
                         +cap[,,"Capital Stock|Non-ESM|Macro (billion US$2005)"],
                         "Capital Stock|Non-ESM (billion US$2005)"))
    
    
    #-Capital investments
    
    invM = mbind(
      setNames(vm_invMacro[ , ,"kaphc"]*1000,"Investments|Non-ESM|Space Conditioning (billion US$2005/yr)"),
      setNames(vm_invMacro[ , ,"kapal"]*1000,"Investments|Non-ESM|Appliances and Light (billion US$2005/yr)"),
      setNames(vm_invMacro[ , ,"kapsc"]*1000,"Investments|Non-ESM|Space Cooling (billion US$2005/yr)"),
      setNames(vm_invMacro[ , ,"kap"]*1000,"Investments|Non-ESM|Macro (billion US$2005/yr)")
    )
    invM = mbind( invM,
                  setNames(invM[,,"Investments|Non-ESM|Space Conditioning (billion US$2005/yr)"]
                           +invM[,,"Investments|Non-ESM|Appliances and Light (billion US$2005/yr)"]
                           +invM[,,"Investments|Non-ESM|Space Cooling (billion US$2005/yr)"],
                           "Investments|Non-ESM|End-use (billion US$2005/yr)")
    )
    
    invM = mbind(invM,
                 setNames(invM[,,"Investments|Non-ESM|End-use (billion US$2005/yr)"]
                          +invM[,,"Investments|Non-ESM|Macro (billion US$2005/yr)"],
                          "Investments|Non-ESM (billion US$2005/yr)"))
    
    #add floorspace
    invM <- mbind(invM,
                  setNames(p36_floorspace[,getYears(invM),] * 1000, "Floorspace demand (million m2)") 
            )
    
  } else {
    cap      <- setNames(vm_cesIO[ , ,"kap"]*1000,        "Capital Stock|Non-ESM (billion US$2005)")
    invM     <- setNames(vm_invMacro[ , ,"kap"]*1000,
                         "Investments|Non-ESM (billion US$2005/yr)")
    
  }
    
  inv      <- setNames(invM[,,"Investments|Non-ESM (billion US$2005/yr)"] + invE, "Investments (billion US$2005/yr)")
  # TODO: add p80_curracc
  #curracc <- setNames(readGDX(gdx,'p80_curracc',field='l',format='first_found') * 1000,"Current Account (billion US$2005/yr)")
  vm_welfare <- readGDX(gdx,c('v02_welfare','v_welfare','vm_welfare'),field='l',format='first_found',react = 'warning') 
  if(!is.null(vm_welfare)) setNames(vm_welfare,"Welfare|Real (1)")
  
  ces <- NULL

  
  #macro 
  ces <- mbind(ces,setNames(vm_cesIO[,,"kap"] * 1000,"CES_input|kap (billion US$2005)"))
  
  #transport
  ces <- mbind(ces,setNames(vm_cesIO[,,name_trsp[3]] * TWa_2_EJ,"CES_input|feelt (EJ/yr)"))
  ces <- mbind(ces,setNames(vm_cesIO[,,name_trsp[1]] * TWa_2_EJ,"CES_input|fepet (EJ/yr)"))
  ces <- mbind(ces,setNames(vm_cesIO[,,name_trsp[2]] * TWa_2_EJ,"CES_input|fedie (EJ/yr)"))
  
  if (stat_mod == "simple"){
    ces <- mbind(ces,setNames(vm_cesIO[,,"feels"] * TWa_2_EJ,"CES_input|feels (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fegas"] * TWa_2_EJ,"CES_input|fegas (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fehos"] * TWa_2_EJ,"CES_input|fehos (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fesos"] * TWa_2_EJ,"CES_input|fesos (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fehes"] * TWa_2_EJ,"CES_input|fehes (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"feh2s"] * TWa_2_EJ,"CES_input|feh2s (EJ/yr)"))
  }
  
  if (buil_mod == "simple"){
    ces <- mbind(ces,setNames(vm_cesIO[,,"feelb"] * TWa_2_EJ,"CES_input|feelb (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fegab"] * TWa_2_EJ,"CES_input|fegab (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fehob"] * TWa_2_EJ,"CES_input|fehob (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fesob"] * TWa_2_EJ,"CES_input|fesob (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"feheb"] * TWa_2_EJ,"CES_input|feheb (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"feh2b"] * TWa_2_EJ,"CES_input|feh2b (EJ/yr)"))
  } else if (buil_mod %in% c("services_putty","services_with_capital")){
    ces <- mbind(ces,setNames(vm_cesIO[,,"fealelb"] * TWa_2_EJ,"CES_input|fealelb (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fescelb"] * TWa_2_EJ,"CES_input|fescelb (EJ/yr)"))
    
    ces <- mbind(ces,setNames(vm_cesIO[,,"uescb"] * TWa_2_EJ,"CES_input|uescb (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"ueshb"] * TWa_2_EJ,"CES_input|ueshb (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"ueswb"] * TWa_2_EJ,"CES_input|ueswb (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"uealb"] * TWa_2_EJ,"CES_input|uealb (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"uecwb"] * TWa_2_EJ,"CES_input|uecwb (EJ/yr)"))
    
    
    ces <- mbind(ces,setNames(vm_cesIO[,,"kapal"] * 1000,"CES_input|kapal (billion US$2005)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"kapsc"] * 1000,"CES_input|kapsc (billion US$2005)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"kaphc"] * 1000,"CES_input|kaphc (billion US$2005)"))
  }
  
  
  if (indu_mod == "fixed_shares"){
    ces <- mbind(ces,setNames(vm_cesIO[,,"feeli"] * TWa_2_EJ,"CES_input|feeli (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fegai"] * TWa_2_EJ,"CES_input|fegai (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fehoi"] * TWa_2_EJ,"CES_input|fehoi (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fesoi"] * TWa_2_EJ,"CES_input|fesoi (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"fehei"] * TWa_2_EJ,"CES_input|fehei (EJ/yr)"))
    ces <- mbind(ces,setNames(vm_cesIO[,,"feh2i"] * TWa_2_EJ,"CES_input|feh2i (EJ/yr)"))
  } else if ('subsectors' == indu_mod) {
    
    ces <- mbind(
      ces, 
      mbind(
        lapply(ppfkap_ind, 
               function(x) {
                 setNames(vm_cesIO[,,x] * 1000, 
                          paste0('CES_input|', x, ' (billion US$2005)'))
               }
        )),
      
      mbind(lapply(ppfen_ind, 
                   function(x) {
                     setNames(vm_cesIO[,,x] * TWa_2_EJ, 
                              paste0('CES_input|', x, ' (EJ/yr)'))
                   }
      ))
    )
  }
  
  #define list of variables that will be exported:
  varlist <- list(cons,gdp,gdp_ppp,invE,invM,pop,cap,inv,ces,damageFactor,welf) #,curracc)
  # use the same temporal resolution for all variables
  # calculate minimal temporal resolution
  tintersect = Reduce(intersect,lapply(varlist,getYears))
  varlist <- lapply(varlist,function(v) v[,tintersect,])
   
  # put all together
  out <- Reduce(mbind,varlist)
  
  # add global region aggregation
  out <- mbind(out,dimSums(out,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))
  
  # calculate interest rate
  inteRate <- new.magpie(getRegions(out),getYears(out),c("Interest Rate (t+1)/(t-1)|Real ()","Interest Rate t/(t-1)|Real ()"),fill=0)
  for (t in getYears(out[,which(getYears(out,as.integer=TRUE)>2005 & getYears(out,as.integer=TRUE)<max(getYears(out,as.integer=TRUE)) ),])){
    inteRate[,t,"Interest Rate (t+1)/(t-1)|Real ()"] <- 
       (1
        - ( (setYears(pm_pvp[,(which(getYears(pm_pvp)==t)+1),],t) / setYears(pm_pvp[,(which(getYears(pm_pvp)==t)-1),],t)) 
            ^ (1/(getYears(pm_pvp[,(which(getYears(pm_pvp)==t)+1),],as.integer=TRUE)-getYears(pm_pvp[,(which(getYears(pm_pvp)==t)-1),],as.integer=TRUE))) 
           )
       )
    inteRate[,t,"Interest Rate t/(t-1)|Real ()"] <- 
      (1
       - ( (pm_pvp[,t,] / setYears(pm_pvp[,(which(getYears(pm_pvp)==t)-1),],t)) 
           ^ (1/(getYears(pm_pvp[,t,],as.integer=TRUE)-getYears(pm_pvp[,(which(getYears(pm_pvp)==t)-1),],as.integer=TRUE))) 
       )
      )
  }
  # add interest rate
  out <- mbind(out,inteRate)
  return(out)
}