#' Read in GDX and calculate tax, used in convGDX2MIF.R for the reporting
#' 
#' Read in tax information from GDX file, information used in convGDX2MIF.R for
#' the reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @author Lavinia Baumstark, Christoph Bertram
#' @examples
#' 
#' \dontrun{reportTax(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind getYears setNames dimSums

reportTax <- function(gdx,regionSubsetList=NULL){

  #---- Functions ----
  find_real_module <- function(module_set, module_name){
    return(module_set[module_set$modules == module_name,2])
  }
  #-------------------
  
  # conversion factors
  TWa_2_EJ     <- 31.536
  tdptwyr2dpgj <- 31.71 #"multipl. factor to convert (TerraDollar per TWyear) to (Dollar per GJoule)"
  
  # ---- read in sets -------------------
  module2realisation <- readGDX(gdx, name = c("module2realisation","modules"), format = "first_found")
  
  
  feForUe = readGDX(gdx, name=c('feForUe', 'feForEs'),types = "sets",format="first_found")
  fety35 <- readGDX(gdx, name=c("FE_Transp_fety35"), types = "sets", format="first_found")
    
  all_esty       <- readGDX(gdx,name=c("all_esty"),types="sets",format="first_found", react = "silent")
  if(!is.null(all_esty)){
   fe2es <-  readGDX(gdx, name=c('fe2es'),types = "sets",format="first_found", react = "silent")
  } else {
   fe2es = NULL
  }
  ppfen_stat <- readGDX(gdx,c("ppfen_stationary_dyn38","ppfen_stationary_dyn28","ppfen_stationary"),format="first_found", react = "silent")
  if (length(ppfen_stat) == 0) ppfen_stat = NULL
  ppfen_build <- readGDX(gdx,c("ppfen_buildings_dyn36","ppfen_buildings_dyn28","ppfen_buildings"),format="first_found", react = "silent")
  ue_dyn36 <- readGDX(gdx,c("ue_dyn36"),format="first_found", react = "silent")
  ppfen_build <- setdiff(ppfen_build, ue_dyn36)
  ppfen_ind <- readGDX(gdx,c("ppfen_industry_dyn37","ppfen_industry_dyn28","ppfen_industry"),format="first_found", react = "silent")
  ppfen_stat_build_ind <- c(ppfen_stat,ppfen_build,ppfen_ind)
  
  esty_build <- readGDX(gdx,c("esty_dyn36"),format="first_found", react = "silent")
  esty_trans <- readGDX(gdx,c("esty_dyn35"),format="first_found", react = "silent")
  esty_bt <- c(esty_build,esty_trans )
  
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
  
  
  # ----  read in needed data -------------------
  
  fe_tax  <- readGDX(gdx, name=c('p21_tau_fe_tax','pm_tau_fe_tax'), format="first_found", react = "silent")
  fe_sub  <- readGDX(gdx, name=c('p21_tau_fe_sub','pm_tau_fe_sub'), format="first_found", react = "silent")
  p21_taxrevGHG0 <- readGDX(gdx, name=c("p21_taxrevGHG0"), format= "first_found")
  p21_taxrevCO2luc0 <- readGDX(gdx, name=c("p21_taxrevCO2luc0"), format= "first_found")
  p21_taxrevCCS0 <- readGDX(gdx, name=c("p21_taxrevCCS0"), format= "first_found")
  p21_taxrevNetNegEmi0 <- readGDX(gdx, name=c("p21_taxrevNetNegEmi0"), format= "first_found")
  p21_taxrevFEtrans0 <- readGDX(gdx, name=c("p21_taxrevFEtrans0"), format= "first_found")
  p21_taxrevFEBuildInd0 <- readGDX(gdx, name=c("p21_taxrevFEBuildInd0"), format= "first_found")
  p21_taxrevFE_Es0 <- readGDX(gdx, name=c("p21_taxrevFE_Es0"), format= "first_found", react = "silent")
  p21_taxrevResEx0 <- readGDX(gdx, name=c("p21_taxrevResEx0"), format= "first_found")
  p21_taxrevPE2SE0 <- readGDX(gdx, name=c("p21_taxrevPE2SE0"), format= "first_found")
  p21_taxrevXport0 <- readGDX(gdx, name=c("p21_taxrevXport0"), format= "first_found")
  p21_taxrevSO20 <- readGDX(gdx, name=c("p21_taxrevSO20"), format= "first_found")
  p21_taxrevBio0 <- readGDX(gdx, name=c("p21_taxrevBio0"), format= "first_found")
  
  if(is.null(fe_tax) & is.null(fe_sub)){
    
    fe_tax = mbind(readGDX(gdx, name=c('p21_tau_fe_tax_transport'))[,,fety35],
                   readGDX(gdx, name=c('p21_tau_fe_tax_bit_st'))[,,ppfen_stat_build_ind],
                   readGDX(gdx, name=c("pm_tau_fe_tax_ES_st",'p21_tau_fe_tax_ES_st'), react = "silent")[,,esty_bt])
    fe_sub = mbind(readGDX(gdx, name=c('p21_tau_fe_sub_transport'))[,,fety35],
                   readGDX(gdx, name=c('p21_tau_fe_sub_bit_st'))[,,ppfen_stat_build_ind],
                   readGDX(gdx, name=c('pm_tau_fe_sub_ES_st','p21_tau_fe_sub_ES_st'), react = "silent")[,,esty_bt])
    
    # Some versions of REMIND have B-I-T but no differentiated taxes for Buildings and Industry
    # If they do not differentiate, splitVersion = F
    splitVersion = T
    
  }else{ splitVersion = F}
  
  fuEx_sub  <- readGDX(gdx, name='p21_tau_fuEx_sub')
  prod_FE <- readGDX(gdx, c("vm_prodFe","v_feprod"),field="l", format="first_found",restore_zeros=FALSE,react="silent")
  if(!is.null(fuEx_sub)){fuExtr <- readGDX(gdx, c("vm_fuExtr"),field="l", format="first_found",restore_zeros=FALSE,react="silent")}
  vm_cesIO <- readGDX(gdx, name=c("vm_cesIO"), field="l", restore_zeros=FALSE,format= "first_found")
  vm_demFeForEs <- readGDX(gdx, name=c("vm_demFeForEs"), field="l", restore_zeros=FALSE,format= "first_found", react = "silent")
  vm_co2CCS  <- readGDX(gdx,name=c("vm_co2CCS","v_co2CCS","v_ccs"),field="l",restore_zeros=FALSE,format="first_found", react = "silent")
  vm_emicdrregi  <- readGDX(gdx,name=c("vm_emiCdr","vm_emicdrregi"),field="l",format="first_found")
  vm_emiengregi  <- readGDX(gdx,name=c("vm_emiTe","vm_emiengregi"),field="l",format="first_found")
  vm_sumeminegregi  <- readGDX(gdx,name=c("vm_emiMac","vm_sumeminegregi"),field="l",format="first_found")
  
  # calculate minimal temporal resolution and convert
  
  y <- Reduce(intersect,list(getYears(fe_tax),getYears(fe_sub),getYears(prod_FE)))
  
  fe_tax   <- fe_tax[,y,] * tdptwyr2dpgj
  fe_sub   <- fe_sub[,y,] * tdptwyr2dpgj
  if(!is.null(fuEx_sub)){fuEx_sub   <- fuEx_sub[,y,] * tdptwyr2dpgj}
  prod_FE  <- prod_FE[,y,]*TWa_2_EJ
  if(!is.null(fuEx_sub)){fuExtr   <- fuExtr[,y,]*TWa_2_EJ}
  vm_cesIO <- vm_cesIO[,y,]*TWa_2_EJ
  if (!is.null(vm_demFeForEs)){
    if (length(vm_demFeForEs) > 0 ){
      vm_demFeForEs  <- vm_demFeForEs[,y,][fe2es]*TWa_2_EJ  
    }} 
  p21_taxrevGHG0 <- p21_taxrevGHG0[,y,]
  if (!is.null(p21_taxrevCO2luc0)){
  p21_taxrevCO2luc0 <- p21_taxrevCO2luc0[,y,]
  }
  p21_taxrevCCS0 <- p21_taxrevCCS0[,y,]
  p21_taxrevNetNegEmi0 <- p21_taxrevNetNegEmi0[,y,]
  p21_taxrevFEtrans0 <- p21_taxrevFEtrans0[,y,]
  p21_taxrevFEBuildInd0 <- p21_taxrevFEBuildInd0[,y,]
  p21_taxrevFE_Es0 <- p21_taxrevFE_Es0[,y,]
  p21_taxrevResEx0 <- p21_taxrevResEx0[,y,]
  p21_taxrevPE2SE0 <- p21_taxrevPE2SE0[,y,]
  p21_taxrevXport0 <- p21_taxrevXport0[,y,]
  p21_taxrevSO20 <- p21_taxrevSO20[,y,]
  p21_taxrevBio0 <- p21_taxrevBio0[,y,]
  vm_co2CCS <- vm_co2CCS[,y,]
  vm_emicdrregi <- vm_emicdrregi[,y,]
  vm_emiengregi <- vm_emiengregi[,y,]
  vm_sumeminegregi <- vm_sumeminegregi[,y,]
  
  ### calculate reporting parameters ###
  tmp_trp = NULL
  tmp_stat = NULL
  tmp_buil = NULL
  tmp_indu = NULL
  tmp_b_i = NULL
  
  
  # transport-liquids
  taxrate_trp_liq <- setNames(fe_tax[,,"fepet"],                           "Tax rate|Final Energy|Transportation|Liquids|Oil (US$2005/GJ)")
  subrate_trp_liq <- setNames(fe_sub[,,"fepet"],                           "Subsidy rate|Final Energy|Transportation|Liquids|Oil (US$2005/GJ)")
    # dimSums over dims c(3,4,5) (entySe.entyFe.te) to sum fepet + fedie
  fe_trp_liq  <- setNames(dimSums(prod_FE[,,c("fepet","fedie")],dim=3),"FE|Transportation|Liquids (EJ/yr)")   
  tax_trp_liq <- setNames(fe_trp_liq*taxrate_trp_liq,                      "Taxes|Final Energy|Transportation|Liquids|Oil (billion US$2005/yr)")
  sub_trp_liq <- -setNames(fe_trp_liq*subrate_trp_liq,                     "Subsidies|Final Energy|Transportation|Liquids|Oil (billion US$2005/yr)")
  #transport- electricity
  taxrate_trp_el <- setNames(fe_tax[,,"feelt"],                           "Tax rate|Final Energy|Transportation|Electricity (US$2005/GJ)")
  subrate_trp_el <- setNames(fe_sub[,,"feelt"],                           "Subsidy rate|Final Energy|Transportation|Electricity (US$2005/GJ)")
  fe_trp_el  <- setNames(prod_FE[,,c("feelt")],"FE|Transportation|Electricity (EJ/yr)")   
  tax_trp_el <- setNames(fe_trp_el*taxrate_trp_el,                      "Taxes|Final Energy|Transportation|Electricity (billion US$2005/yr)")
  sub_trp_el <- -setNames(fe_trp_el*subrate_trp_el,                     "Subsidies|Final Energy|Transportation|Electricity (billion US$2005/yr)")
  # transport
  tax_trp     <- setNames(tax_trp_liq + tax_trp_el, "Taxes|Final Energy|Transportation (billion US$2005/yr)")
  sub_trp     <- setNames(sub_trp_liq + sub_trp_el,                                 "Subsidies|Final Energy|Transportation (billion US$2005/yr)")
  
  tmp_trp = mbind(taxrate_trp_liq,subrate_trp_liq,
                  tax_trp_liq,sub_trp_liq,
                  taxrate_trp_el, subrate_trp_el,
                  tax_trp_el, sub_trp_el,
                  tax_trp,sub_trp)
  
  #--- Stationary module or Undifferentiated taxes for Buil and Indu
  
  
  if (stat_mod == "simple" | !splitVersion){
    # stationary-solids
    taxrate_sta_sol <- setNames(fe_tax[,,"fesos"],                       "Tax rate|Final Energy|Stationary|Solids (US$2005/GJ)")
    subrate_sta_sol <- setNames(fe_sub[,,"fesos"],                       "Subsidy rate|Final Energy|Stationary|Solids (US$2005/GJ)")
    tax_sta_sol   <- setNames(prod_FE[,,"fesos"]*taxrate_sta_sol,        "Taxes|Final Energy|Stationary|Solids (billion US$2005/yr)")
    sub_sta_sol     <- -setNames(prod_FE[,,"fesos"]*subrate_sta_sol,     "Subsidies|Final Energy|Stationary|Solids (billion US$2005/yr)")
    # stationary-liquids
    taxrate_sta_liq <- setNames(fe_tax[,,"fehos"],                       "Tax rate|Final Energy|Stationary|Liquids (US$2005/GJ)")     
    subrate_sta_liq <- setNames(fe_sub[,,"fehos"],                       "Subsidy rate|Final Energy|Stationary|Liquids (US$2005/GJ)")
    tax_sta_liq     <- setNames(prod_FE[,,"fehos"]*taxrate_sta_liq,      "Taxes|Final Energy|Stationary|Liquids (billion US$2005/yr)")
    sub_sta_liq     <- -setNames(prod_FE[,,"fehos"]*subrate_sta_liq,     "Subsidies|Final Energy|Stationary|Liquids (billion US$2005/yr)")
    # stationary-gases  
    taxrate_sta_gas <- setNames(fe_tax[,,"fegas"],                       "Tax rate|Final Energy|Stationary|Gases (US$2005/GJ)")
    subrate_sta_gas <- setNames(fe_sub[,,"fegas"],                       "Subsidy rate|Final Energy|Stationary|Gases (US$2005/GJ)")
    tax_sta_gas   <- setNames(prod_FE[,,"fegas"]*taxrate_sta_gas,        "Taxes|Final Energy|Stationary|Gases (billion US$2005/yr)")
    sub_sta_gas    <- -setNames(prod_FE[,,"fegas"]*subrate_sta_gas,      "Subsidies|Final Energy|Stationary|Gases (billion US$2005/yr)")
    # stationary-electricity 
    taxrate_sta_ele <- setNames(fe_tax[,,"feels"],                       "Tax rate|Final Energy|Stationary|Electricity (US$2005/GJ)")
    subrate_sta_ele <- setNames(fe_sub[,,"feels"],                       "Subsidy rate|Final Energy|Stationary|Electricity (US$2005/GJ)")
    tax_sta_ele     <- setNames(prod_FE[,,"feels"]*taxrate_sta_ele,      "Taxes|Final Energy|Stationary|Electricity (billion US$2005/yr)")   
    sub_sta_ele    <- -setNames(prod_FE[,,"feels"]*subrate_sta_ele,      "Subsidies|Final Energy|Stationary|Electricity (billion US$2005/yr)") 
    
    # stationary
    tax_sta <- setNames( tax_sta_sol+tax_sta_liq + setNames(tax_sta_gas,NULL)
                         + setNames(tax_sta_ele,NULL),                     "Taxes|Final Energy|Stationary (billion US$2005/yr)")
    sub_sta  <- setNames(sub_sta_sol+sub_sta_liq + setNames(sub_sta_gas,NULL)
                         + setNames(sub_sta_ele,NULL),                     "Subsidies|Final Energy|Stationary (billion US$2005/yr)")
    
    
    # Bind all Stationary
    tmp_stat = mbind(taxrate_sta_sol,subrate_sta_sol,tax_sta_sol,sub_sta_sol,
                     taxrate_sta_liq,subrate_sta_liq,tax_sta_liq,sub_sta_liq,
                     taxrate_sta_gas,subrate_sta_gas,tax_sta_gas,sub_sta_gas,
                     taxrate_sta_ele,subrate_sta_ele,tax_sta_ele,sub_sta_ele,
                     tax_sta,sub_sta)
    
    
  } 
  
  if (splitVersion){
    
    #---- Buildings Module
    
    if (buil_mod == "simple"){
      
      # buildings-solids
      taxrate_sta_sol_b <- setNames(fe_tax[,,"fesob"],                       "Tax rate|Final Energy|Buildings|Solids (US$2005/GJ)")
      subrate_sta_sol_b <- setNames(fe_sub[,,"fesob"],                       "Subsidy rate|Final Energy|Buildings|Solids (US$2005/GJ)")
      tax_sta_sol_b   <- setNames(vm_cesIO[,,"fesob"]*taxrate_sta_sol_b,        "Taxes|Final Energy|Buildings|Solids (billion US$2005/yr)")
      sub_sta_sol_b     <- -setNames(vm_cesIO[,,"fesob"]*subrate_sta_sol_b,     "Subsidies|Final Energy|Buildings|Solids (billion US$2005/yr)")
      # buildings-liquids
      taxrate_sta_liq_b <- setNames(fe_tax[,,"fehob"],                       "Tax rate|Final Energy|Buildings|Liquids (US$2005/GJ)")     
      subrate_sta_liq_b <- setNames(fe_sub[,,"fehob"],                       "Subsidy rate|Final Energy|Buildings|Liquids (US$2005/GJ)")
      tax_sta_liq_b     <- setNames(vm_cesIO[,,"fehob"]*taxrate_sta_liq_b,      "Taxes|Final Energy|Buildings|Liquids (billion US$2005/yr)")
      sub_sta_liq_b     <- -setNames(vm_cesIO[,,"fehob"]*subrate_sta_liq_b,     "Subsidies|Final Energy|Buildings|Liquids (billion US$2005/yr)")
      # buildings-gases  
      taxrate_sta_gas_b <- setNames(fe_tax[,,"fegab"],                       "Tax rate|Final Energy|Buildings|Gases (US$2005/GJ)")
      subrate_sta_gas_b <- setNames(fe_sub[,,"fegab"],                       "Subsidy rate|Final Energy|Buildings|Gases (US$2005/GJ)")
      tax_sta_gas_b   <- setNames(vm_cesIO[,,"fegab"]*taxrate_sta_gas_b,        "Taxes|Final Energy|Buildings|Gases (billion US$2005/yr)")
      sub_sta_gas_b    <- -setNames(vm_cesIO[,,"fegab"]*subrate_sta_gas_b,      "Subsidies|Final Energy|Buildings|Gases (billion US$2005/yr)")
      # buildings-electricity 
      taxrate_sta_ele_b <- setNames(fe_tax[,,"feelb"],                       "Tax rate|Final Energy|Buildings|Electricity (US$2005/GJ)")
      subrate_sta_ele_b <- setNames(fe_sub[,,"feelb"],                       "Subsidy rate|Final Energy|Buildings|Electricity (US$2005/GJ)")
      tax_sta_ele_b     <- setNames(vm_cesIO[,,"feelb"]*taxrate_sta_ele_b,      "Taxes|Final Energy|Buildings|Electricity (billion US$2005/yr)")   
      sub_sta_ele_b    <- -setNames(vm_cesIO[,,"feelb"]*subrate_sta_ele_b,   "Subsidies|Final Energy|Buildings|Electricity (billion US$2005/yr)")
      
      # buildings total
      tax_sta_b     <- setNames(tax_sta_sol_b + tax_sta_liq_b + tax_sta_gas_b + tax_sta_ele_b   ,"Taxes|Final Energy|Buildings (billion US$2005/yr)")
      sub_sta_b     <- setNames(sub_sta_sol_b + sub_sta_liq_b + sub_sta_gas_b + sub_sta_ele_b   ,"Subsidies|Final Energy|Buildings (billion US$2005/yr)")
      taxrate_sta_b <- setNames(tax_sta_b/ dimSums(vm_cesIO[,,c("fesob","fehob","fegab","feelb","feheb","feh2b")], dim =3),"Tax rate|Final Energy|Buildings (US$2005/GJ)")
      subrate_sta_b <- setNames(sub_sta_b/ dimSums(vm_cesIO[,,c("fesob","fehob","fegab","feelb","feheb","feh2b")], dim =3),"Subsidy rate|Final Energy|Buildings (US$2005/GJ)")
      
      #Bind all buildings
      tmp_buil = mbind(taxrate_sta_sol_b,subrate_sta_sol_b,tax_sta_sol_b,sub_sta_sol_b,
                       taxrate_sta_liq_b,subrate_sta_liq_b,tax_sta_liq_b,sub_sta_liq_b,
                       taxrate_sta_gas_b,subrate_sta_gas_b,tax_sta_gas_b,sub_sta_gas_b,
                       taxrate_sta_ele_b,subrate_sta_ele_b,tax_sta_ele_b,sub_sta_ele_b,
                       tax_sta_b,sub_sta_b,taxrate_sta_b,subrate_sta_b )
    }
    
    if (buil_mod %in% c("services_putty", "services_with_capital")){
      
      
      ces_elec = c(grep("elb$", ppfen_build, value = T),grep("hpb$", ppfen_build, value = T))
      es_elec = c(grep("elb$", esty_build, value = T),grep("hpb$", esty_build, value = T))
      es_solids = c(grep("sob$", esty_build, value = T), grep("stb$", esty_build, value = T))
      es_gas = grep("gab$", esty_build, value = T)
      es_liq = grep("hob$", esty_build, value = T)
      es_heat = grep("heb$", esty_build, value = T)
      es_hydro = grep("h2b$", esty_build, value = T)
      
      # buildings-solids
      taxrate_sta_sol_b <- setNames(fe_tax[,,es_solids[1]],                       "Tax rate|Final Energy|Buildings|Solids (US$2005/GJ)")
      subrate_sta_sol_b <- setNames(fe_sub[,,es_solids[1]],                       "Subsidy rate|Final Energy|Buildings|Solids (US$2005/GJ)")
      tax_sta_sol_b   <- setNames(dimSums(vm_demFeForEs[,,es_solids],dim = 3)*taxrate_sta_sol_b,        "Taxes|Final Energy|Buildings|Solids (billion US$2005/yr)")
      sub_sta_sol_b     <- -setNames(dimSums(vm_demFeForEs[,,es_solids],dim = 3)*subrate_sta_sol_b,     "Subsidies|Final Energy|Buildings|Solids (billion US$2005/yr)")
      # buildings-liquids
      taxrate_sta_liq_b <- setNames(fe_tax[,,es_liq[1]],                       "Tax rate|Final Energy|Buildings|Liquids (US$2005/GJ)")     
      subrate_sta_liq_b <- setNames(fe_sub[,,es_liq[1]],                       "Subsidy rate|Final Energy|Buildings|Liquids (US$2005/GJ)")
      tax_sta_liq_b     <- setNames(dimSums(vm_demFeForEs[,,es_liq],dim = 3)*taxrate_sta_liq_b,      "Taxes|Final Energy|Buildings|Liquids (billion US$2005/yr)")
      sub_sta_liq_b     <- -setNames(dimSums(vm_demFeForEs[,,es_liq],dim = 3)*subrate_sta_liq_b,     "Subsidies|Final Energy|Buildings|Liquids (billion US$2005/yr)")
      # buildings-gases  
      taxrate_sta_gas_b <- setNames(fe_tax[,,es_gas[1]],                       "Tax rate|Final Energy|Buildings|Gases (US$2005/GJ)")
      subrate_sta_gas_b <- setNames(fe_sub[,,es_gas[1]],                       "Subsidy rate|Final Energy|Buildings|Gases (US$2005/GJ)")
      tax_sta_gas_b   <- setNames(dimSums(vm_demFeForEs[,,es_gas],dim = 3)*taxrate_sta_gas_b,        "Taxes|Final Energy|Buildings|Gases (billion US$2005/yr)")
      sub_sta_gas_b    <- -setNames(dimSums(vm_demFeForEs[,,es_gas],dim = 3)*subrate_sta_gas_b,      "Subsidies|Final Energy|Buildings|Gases (billion US$2005/yr)")
      # buildings-electricity 
      taxrate_sta_ele_b <- setNames(fe_tax[,,es_elec[1]],                       "Tax rate|Final Energy|Buildings|Electricity (US$2005/GJ)")
      subrate_sta_ele_b <- setNames(fe_sub[,,es_elec[1]],                       "Subsidy rate|Final Energy|Buildings|Electricity (US$2005/GJ)")
      tax_sta_ele_b     <- setNames((dimSums(vm_cesIO[,,ces_elec],dim = 3)
                                     + dimSums(vm_demFeForEs[,,es_elec],dim = 3)) *taxrate_sta_ele_b,      "Taxes|Final Energy|Buildings|Electricity (billion US$2005/yr)")   
      sub_sta_ele_b    <- -setNames((dimSums(vm_cesIO[,,ces_elec],dim = 3)
                                     + dimSums(vm_demFeForEs[,,es_elec],dim = 3))*subrate_sta_ele_b,   "Subsidies|Final Energy|Buildings|Electricity (billion US$2005/yr)")
      
      # buildings total
      tax_sta_b     <- setNames(tax_sta_sol_b + tax_sta_liq_b + tax_sta_gas_b + tax_sta_ele_b   ,"Taxes|Final Energy|Buildings (billion US$2005/yr)")
      sub_sta_b     <- setNames(sub_sta_sol_b + sub_sta_liq_b + sub_sta_gas_b + sub_sta_ele_b   ,"Subsidies|Final Energy|Buildings (billion US$2005/yr)")
      taxrate_sta_b <- setNames(tax_sta_b/ (dimSums(vm_cesIO[,,ppfen_build], dim =3)
                                           + dimSums(vm_demFeForEs[,,esty_build], dim = 3)),"Tax rate|Final Energy|Buildings (US$2005/GJ)")
      subrate_sta_b <- setNames(sub_sta_b/ (dimSums(vm_cesIO[,,ppfen_build], dim =3)
                                            + dimSums(vm_demFeForEs[,,esty_build], dim = 3)),"Subsidy rate|Final Energy|Buildings (US$2005/GJ)")
      
      #Bind all buildings
      tmp_buil = mbind(taxrate_sta_sol_b,subrate_sta_sol_b,tax_sta_sol_b,sub_sta_sol_b,
                       taxrate_sta_liq_b,subrate_sta_liq_b,tax_sta_liq_b,sub_sta_liq_b,
                       taxrate_sta_gas_b,subrate_sta_gas_b,tax_sta_gas_b,sub_sta_gas_b,
                       taxrate_sta_ele_b,subrate_sta_ele_b,tax_sta_ele_b,sub_sta_ele_b,
                       tax_sta_b,sub_sta_b,taxrate_sta_b,subrate_sta_b )
    }
    
    #---- Industry Module
    
    if (indu_mod %in% c('fixed_shares', 'subsectors')){
      # make a list of industry FE carriers
      ppfen_ind_group <- list()
      
      ppfen_ind_group$solids      <- grep('^(feso_|fesoi$)', ppfen_ind, value = TRUE)
      ppfen_ind_group$liquids     <- grep('^(feli_|fehoi$)', ppfen_ind, value = TRUE)
      ppfen_ind_group$gases       <- grep('^(fega_|fegai$)', ppfen_ind, value = TRUE)
      ppfen_ind_group$hydrogen    <- grep('^(feh2_|feh2i$)', ppfen_ind, value = TRUE)
      ppfen_ind_group$heat        <- grep('^(fehe_|fehei$)', ppfen_ind, value = TRUE)
      ppfen_ind_group$electricity <- grep('^(feel_|feeli$)', ppfen_ind, value = TRUE)
      
      tmp_indu <- NULL
      
      for (fe in c('solids', 'liquids', 'gases', 'hydrogen', 'heat', 
                   'electricity')) {      # for all FE carriers
        for (quan in c('tax', 'sub')) {   # for taxes and subsidies
          
          tmp_indu <- mbind(
            tmp_indu,
            
            # compute taxes/subsidies as sum of rates times energy use
            setNames(
              dimSums(
                ( get(paste0('fe_', quan))[,,ppfen_ind_group[[fe]]]
                * vm_cesIO[,,ppfen_ind_group[[fe]]]
                ), dim = 3
              ),
              paste0(ifelse('sub' == quan, 'Subsidies', 'Taxes'),
                     '|Final Energy|Industry|', tools::toTitleCase(fe),
                     ' (billion US$2005/yr)')
            ),
            
            # compute rates as average rate weighted by energy use
            setNames(
              ( dimSums(
                ( get(paste0('fe_', quan))[,,ppfen_ind_group[[fe]]]
                * vm_cesIO[,,ppfen_ind_group[[fe]]]
                ), dim = 3)
              / dimSums(vm_cesIO[,,ppfen_ind_group[[fe]]], dim = 3)
              ),
              paste0(ifelse('sub' == quan, 'Subsidy', 'Tax'), ' rate',
                     '|Final Energy|Industry|', tools::toTitleCase(fe),
                     ' (US$2005/GJ)')
            )
          )
        }
      }
      
      # compute totals and overall averages
      tmp_indu <- mbind(
        tmp_indu,
        
        setNames(
          dimSums(tmp_indu[,,grep('Taxes', getNames(tmp_indu))], dim = 3),
          'Taxes|Final Energy|Industry (billion US$2005/yr)'),
        
        setNames(
          dimSums(tmp_indu[,,grep('Subsidies', getNames(tmp_indu))], dim = 3),
          'Subsidies|Final Energy|Industry (billion US$2005/yr)'),
        
        setNames(
          ( dimSums(tmp_indu[,,grep('Taxes', getNames(tmp_indu))], dim = 3)
          / dimSums(vm_cesIO[,,ppfen_ind], dim = 3)
          ),
          'Tax rate|Final Energy|Industry (US$2005/GJ)'),
        
        setNames(
          ( dimSums(tmp_indu[,,grep('Subsidies', getNames(tmp_indu))], dim = 3)
          / dimSums(vm_cesIO[,,ppfen_ind], dim = 3)
          ),
          'Subsidy rate|Final Energy|Industry (US$2005/GJ)')
      )
    }
    
    #--- Join Buildings and Industry
    if (!('off' == buil_mod & 'off' == indu_mod)) {
      tmp_b_i = mbind(tmp_buil,tmp_indu)
      
      #taxes
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings|Solids (billion US$2005/yr)"] + tmp_b_i[,,"Taxes|Final Energy|Industry|Solids (billion US$2005/yr)"],
                               "Taxes|Final Energy|Buildings and Industry|Solids (billion US$2005/yr)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings|Liquids (billion US$2005/yr)"] + tmp_b_i[,,"Taxes|Final Energy|Industry|Liquids (billion US$2005/yr)"],
                               "Taxes|Final Energy|Buildings and Industry|Liquids (billion US$2005/yr)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings|Gases (billion US$2005/yr)"] + tmp_b_i[,,"Taxes|Final Energy|Industry|Gases (billion US$2005/yr)"],
                               "Taxes|Final Energy|Buildings and Industry|Gases (billion US$2005/yr)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings|Electricity (billion US$2005/yr)"] + tmp_b_i[,,"Taxes|Final Energy|Industry|Electricity (billion US$2005/yr)"],
                               "Taxes|Final Energy|Buildings and Industry|Electricity (billion US$2005/yr)"))
      
      #subsidies
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings|Solids (billion US$2005/yr)"] + tmp_b_i[,,"Subsidies|Final Energy|Industry|Solids (billion US$2005/yr)"],
                               "Subsidies|Final Energy|Buildings and Industry|Solids (billion US$2005/yr)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings|Liquids (billion US$2005/yr)"] + tmp_b_i[,,"Subsidies|Final Energy|Industry|Liquids (billion US$2005/yr)"],
                               "Subsidies|Final Energy|Buildings and Industry|Liquids (billion US$2005/yr)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings|Gases (billion US$2005/yr)"] + tmp_b_i[,,"Subsidies|Final Energy|Industry|Gases (billion US$2005/yr)"],
                               "Subsidies|Final Energy|Buildings and Industry|Gases (billion US$2005/yr)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings|Electricity (billion US$2005/yr)"] + tmp_b_i[,,"Subsidies|Final Energy|Industry|Electricity (billion US$2005/yr)"],
                               "Subsidies|Final Energy|Buildings and Industry|Electricity (billion US$2005/yr)"))
      
      #totals
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings (billion US$2005/yr)"] + tmp_b_i[,,"Taxes|Final Energy|Industry (billion US$2005/yr)"],
                               "Taxes|Final Energy|Buildings and Industry (billion US$2005/yr)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings (billion US$2005/yr)"] + tmp_b_i[,,"Subsidies|Final Energy|Industry (billion US$2005/yr)"],
                               "Subsidies|Final Energy|Buildings and Industry (billion US$2005/yr)"))
      
      #tax rates
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings and Industry|Solids (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Taxes|Final Energy|Buildings|Solids (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Buildings|Solids (US$2005/GJ)"]
                                  + tmp_b_i[,,"Taxes|Final Energy|Industry|Solids (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Industry|Solids (US$2005/GJ)"]
                               ),
                               "Tax rate|Final Energy|Buildings and Industry|Solids (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings and Industry|Liquids (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Taxes|Final Energy|Buildings|Liquids (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Buildings|Liquids (US$2005/GJ)"]
                                  + tmp_b_i[,,"Taxes|Final Energy|Industry|Liquids (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Industry|Liquids (US$2005/GJ)"]
                               ),
                               "Tax rate|Final Energy|Buildings and Industry|Liquids (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings and Industry|Gases (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Taxes|Final Energy|Buildings|Gases (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Buildings|Gases (US$2005/GJ)"]
                                  + tmp_b_i[,,"Taxes|Final Energy|Industry|Gases (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Industry|Gases (US$2005/GJ)"]
                               ),
                               "Tax rate|Final Energy|Buildings and Industry|Gases (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings and Industry|Electricity (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Taxes|Final Energy|Buildings|Electricity (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Buildings|Electricity (US$2005/GJ)"]
                                  + tmp_b_i[,,"Taxes|Final Energy|Industry|Electricity (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Industry|Electricity (US$2005/GJ)"]
                               ),
                               "Tax rate|Final Energy|Buildings and Industry|Electricity (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Taxes|Final Energy|Buildings and Industry (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Taxes|Final Energy|Buildings (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Buildings (US$2005/GJ)"]
                                  + tmp_b_i[,,"Taxes|Final Energy|Industry (billion US$2005/yr)"] /tmp_b_i[,,"Tax rate|Final Energy|Industry (US$2005/GJ)"]
                               ),
                               "Tax rate|Final Energy|Buildings and Industry (US$2005/GJ)"))
      
      #subsidy rates
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings and Industry|Solids (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Subsidies|Final Energy|Buildings|Solids (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Buildings|Solids (US$2005/GJ)"]
                                  + tmp_b_i[,,"Subsidies|Final Energy|Industry|Solids (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Industry|Solids (US$2005/GJ)"]
                               ),
                               "Subsidy rate|Final Energy|Buildings and Industry|Solids (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings and Industry|Liquids (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Subsidies|Final Energy|Buildings|Liquids (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Buildings|Liquids (US$2005/GJ)"]
                                  + tmp_b_i[,,"Subsidies|Final Energy|Industry|Liquids (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Industry|Liquids (US$2005/GJ)"]
                               ),
                               "Subsidy rate|Final Energy|Buildings and Industry|Liquids (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings and Industry|Gases (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Subsidies|Final Energy|Buildings|Gases (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Buildings|Gases (US$2005/GJ)"]
                                  + tmp_b_i[,,"Subsidies|Final Energy|Industry|Gases (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Industry|Gases (US$2005/GJ)"]
                               ),
                               "Subsidy rate|Final Energy|Buildings and Industry|Gases (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings and Industry|Electricity (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Subsidies|Final Energy|Buildings|Electricity (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Buildings|Electricity (US$2005/GJ)"]
                                  + tmp_b_i[,,"Subsidies|Final Energy|Industry|Electricity (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Industry|Electricity (US$2005/GJ)"]
                               ),
                               "Subsidy rate|Final Energy|Buildings and Industry|Electricity (US$2005/GJ)"))
      tmp_b_i = mbind(tmp_b_i,
                      setNames(tmp_b_i[,,"Subsidies|Final Energy|Buildings and Industry (billion US$2005/yr)"]
                               / (tmp_b_i[,,"Subsidies|Final Energy|Buildings (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Buildings (US$2005/GJ)"]
                                  + tmp_b_i[,,"Subsidies|Final Energy|Industry (billion US$2005/yr)"] /tmp_b_i[,,"Subsidy rate|Final Energy|Industry (US$2005/GJ)"]
                               ),
                               "Subsidy rate|Final Energy|Buildings and Industry (US$2005/GJ)"))
    }
  }

  # put all together
  out <- mbind(tmp_trp, tmp_stat, tmp_b_i)
  
  if (stat_mod == "simple" | !splitVersion) {
    
    out = mbind(out, setNames(out[,,"Taxes|Final Energy|Transportation (billion US$2005/yr)"]
                              + out[,,"Taxes|Final Energy|Stationary (billion US$2005/yr)"],
                              "Taxes|Final Energy (billion US$2005/yr)"))
    
    out = mbind(out, setNames(out[,,"Subsidies|Final Energy|Transportation (billion US$2005/yr)"]
                              + out[,,"Subsidies|Final Energy|Stationary (billion US$2005/yr)"],
                              "Subsidies|Final Energy (billion US$2005/yr)"))
    
  }else {
    
    out = mbind(out, setNames(out[,,"Taxes|Final Energy|Transportation (billion US$2005/yr)"]
                              + out[,,"Taxes|Final Energy|Buildings and Industry (billion US$2005/yr)"],
                              "Taxes|Final Energy (billion US$2005/yr)"))
    
    out = mbind(out, setNames(out[,,"Subsidies|Final Energy|Transportation (billion US$2005/yr)"]
                              + out[,,"Subsidies|Final Energy|Buildings and Industry (billion US$2005/yr)"],
                              "Subsidies|Final Energy (billion US$2005/yr)"))
    
    
  }
  
  
  #ressource extraction subsidies
  if (!is.null(fuEx_sub)){
    out <- mbind(out,
                 setNames(fuEx_sub[,,"pecoal"],"Subsidy Rate|Fuel Extraction|Coal (US$2005/GJ)"),
                 setNames(fuEx_sub[,,"peoil"],"Subsidy Rate|Fuel Extraction|Oil (US$2005/GJ)"),
                 setNames(fuEx_sub[,,"pegas"],"Subsidy Rate|Fuel Extraction|Natural Gas (US$2005/GJ)"),
                 -setNames(fuEx_sub[,,"pecoal"]* dimSums(fuExtr[,,"pecoal"],dim=3, na.rm=T),"Subsidies|Fuel Extraction|Coal (billion US$2005/yr)"),
                 -setNames(fuEx_sub[,,"peoil"]* dimSums(fuExtr[,,"peoil"],dim=3, na.rm=T),"Subsidies|Fuel Extraction|Oil (billion US$2005/yr)"),
                 -setNames(fuEx_sub[,,"pegas"]* dimSums(fuExtr[,,"pegas"],dim=3, na.rm=T),"Subsidies|Fuel Extraction|Natural Gas (billion US$2005/yr)"),
                 -setNames(fuEx_sub[,,"pecoal"]* dimSums(fuExtr[,,"pecoal"],dim=3, na.rm=T) + 
                             fuEx_sub[,,"peoil"]* dimSums(fuExtr[,,"peoil"],dim=3, na.rm=T) +
                             fuEx_sub[,,"pegas"]* dimSums(fuExtr[,,"pegas"],dim=3, na.rm=T),"Subsidies|Fuel Extraction (billion US$2005/yr)"),
                 setNames(out[,,"Subsidies|Final Energy (billion US$2005/yr)"]-fuEx_sub[,,"pecoal"]* dimSums(fuExtr[,,"pecoal"],dim=3, na.rm=T) - 
                            fuEx_sub[,,"peoil"]* dimSums(fuExtr[,,"peoil"],dim=3, na.rm=T) -
                            fuEx_sub[,,"pegas"]* dimSums(fuExtr[,,"pegas"],dim=3, na.rm=T),"Subsidies (billion US$2005/yr)")
    )
  }
  
  #tax levels of all taxes in equation q21_taxrev
  if (!is.null(p21_taxrevGHG0)){
    if (!is.null(p21_taxrevCO2luc0)){
      #note: CO2 from LUC is now treated separately in tax revenue recycling
      #total GHG tax revenue = taxrevGHG0 + taxrevCO2luc0
    out <- mbind(out,
                 setNames(p21_taxrevGHG0*1000,"Taxes|GHG emissions w/o CO2 LUC|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevCO2luc0*1000,"Taxes|CO2 LUC|GAMS calculated (billion US$2005/yr)"),
                 setNames((p21_taxrevGHG0+p21_taxrevCO2luc0)*1000,"Taxes|GHG emissions|GAMS calculated (billion US$2005/yr)")
    )
    }else{
      out <- mbind(out,
                   setNames(p21_taxrevGHG0*1000,"Taxes|GHG emissions|GAMS calculated (billion US$2005/yr)")
                   )
    }
    out <- mbind(out,
                 setNames(p21_taxrevCCS0*1000,"Taxes|CCS|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevNetNegEmi0*1000,"Taxes|Net-negative emissions|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevFEtrans0*1000,"Taxes|Final Energy|Transportation II|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevFEBuildInd0*1000,"Taxes|Final Energy|Buildings and Industry I|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevResEx0*1000,"Taxes|Fuel Extraction|Fossils|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevPE2SE0*1000,"Taxes|PE2SE Technologies|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevXport0*1000,"Taxes|Exports|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevSO20*1000,"Taxes|SO2|GAMS calculated (billion US$2005/yr)"),
                 setNames(p21_taxrevBio0*1000,"Taxes|Bioenergy|GAMS calculated (billion US$2005/yr)")
                 )
    out <- mbind(out,
                 setNames(out[,,"Taxes|CCS|GAMS calculated (billion US$2005/yr)"]/vm_co2CCS[,,"cco2.ico2.ccsinje.1"]*12/44,"Tax rate|CCS|GAMS calculated (US$2005/t CO2)"),
                 setNames(out[,,"Taxes|Net-negative emissions|GAMS calculated (billion US$2005/yr)"]/(vm_emiengregi[,,"co2"] + vm_sumeminegregi[,,"co2"] + vm_emicdrregi[,,"co2"])*12/44*(-1),"Tax rate|Net-negative emissions|GAMS calculated (US$2005/t CO2)")
    )
  }
  
  if (!is.null(p21_taxrevFE_Es0)){
    out <- mbind(out,
                 setNames(p21_taxrevFE_Es0*1000,"Taxes|Final Energy|Buildings and Transportation III|GAMS calculated (billion US$2005/yr)"))
  }
                 

  # add global values
  out <- mbind(out,dimSums(out,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))
  
  return(out)
}
