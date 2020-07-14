#' Read in GDX and calculate final energy, used in convGDX2MIF.R for the
#' reporting
#' 
#' Read in final energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @author Christoph Bertram, Antoine Levesque
#' @examples
#' 
#'   \dontrun{reportPE(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie mselect getRegions getYears mbind setNames 
#'                      dimSums getNames<- as.data.frame
#' @importFrom dplyr filter_ %>% mutate select inner_join group_by summarise 
#'                   ungroup rename
#' @importFrom quitte inline.data.frame

reportFE <- function(gdx,regionSubsetList=NULL) {
  #---- Functions ----
  find_real_module <- function(module_set, module_name){
    return(module_set[module_set$modules == module_name,2])
  }
  

  ####### conversion factors ##########
  TWa_2_EJ     <- 31.536
  ####### read in needed data #########
  ## sets
  module2realisation <- readGDX(gdx, "module2realisation", react = "silent")
  FE_Transp_fety35    <- readGDX(gdx,"FE_Transp_fety35")
  FE_Elec_fety35    <- readGDX(gdx,"FE_Elec_fety35")
  sety <- readGDX(gdx,c("entySe","sety"),format="first_found")
  
  # the set liquids changed from sepet+sedie to seLiq in REMIND 1.7. Seliq, sega and seso changed to include biomass or Fossil origin after REMIND 2.0
  se_Liq    <- intersect(c("seliqfos", "seliqbio", "seliq", "sepet","sedie"),sety)
  se_Gas    <- intersect(c("segafos", "segabio", "sega"),sety)
  se_Solids <- intersect(c("sesofos", "sesobio", "seso"),sety)
  
  se2fe <- readGDX(gdx,"se2fe")
  FE_H2_fety <- c("feh2s", "feh2t")
  pe2se <- readGDX(gdx, "pe2se")
  FE_Stat_fety <- c("fegas", "fehos", "fesos", "feh2s", "fehes", "feels") 
  
  esty <- readGDX(gdx,c("entyUe", "entyEs", "esty"),format="first_found")
  fety <- readGDX(gdx,c("entyFe","fety"),format="first_found")
  te <- readGDX(gdx,"te")
  pebio <- readGDX(gdx,c("peBio","pebio"),format="first_found")
  t2100 <- c(seq(2005,2060,by = 5), seq(2070,2110, by = 10),2130,2150)
  
  ppfen_stat <- readGDX(gdx,c("ppfen_stationary_dyn38","ppfen_stationary_dyn28","ppfen_stationary"),format="first_found", react = "silent")
  if (length(ppfen_stat) == 0) ppfen_stat = NULL
  ppfen_build <- readGDX(gdx,c("ppfen_buildings_dyn36","ppfen_buildings_dyn28","ppfen_buildings"),format="first_found", react = "silent")
  ppfen_ind <- readGDX(gdx,c("ppfen_industry_dyn37","ppfen_industry_dyn28","ppfen_industry"),format="first_found", react = "silent")
  ppfen_stat_build_ind <- c(ppfen_stat,ppfen_build,ppfen_ind)
  
  esty_build <-  readGDX(gdx,c("esty_dyn36"),format="first_found", react = "silent")
  all_esty       <- readGDX(gdx,name=c("all_esty"),types="sets",format="first_found", react = "silent")
  if(!is.null(all_esty)){
  fe2es <- readGDX(gdx,c("fe2es"),format="first_found", react = "silent")
  } else {
    fe2es = NULL
  }
  
  rlf <- readGDX(gdx,"rlf")
  
  ## parameter
  p_eta_conv = readGDX(gdx, c("pm_eta_conv","p_eta_conv"), restore_zeros = FALSE,format="first_found")
  pm_cesdata = readGDX(gdx,"pm_cesdata")
  s33_rockgrind_fedem <- readGDX(gdx,"s33_rockgrind_fedem", react = "silent")
  if (is.null(s33_rockgrind_fedem)){
    s33_rockgrind_fedem  <- new.magpie("GLO",NULL,fill=0)
  }
  
  ## variables
  prodFE  <- readGDX(gdx,name=c("vm_prodFe"),field="l",restore_zeros=FALSE,format="first_found")*TWa_2_EJ
  prodFE  <- prodFE[se2fe]
  prodSE <- readGDX(gdx,name=c("vm_prodSe","v_seprod"),field="l",restore_zeros=FALSE,format="first_found")*TWa_2_EJ
  prodSE <- mselect(prodSE,all_enty1=sety)
  vm_cesIO <- readGDX(gdx, name=c("vm_cesIO"), field="l", restore_zeros=FALSE,format= "first_found")*TWa_2_EJ
  vm_otherFEdemand  <- readGDX(gdx,name=c("vm_otherFEdemand"),field="l",format="first_found")*TWa_2_EJ
  vm_demFeForEs <- readGDX(gdx,name = c("vm_demFeForEs"), field="l", restore_zeros=FALSE,format= "first_found",react = "silent")*TWa_2_EJ
  v_prodEs <- readGDX(gdx,name = c("v_prodEs"), field="l",restore_zeros = F, format = "first_found", react = "silent") * TWa_2_EJ

  v33_grindrock_onfield  <- readGDX(gdx,name=c("v33_grindrock_onfield"),field="l",format="first_found",react = "silent")
  if (is.null(v33_grindrock_onfield)){
    v33_grindrock_onfield  <- new.magpie(getRegions(vm_otherFEdemand),getYears(vm_otherFEdemand),rlf,fill=0)
  }
  
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
    tran_mod = "complex"
  }

  ####### calculate minimal temporal resolution #####
  y <- Reduce(intersect,list(getYears(prodFE),getYears(prodSE)))
  prodFE  <- prodFE[,y,]
  prodSE <- prodSE[,y,]
  vm_cesIO <- vm_cesIO[,y,]
  if (0 < length(v_prodEs)) v_prodEs <- v_prodEs[,y,]
  vm_otherFEdemand <- vm_otherFEdemand[,y,]
  v33_grindrock_onfield<- v33_grindrock_onfield[,y,]
  
  ######## compute sets for summation below ######
  setSolBio <- dplyr::filter_(pe2se, ~all_enty %in% pebio, ~all_enty1 %in% se_Solids)

  ####### calculate reporting parameters ############
  tmp0 <- NULL

  if (any(grep('\\.offset_quantity$', getNames(pm_cesdata)))) {
    # Correct for offset quantities in the transition between ESM and CES for zero quantities
    pf <- paste0(getNames(vm_cesIO), '.offset_quantity')
    offset <- collapseNames(pm_cesdata[,,pf]) * TWa_2_EJ
    vm_cesIO = vm_cesIO + offset[,y,getNames(vm_cesIO)]
  }
  
  #--- Stationary Module ---
  if (stat_mod == "simple"){
    tmp0 <- mbind(tmp0,
                  setNames((dimSums(vm_cesIO[,,"feels"],dim=3) + vm_otherFEdemand[,,"feels"]),          "FE|Stationary|Electricity (EJ/yr)"),
                  setNames((dimSums(vm_cesIO[,,"fegas"],dim=3) + vm_otherFEdemand[,,"fegas"]),          "FE|Stationary|Gases (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"fesos"],dim=3),          "FE|Stationary|Solids (EJ/yr)"),
                  setNames((dimSums(vm_cesIO[,,"fehos"],dim=3) + vm_otherFEdemand[,,"fedie"]),          "FE|Stationary|Liquids (EJ/yr)"),
                  setNames((dimSums(vm_cesIO[,,"feh2s"],dim=3) + vm_otherFEdemand[,,"feh2s"]),          "FE|Stationary|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"fehes"],dim=3),          "FE|Stationary|Heat (EJ/yr)"), 
                  setNames( ( dimSums(vm_cesIO[,,ppfen_stat_build_ind],dim=3)
                              + vm_otherFEdemand[,,"feels"] 
                              + vm_otherFEdemand[,,"fegas"] 
                              + vm_otherFEdemand[,,"feh2s"]
                              + vm_otherFEdemand[,,"fedie"]),             "FE|Other Sector (EJ/yr)")   
    )
  }
  
  #--- Buildings Module ---
  if (buil_mod == "simple"){
    tmp0 <- mbind(tmp0,
                  setNames(dimSums(vm_cesIO[,,"feelb"],dim=3),          "FE|Buildings|Electricity (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"fegab"],dim=3),          "FE|Buildings|Gases (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"fesob"],dim=3),          "FE|Buildings|Solids (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"fehob"],dim=3),          "FE|Buildings|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"feh2b"],dim=3),          "FE|Buildings|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"feheb"],dim=3),          "FE|Buildings|Heat (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,ppfen_build],dim=3),      "FE|Buildings (EJ/yr)")
    )
  }
  
  if (buil_mod %in% c("services_putty", "services_with_capital")){
    
    
    vm_demFeForEs = vm_demFeForEs[fe2es]
    
    ces_elec = c(grep("elb$", ppfen_build, value = T),grep("hpb$", ppfen_build, value = T))
    es_elec = c(grep("elb$", esty_build, value = T),grep("hpb$", esty_build, value = T))
    es_solids = c(grep("sob$", esty_build, value = T), grep("stb$", esty_build, value = T))
    es_gas = grep("gab$", esty_build, value = T)
    es_liq = grep("hob$", esty_build, value = T)
    es_heat = grep("heb$", esty_build, value = T)
    es_hydro = grep("h2b$", esty_build, value = T)
    
    putty_ue = c("uescb","ueshb","uealb","uecwb")
    
    tmp0 <- mbind(tmp0,
                  # Useful Energy
                  setNames(dimSums(vm_cesIO[,,"uealb"],dim=3),        "UE|Buildings|Appliances and Light (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"uecwb"],dim=3),        "UE|Buildings|Cooking and Water (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"ueshb"],dim=3),        "UE|Buildings|Space Heating (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"uescb"],dim=3),        "UE|Buildings|Space Cooling (EJ/yr)"),
                  
                  setNames(dimSums(vm_cesIO[,,putty_ue],dim=3),       "UE|Buildings (EJ/yr)"),
                  
                  setNames(dimSums(vm_cesIO[,,"uescb"],dim=3),        "UE|Buildings|Space Cooling|Electricity (EJ/yr)"),
                  
                  setNames(dimSums(vm_cesIO[,,"uealb"],dim=3),        "UE|Buildings|Appliances and Light|Electricity (EJ/yr)"),
                  
                  setNames(dimSums(v_prodEs[,,c("uecwsob","uecwstb")],dim=3),        "UE|Buildings|Cooking and Water|Solids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwsob"],dim=3),        "UE|Buildings|Cooking and Water|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwstb"],dim=3),        "UE|Buildings|Cooking and Water|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwelb"],dim=3),        "UE|Buildings|Cooking and Water|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwheb"],dim=3),        "UE|Buildings|Cooking and Water|Heat (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwgab"],dim=3),        "UE|Buildings|Cooking and Water|Gases (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwhob"],dim=3),        "UE|Buildings|Cooking and Water|Liquids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwh2b"],dim=3),        "UE|Buildings|Cooking and Water|Hydrogen (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwhpb"],dim=3),        "UE|Buildings|Cooking and Water|Electricity|Heat pumps (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,c("uecwelb","uecwhpb")],dim=3), "UE|Buildings|Cooking and Water|Electricity (EJ/yr)"),
                  
                  
                  setNames(dimSums(v_prodEs[,,c("ueshsob","ueshstb")],dim=3),        "UE|Buildings|Space Heating|Solids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshsob"],dim=3),        "UE|Buildings|Space Heating|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshstb"],dim=3),        "UE|Buildings|Space Heating|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshelb"],dim=3),        "UE|Buildings|Space Heating|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshheb"],dim=3),        "UE|Buildings|Space Heating|Heat (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshgab"],dim=3),        "UE|Buildings|Space Heating|Gases (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshhob"],dim=3),        "UE|Buildings|Space Heating|Liquids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshh2b"],dim=3),        "UE|Buildings|Space Heating|Hydrogen (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshhpb"],dim=3),        "UE|Buildings|Space Heating|Electricity|Heat pumps (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,c("ueshelb","ueshhpb")],dim=3), "UE|Buildings|Space Heating|Electricity (EJ/yr)"),
                  
                  # Final Energy
                  setNames(dimSums(vm_cesIO[,,"fealelb"],dim=3),        "FE|Buildings|Appliances and Light|Electricity (EJ/yr)"),
                  
                  setNames(dimSums(vm_demFeForEs[,,c("uecwsob","uecwstb")],dim=3),        "FE|Buildings|Cooking and Water|Solids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwsob"],dim=3),        "FE|Buildings|Cooking and Water|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwstb"],dim=3),        "FE|Buildings|Cooking and Water|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwelb"],dim=3),        "FE|Buildings|Cooking and Water|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwheb"],dim=3),        "FE|Buildings|Cooking and Water|Heat (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwgab"],dim=3),        "FE|Buildings|Cooking and Water|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwhob"],dim=3),        "FE|Buildings|Cooking and Water|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwh2b"],dim=3),        "FE|Buildings|Cooking and Water|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwhpb"],dim=3),        "FE|Buildings|Cooking and Water|Electricity|Heat pumps (EJ/yr)"),
                  
                  setNames(dimSums(vm_demFeForEs[,,c("uecwelb","uecwhpb")],dim=3), "FE|Buildings|Cooking and Water|Electricity (EJ/yr)"),
                  
                  setNames(dimSums(vm_cesIO[,,"fescelb"],dim=3),        "FE|Buildings|Space Cooling|Electricity (EJ/yr)"),
                  
                  setNames(dimSums(vm_demFeForEs[,,c("ueshsob","ueshstb")],dim=3),        "FE|Buildings|Space Heating|Solids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshsob"],dim=3),        "FE|Buildings|Space Heating|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshstb"],dim=3),        "FE|Buildings|Space Heating|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshelb"],dim=3),        "FE|Buildings|Space Heating|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshheb"],dim=3),        "FE|Buildings|Space Heating|Heat (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshgab"],dim=3),        "FE|Buildings|Space Heating|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshhob"],dim=3),        "FE|Buildings|Space Heating|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshh2b"],dim=3),        "FE|Buildings|Space Heating|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshhpb"],dim=3),        "FE|Buildings|Space Heating|Electricity|Heat pumps (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,c("ueshelb","ueshhpb")],dim=3), "FE|Buildings|Space Heating|Electricity (EJ/yr)"),
                  
                  setNames(dimSums(vm_cesIO[,,ces_elec],dim=3)
                           + dimSums(vm_demFeForEs[,,es_elec],dim=3),        "FE|Buildings|Electricity (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,es_gas],dim=3),         "FE|Buildings|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,es_solids],dim=3),      "FE|Buildings|Solids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,es_liq],dim=3),         "FE|Buildings|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,es_hydro],dim=3),       "FE|Buildings|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,es_heat],dim=3),        "FE|Buildings|Heat (EJ/yr)"),
                  
                  setNames(dimSums(vm_cesIO[,,ces_elec],dim=3) 
                           + dimSums(vm_demFeForEs[,,esty_build],dim=3) ,       "FE|Buildings (EJ/yr)")
    )
    
    tmp0 <- mbind(tmp0,
                  
                  setNames(dimSums(tmp0[,,c("UE|Buildings|Space Heating (EJ/yr)",
                                            "UE|Buildings|Space Cooling (EJ/yr)")],dim=3),        "UE|Buildings|Space Conditioning (EJ/yr)"),
                  
                  setNames(dimSums(tmp0[,,"FE|Buildings|Appliances and Light|Electricity (EJ/yr)"],dim=3),        "FE|Buildings|Appliances and Light (EJ/yr)"),
                  
                  setNames(dimSums(tmp0[,,c("FE|Buildings|Cooking and Water|Solids (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Electricity (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Heat (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Gases (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Liquids (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Hydrogen (EJ/yr)")],dim=3),        "FE|Buildings|Cooking and Water (EJ/yr)"),
                  
                  
                  setNames(dimSums(tmp0[,,"FE|Buildings|Space Cooling|Electricity (EJ/yr)"],dim=3),        "FE|Buildings|Space Cooling (EJ/yr)"),
                  
                  setNames(dimSums(tmp0[,,c("FE|Buildings|Space Heating|Solids (EJ/yr)",
                                            "FE|Buildings|Space Heating|Electricity (EJ/yr)",
                                            "FE|Buildings|Space Heating|Heat (EJ/yr)",
                                            "FE|Buildings|Space Heating|Gases (EJ/yr)",
                                            "FE|Buildings|Space Heating|Liquids (EJ/yr)",
                                            "FE|Buildings|Space Heating|Hydrogen (EJ/yr)")],dim=3),        "FE|Buildings|Space Heating (EJ/yr)")
                  
    )
  }
  

  # ---- Industry Module ----
  if (indu_mod == "fixed_shares"){
    tmp0 = mbind(tmp0,
                 setNames(dimSums(vm_cesIO[,,"feeli"],dim=3),          "FE|Industry|Electricity (EJ/yr)"),
                 setNames(dimSums(vm_cesIO[,,"fegai"],dim=3),          "FE|Industry|Gases (EJ/yr)"),
                 setNames(dimSums(vm_cesIO[,,"fesoi"],dim=3),          "FE|Industry|Solids (EJ/yr)"),
                 setNames(dimSums(vm_cesIO[,,"fehoi"],dim=3),          "FE|Industry|Liquids (EJ/yr)"),
                 setNames(dimSums(vm_cesIO[,,"feh2i"],dim=3),          "FE|Industry|Hydrogen (EJ/yr)"),
                 setNames(dimSums(vm_cesIO[,,"fehei"],dim=3),          "FE|Industry|Heat (EJ/yr)")
    )
  } else if ('subsectors' == indu_mod) {
    # list of FE items to calculate
    var_FE_Industry <- list(
      # solids
      'Solids|Cement'          = 'feso_cement',
      'Solids|Chemicals'       = 'feso_chemicals',
      'Solids|Steel'           = 'feso_steel',
      'Solids|Steel|Primary'   = 'feso_steel',
      'Solids|Steel|Secondary' = c(),
      'Solids|other'           = 'feso_otherInd',
      'Solids'                 = c('feso_cement', 'feso_chemicals', 
                                   'feso_steel', 'feso_otherInd'),
      # liquids
      'Liquids|Cement'          = 'feli_cement',
      'Liquids|Chemicals'       = 'feli_chemicals',
      'Liquids|Steel'           = 'feli_steel',
      'Liquids|Steel|Primary'   = 'feli_steel',
      'Liquids|Steel|Secondary' = c(),
      'Liquids|other'           = 'feli_otherInd',
      'Liquids'                 = c('feli_cement', 'feli_chemicals', 
                                    'feli_steel', 'feli_otherInd'),
      # gases
      'Gases|Cement'          = 'fega_cement',
      'Gases|Chemicals'       = 'fega_chemicals',
      'Gases|Steel'           = 'fega_steel',
      'Gases|Steel|Primary'   = 'fega_steel',
      'Gases|Steel|Secondary' = c(),
      'Gases|other'           = 'fega_otherInd',
      'Gases'                 = c('fega_cement', 'fega_chemicals', 
                                  'fega_steel', 'fega_otherInd'),
      # hydrogen
      'Hydrogen|Cement'          = 'feh2_cement',
      'Hydrogen|Chemicals'       = 'feh2_chemicals',
      'Hydrogen|Steel'           = 'feh2_steel',
      'Hydrogen|Steel|Primary'   = 'feh2_steel',
      'Hydrogen|Steel|Secondary' = c(),
      'Hydrogen|other'           = 'feh2_otherInd',
      'Hydrogen'                 = c('feh2_cement', 'feh2_chemicals', 
                                     'feh2_steel', 'feh2_otherInd'),
      # heat (only used in other Industry subsectors)
      'Heat|Cement'          = c(),
      'Heat|Chemicals'       = c(),
      'Heat|Steel'           = c(),
      'Heat|Steel|Primary'   = c(),
      'Heat|Steel|Secondary' = c(),
      'Heat|other'           = 'fehe_otherInd',
      'Heat'                 = 'fehe_otherInd',
      
      # electricity
      'Electricity|Cement' = 'feel_cement',
      
      'Electricity|Chemicals' = c('feelhth_chemicals', 'feelwlth_chemicals'),
      'Electricity|Chemicals|High-Temperature Heat' = 'feelhth_chemicals',
      'Electricity|Chemicals|Work and Low-Temperature Heat' = 
        'feelwlth_chemicals',
      
      'Electricity|Steel'           = c('feel_steel_primary', 
                                        'feel_steel_secondary'),
      'Electricity|Steel|Primary'   = 'feel_steel_primary',
      'Electricity|Steel|Secondary' = 'feel_steel_secondary',
      
      'Electricity|other' = c('feelhth_otherInd', 'feelwlth_otherInd'),
      'Electricity|other|High-Temperature Heat'         = 'feelhth_otherInd',
      'Electricity|other|Work and Low-Temperature Heat' = 'feelwlth_otherInd',
      
      'Electricity' = c('feel_cement', 'feelhth_chemicals', 
                        'feelwlth_chemicals', 'feel_steel_primary',
                        'feel_steel_secondary', 'feelhth_otherInd',
                        'feelwlth_otherInd'),
      
      'Electricity|High-Temperature Heat' = c('feelhth_chemicals', 
                                              'feelhth_otherInd'),
      
      'Electricity|Work and Low-Temperature Heat' = c('feel_cement',
                                                      'feelwlth_chemicals',
                                                      'feel_steel_primary',
                                                      'feel_steel_secondary',
                                                      'feelwlth_otherInd'),
      
      # subsector totals
      'Cement' = c('feso_cement', 'feli_cement', 'fega_cement', 'feh2_cement',
                   'feh2_cement', 'feel_cement'),
      
      'Chemicals' = c('feso_chemicals', 'feli_chemicals', 'fega_chemicals', 
                      'feh2_chemicals', 'feelhth_chemicals', 
                      'feelwlth_chemicals'),
      
      'Steel' = c('feso_steel', 'feli_steel', 'fega_steel', 'feh2_steel', 
                  'feel_steel_primary', 'feel_steel_secondary'),
      
      'Steel|Primary' = c('feso_steel', 'feli_steel', 'fega_steel', 
                          'feh2_steel', 'feel_steel_primary'),
      
      'Steel|Secondary' = 'feel_steel_secondary',

      'other' = c('feso_otherInd', 'feli_otherInd', 'fega_otherInd', 
                  'feh2_otherInd', 'feh2_otherInd', 'feelhth_otherInd', 
                  'feelwlth_otherInd')
    )

    # list of production items to calculate, including factor for unit conversion
    var_UE_Industry <- inline.data.frame(
      'item;                                                  pf;                   factor',
      'Production|Industry|Cement (Mt/yr);                    ue_cement;            1e3',
      'Production|Industry|Steel|Primary (Mt/yr);             ue_steel_primary;     1e3',
      'Production|Industry|Steel|Secondary (Mt/yr);           ue_steel_secondary;   1e3',
      'Value Added|Industry|Chemicals (billion US$2005/yr);   ue_chemicals;         1e3',
      'Value Added|Industry|other (billion US$2005/yr);       ue_otherInd;          1e3',
      'Activity|Industry (arbitrary unit/yr);                 ue_industry;          1'
    )
    
    tmp0 <- mbind(
      tmp0,
      
      # for each item in var_FE_Industry
      lapply(var_FE_Industry, 
             function(x) {
               # sum up indicated values from vm_cesIO, convert to EJ
               dimSums(mselect(vm_cesIO, all_in = x), dim = 3)
             }
      ) %>% 
        # bind resulting list to single magpie object
        mbind() %>% 
        # and rename accordingly
        setNames(paste0('FE|Industry|', names(var_FE_Industry), ' (EJ/yr)')),
      
      # get all UE values from vm_cesIO
      mselect(vm_cesIO, all_in = var_UE_Industry$pf) %>% 
        magclass::as.data.frame() %>% 
        mutate(Data1 = as.character(.data$Data1)) %>% 
        select(-.data$Cell) %>% 
        # combine with new names and factors
        inner_join(var_UE_Industry, c('Data1' = 'pf')) %>% 
        # compute converted values
        group_by(.data$Region, .data$Year, .data$item) %>% 
        # reverse unit conversion done during loading
        summarise(Value = sum(.data$Value * .data$factor) / TWa_2_EJ) %>% 
        ungroup() %>% 
        rename(Data1 = .data$item) %>% 
        # back to magpie
        as.magpie()
    )
    
    # calculate UE|Steel as the sum of primary and secondary steel
    tmp0 <- mbind(
      tmp0,
      setNames(
        tmp0[,,'Production|Industry|Steel|Primary (Mt/yr)']
        + tmp0[,,'Production|Industry|Steel|Secondary (Mt/yr)'],
        'Production|Industry|Steel (Mt/yr)')
    )
   
    # calculate specific energy consumption of industrial production
    tmp0 <- mbind(
      tmp0,
      
      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( tmp0[,,'FE|Industry|Cement (EJ/yr)']
        / tmp0[,,'Production|Industry|Cement (Mt/yr)']
        ) * 1e6,
        
        'Specific Energy Consumption|Production|Cement (MJ/t)'
      ),
      
      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( tmp0[,,'FE|Industry|Steel (EJ/yr)']
        / tmp0[,,'Production|Industry|Steel (Mt/yr)']
        ) * 1e6,
        
        'Specific Energy Consumption|Production|Steel (MJ/t)'
      )
    )
    
  }
  
  if ('simple' != stat_mod) {
    tmp0 <- mbind(
      tmp0,
      setNames(
        ( dimSums(vm_cesIO[,,ppfen_ind], dim = 3) ),  'FE|Industry (EJ/yr)'),
      
      setNames(
        ( dimSums(vm_cesIO[,,c(ppfen_build, ppfen_ind)], dim = 3)
          + vm_otherFEdemand[,,'feels'] 
          + vm_otherFEdemand[,,'fegas'] 
          + vm_otherFEdemand[,,'feh2s']
          + vm_otherFEdemand[,,'fedie']
        ),
        'FE|Other Sector (EJ/yr)')
    )
  }
  
  # add Industry electricity share (for SDG targets)
  tmp0 <- mbind(
    tmp0,
    setNames(
        tmp0[,,'FE|Industry|Electricity (EJ/yr)'] 
      / tmp0[,,'FE|Industry (EJ/yr)']
      * 100,
      'FE|Industry|Electricity|Share (%)')
  )
  
  # ----
  tmp1 <- NULL 
  tmp1 <- mbind(tmp0,
                setNames(dimSums(prodFE[,,se_Solids],dim=3),  "FE|+|Solids (EJ/yr)"),
                setNames(dimSums(prodFE[,,se_Liq],dim=3),     "FE|+|Liquids (EJ/yr)"),
                setNames(dimSums(prodFE[,,se_Gas],dim=3),  "FE|+|Gases (EJ/yr)"),
                setNames(dimSums(prodFE[,,FE_Elec_fety35],dim=3),      "FE|+|Electricity (EJ/yr)"),
                setNames(dimSums(prodFE[,,FE_H2_fety],dim=3),          "FE|+|Hydrogen (EJ/yr)"),
                setNames(dimSums(prodFE[,,"sehe.fehes.tdhes"],dim=3),  "FE|+|Heat (EJ/yr)"),
                setNames(dimSums(prodFE[,,FE_Transp_fety35],dim=3) - vm_otherFEdemand[,,'fedie'],    "FE|Transport (EJ/yr)"),
                setNames(dimSums(prodFE[,,"seel.feelt.tdelt"],dim=3),  "FE|Transport|Electricity (EJ/yr)"),
                setNames(dimSums(prodFE[,,"seh2.feh2t.tdh2t"],dim=3),  "FE|Transport|Hydrogen (EJ/yr)"),
                setNames(dimSums(prodFE[,,c("fedie","fepet")],dim=3) - vm_otherFEdemand[,,'fedie'],  "FE|Transport|Liquids (EJ/yr)"),
                setNames(dimSums(prodFE[,,"fedie"],dim=3)- vm_otherFEdemand[,,'fedie'],             "FE|Transport|Diesel (EJ/yr)"),
                setNames(dimSums(prodFE[,,"fepet"],dim=3),             "FE|Transport|Petrol (EJ/yr)"),
                setNames(p_eta_conv[,t2100,intersect(c("tdsos", "tdbiosos"),getNames(p_eta_conv))] * dimSums(prodSE[setSolBio][,t2100,], dim = 3), "FE|Solids|Biomass (EJ/yr)"),
                setNames(p_eta_conv[,t2100,intersect(c("tdsos", "tdbiosos"),getNames(p_eta_conv))] * dimSums(prodSE[,t2100,"biotrmod"], dim = 3),  "FE|Solids|Biomass|Modern (EJ/yr)"),
                setNames(p_eta_conv[,t2100,intersect(c("tdsos", "tdbiosos"),getNames(p_eta_conv))] * dimSums(prodSE[,t2100,"biotr"], dim = 3),     "FE|Solids|Biomass|Traditional (EJ/yr)"),
                setNames(p_eta_conv[,t2100,intersect(c("tdsos", "tdfossos"),getNames(p_eta_conv))] * dimSums(prodSE[,t2100,"coaltr"], dim = 3),    "FE|Solids|Coal (EJ/yr)")
  )

  if ("fegat" %in% getNames(prodFE, dim=2)){
    tmp1 <- mbind(tmp1,
                  setNames(dimSums(prodFE[,,"fegat"],dim=3),             "FE|Transport|Gases (EJ/yr)"))
  }else{
    ## If there is no nat. gas in transport, let's just put zero
    tmp1 <- mbind(tmp1,
                  setNames(dimSums(prodFE[,,"fepet"] * 0, dim=3),        "FE|Transport|Gases (EJ/yr)"))      
  }
    
  if (tran_mod == "complex"){

    ## load conversion parameters
    p35_passLDV_ES_efficiency <- readGDX(gdx,"p35_passLDV_ES_efficiency", restore_zeros = FALSE)
    p35_pass_FE_share_transp <- readGDX(gdx,"p35_pass_FE_share_transp", restore_zeros = FALSE)
    p35_freight_ES_efficiency <- readGDX(gdx,"p35_freight_ES_efficiency", restore_zeros = FALSE)
    p35_pass_nonLDV_ES_efficiency <- readGDX(gdx,"p35_pass_nonLDV_ES_efficiency", restore_zeros = FALSE)
    
    #choose the CES entries names for transport
    name_trsp=c("fepet","ueLDVt","fedie","ueHDVt","feelt","ueelTt","fepet_pass_sm","fedie_pass_sm","feelt_pass_sm","fedie_pass_lo","fedie_frgt_sm","feelt_frgt_sm","fedie_frgt_lo")
    name_trsp=name_trsp[name_trsp%in%getNames(vm_cesIO)]
    
    name_trsp_HDV <- c("fedie","ueHDVt")
    name_trsp_HDV=name_trsp_HDV[name_trsp_HDV%in%getNames(vm_cesIO)]
    name_trsp_LDV <- c("fepet","ueLDVt")
    name_trsp_LDV=name_trsp_LDV[name_trsp_LDV%in%getNames(vm_cesIO)]
    name_trsp_ELT <- c("feelt","ueelTt")
    name_trsp_ELT=name_trsp_ELT[name_trsp_ELT%in%getNames(vm_cesIO)]

    fe2ue <- readGDX(gdx,c("fe2ue", "fe2es"), format = "first_found")
    setTrainEl <- filter_(fe2ue, ~all_enty == "feelt", ~all_te == "apTrnElT")
    LDV35 <- readGDX(gdx, "LDV35")


    demFE <- readGDX(gdx,name=c("v_demFe","vm_demFe"),field="l",restore_zeros=FALSE,format="first_found")*TWa_2_EJ
    demFE <- demFE[fe2ue]

    tmp1 <- mbind(tmp1,
                  setNames(dimSums(demFE[,, "fepet"],dim=3),             "FE|Transport|Pass|Liquids (EJ/yr)" ),
                  setNames(dimSums(demFE[,, "fedie"] - vm_otherFEdemand[,,'fedie'],dim=3),             "FE|Transport|Freight|Liquids (EJ/yr)" ),
                  setNames(dimSums(demFE[,, "feh2t"],dim=3),             "FE|Transport|Pass|Hydrogen (EJ/yr)" ),
                  setNames(dimSums(demFE[,, "feelt"],dim=3),             "FE|Transport|Pass|Electricity (EJ/yr)" ),
                  setNames(dimSums(demFE[setTrainEl],dim=3),             "FE|Transport|Pass|Train|Electricity (EJ/yr)" ),
                  setNames(dimSums(demFE[,,LDV35],dim=3),                "FE|Transport|Pass|Road|LDV (EJ/yr)"),
                  setNames(dimSums(demFE[,,"apCarH2T"],dim=3),           "FE|Transport|Pass|Road|LDV|Hydrogen (EJ/yr)"),
                  setNames(dimSums(demFE[,,"apCarPeT"],dim=3),           "FE|Transport|Pass|Road|LDV|Liquids (EJ/yr)"),
                  setNames(dimSums(demFE[,,"apCarElT"],dim=3),           "FE|Transport|Pass|Road|LDV|Electricity (EJ/yr)"),    
                  setNames(dimSums(prodFE[,,FE_Transp_fety35],dim=3) 
                           -dimSums(demFE[,,LDV35],dim=3) - vm_otherFEdemand[,,'fedie'],               "FE|Transport|non-LDV (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,name_trsp_LDV],dim=3) * p35_passLDV_ES_efficiency,            "ES|Transport|Pass|LDV (bn pkm/yr)"),
                  setNames(dimSums(vm_cesIO[,,name_trsp_LDV],dim=3),            "UE|Transport|LDV (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,name_trsp_HDV],dim=3),            "UE|Transport|HDV (EJ/yr)"),
                  NULL
    )
  }
  if (tran_mod == "edge_esm") {
    ## define the set that contains fe2es for transport
    fe2es_dyn35 <- readGDX(gdx,c("fe2es_dyn35"), format = "first_found")
    
    vm_demFeForEs_trnsp = vm_demFeForEs[fe2es_dyn35]
    
    tmp1 <- mbind(tmp1,
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_",pmatch=TRUE],dim=3),"FE|Transport|Freight|Electricity (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_",pmatch=TRUE],dim=3),"FE|Transport|Pass|Electricity (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_",pmatch=TRUE],dim=3),"FE|Transport|Freight|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,c("esdie_pass_", "espet_pass_"),pmatch=TRUE],dim=3),"FE|Transport|Pass|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_",pmatch=TRUE],dim=3),"FE|Transport|Freight|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_",pmatch=TRUE],dim=3),"FE|Transport|Pass|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_",pmatch=TRUE],dim=3),"FE|Transport|Pass|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_",pmatch=TRUE],dim=3),"FE|Transport|Freight|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_sm",pmatch=TRUE],dim=3),"FE|Transport|Freight|Short-Medium distance|Electricity (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_sm",pmatch=TRUE],dim=3),"FE|Transport|Pass|Short-Medium distance|Electricity (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_sm",pmatch=TRUE],dim=3),"FE|Transport|Freight|Short-Medium distance|Diesel Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_sm",pmatch=TRUE],dim=3),"FE|Transport|Pass|Short-Medium distance|Diesel Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"espet_pass_sm",pmatch=TRUE],dim=3),"FE|Transport|Pass|Short-Medium distance|Petrol Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_lo",pmatch=TRUE],dim=3),"FE|Transport|Freight|Long distance|Diesel Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_lo",pmatch=TRUE],dim=3),"FE|Transport|Pass|Long distance|Diesel Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_sm",pmatch=TRUE],dim=3),"FE|Transport|Freight|Short-Medium distance|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_sm",pmatch=TRUE],dim=3),"FE|Transport|Pass|Short-Medium distance|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_sm",pmatch=TRUE],dim=3),"FE|Transport|Pass|Short-Medium distance|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_sm",pmatch=TRUE],dim=3),"FE|Transport|Freight|Short-Medium distance|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"_frgt_",pmatch=TRUE],dim=3),"FE|Transport|Freight (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs_trnsp[,,"_pass_",pmatch=TRUE],dim=3),"FE|Transport|Pass (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"entrp_frgt_",pmatch=TRUE],dim=3)/TWa_2_EJ * 1e3, # remove EJ conversion factor, conv. trillion to billion tkm
                           "ES|Transport|Freight (bn tkm/yr)"),
                  setNames(dimSums(vm_cesIO[,,"entrp_pass_",pmatch=TRUE],dim=3)/TWa_2_EJ * 1e3, # trillion to billion pkm
                           "ES|Transport|Pass (bn pkm/yr)"),
                  setNames(dimSums(vm_cesIO[,,"entrp_frgt_sm",pmatch=TRUE],dim=3)/TWa_2_EJ * 1e3, # trillion to billion tkm
                           "ES|Transport|Freight|Short-Medium distance (bn tkm/yr)"),
                  setNames(dimSums(vm_cesIO[,,"entrp_pass_sm",pmatch=TRUE],dim=3)/TWa_2_EJ * 1e3, # trillion to billion pkm
                           "ES|Transport|Pass|Short-Medium distance (bn pkm/yr)"),
                  setNames(dimSums(vm_cesIO[,,"entrp_frgt_lo",pmatch=TRUE],dim=3)/TWa_2_EJ * 1e3, # trillion to billion tkm
                           "ES|Transport|Freight|Long distance (bn tkm/yr)"),
                  setNames(dimSums(vm_cesIO[,,"entrp_pass_lo",pmatch=TRUE],dim=3)/TWa_2_EJ * 1e3, # trillion to billion pkm
                           "ES|Transport|Pass|Long distance (bn pkm/yr)"))

                        
    }
    
  
  
  
  #--- CDR reporting ---
  
  tmpCDR1 <- NULL
  tmpCDR1 <- mbind(tmpCDR1,
                setNames(vm_otherFEdemand[,,"feh2s"],        "FE|CDR|DAC|Hydrogen (EJ/yr)"),
                setNames(vm_otherFEdemand[,,"fegas"],        "FE|CDR|DAC|Gases (EJ/yr)"),
                setNames(vm_otherFEdemand[,,"fedie"],        "FE|CDR|EW|Diesel (EJ/yr)"),
                setNames(s33_rockgrind_fedem*dimSums(v33_grindrock_onfield[,,],dim=3),        "FE|CDR|EW|Electricity (EJ/yr)"),
                setNames(vm_otherFEdemand[,,"feels"],        "FE|CDR|Electricity (EJ/yr)"),
                setNames(vm_otherFEdemand[,,"fedie"],        "FE|CDR|Liquids (EJ/yr)"),
                setNames(vm_otherFEdemand[,,"fegas"],        "FE|CDR|Gases (EJ/yr)"),
                setNames(vm_otherFEdemand[,,"feh2s"],        "FE|CDR|Hydrogen (EJ/yr)")
  )
  
  
  tmpCDR2 <- NULL
  tmpCDR2 <- mbind(tmpCDR1,
                setNames(tmpCDR1[,,"FE|CDR|Electricity (EJ/yr)"] - tmpCDR1[,,"FE|CDR|EW|Electricity (EJ/yr)"], "FE|CDR|DAC|Electricity (EJ/yr)")
  )
  tmpCDR3 <- NULL
  tmpCDR3 <- mbind(tmpCDR2,
                setNames(tmpCDR1[,,"FE|CDR|DAC|Hydrogen (EJ/yr)"] + tmpCDR1[,,"FE|CDR|DAC|Gases (EJ/yr)"] + tmpCDR2[,,"FE|CDR|DAC|Electricity (EJ/yr)"], "FE|CDR|DAC (EJ/yr)"),
                setNames(tmpCDR2[,,"FE|CDR|EW|Diesel (EJ/yr)"] + tmpCDR2[,,"FE|CDR|EW|Electricity (EJ/yr)"], "FE|CDR|EW (EJ/yr)")
  )
  tmpCDR4 <- NULL
  tmpCDR4 <- mbind(tmpCDR3,
                setNames(tmpCDR3[,,"FE|CDR|DAC (EJ/yr)"] + tmpCDR3[,,"FE|CDR|EW (EJ/yr)"], "FE|CDR (EJ/yr)")
  )

  tmp2 <- mbind(
    tmp1,
    tmpCDR4)
  
  if (stat_mod == "simple"){
    tmp2 = mbind(tmp2,
                 setNames(tmp1[,,"FE|Stationary|Liquids (EJ/yr)"] + tmpCDR4[,,"FE|CDR|Liquids (EJ/yr)"],"FE|Other Sector|Liquids (EJ/yr)"),
                 setNames(tmp1[,,"FE|Stationary|Gases (EJ/yr)"] + tmpCDR4[,,"FE|CDR|Gases (EJ/yr)"],"FE|Other Sector|Gases (EJ/yr)"),
                 setNames(tmp1[,,"FE|Stationary|Hydrogen (EJ/yr)"] + tmpCDR4[,,"FE|CDR|Hydrogen (EJ/yr)"],"FE|Other Sector|Hydrogen (EJ/yr)"),
                 setNames(tmp1[,,"FE|Stationary|Electricity (EJ/yr)"] + tmpCDR4[,,"FE|CDR|Electricity (EJ/yr)"],"FE|Other Sector|Electricity (EJ/yr)"),
                 setNames(tmp1[,,"FE|Stationary|Solids (EJ/yr)"], "FE|Other Sector|Solids (EJ/yr)"),
                 setNames(tmp1[,,"FE|Stationary|Heat (EJ/yr)"], "FE|Other Sector|Heat (EJ/yr)")
                 )
  } else if (stat_mod == "off"){
    tmp2 = mbind(tmp2,
                 setNames(tmp1[,,"FE|Buildings|Solids (EJ/yr)"] + tmp1[,,"FE|Industry|Solids (EJ/yr)"] ,"FE|Other Sector|Solids (EJ/yr)"),
                 setNames(tmp1[,,"FE|Buildings|Liquids (EJ/yr)"] + tmp1[,,"FE|Industry|Liquids (EJ/yr)"] + tmpCDR4[,,"FE|CDR|Liquids (EJ/yr)"],"FE|Other Sector|Liquids (EJ/yr)"),
                 setNames(tmp1[,,"FE|Buildings|Gases (EJ/yr)"] + tmp1[,,"FE|Industry|Gases (EJ/yr)"] + tmpCDR4[,,"FE|CDR|Gases (EJ/yr)"],"FE|Other Sector|Gases (EJ/yr)"),
                 setNames(tmp1[,,"FE|Buildings|Hydrogen (EJ/yr)"] + tmp1[,,"FE|Industry|Hydrogen (EJ/yr)"]+ tmpCDR4[,,"FE|CDR|Hydrogen (EJ/yr)"],"FE|Other Sector|Hydrogen (EJ/yr)"),
                 setNames(tmp1[,,"FE|Buildings|Electricity (EJ/yr)"] + tmp1[,,"FE|Industry|Electricity (EJ/yr)"]+ tmpCDR4[,,"FE|CDR|Electricity (EJ/yr)"],"FE|Other Sector|Electricity (EJ/yr)"),
                 setNames(tmp1[,,"FE|Buildings|Heat (EJ/yr)"] + tmp1[,,"FE|Industry|Heat (EJ/yr)"],"FE|Other Sector|Heat (EJ/yr)")
                 )
    if("segabio" %in% se_Gas){
      tmp2 <- mbind(tmp2,
                 setNames(dimSums(prodFE[,,"segabio.fegas.tdbiogas"],dim=3),"FE|Other Sector|Gases|Biomass (EJ/yr)"),
                 setNames(dimSums(prodFE[,,"segafos.fegas.tdfosgas"],dim=3),"FE|Other Sector|Gases|Non-Biomass (EJ/yr)"),
                 setNames(dimSums(prodFE[,,c("seliqbio.fedie.tdbiodie", "seliqbio.fepet.tdbiopet")],dim=3),"FE|Transport|Liquids|Biomass (EJ/yr)"),
                 setNames(dimSums(prodFE[,,c("seliqfos.fedie.tdfosdie", "seliqfos.fepet.tdfospet")],dim=3),"FE|Transport|Liquids|Non-Biomass (EJ/yr)")
                 )
    }

    if("fegat" %in% fety && "segabio" %in% se_Gas){
      tmp2 <- mbind(tmp2,
                 setNames(dimSums(prodFE[,,"segabio.fegat.tdbiogat"],dim=3),"FE|Transport|Gases|Biomass (EJ/yr)"),
                 setNames(dimSums(prodFE[,,"segafos.fegat.tdfosgat"],dim=3),"FE|Transport|Gases|Non-Biomass (EJ/yr)")
                 )

    }
  }
    
    tmp2 = mbind(tmp2,
    setNames(tmp1[,,"FE|Solids|Biomass (EJ/yr)"],"FE|Other Sector|Solids|Biomass (EJ/yr)"),
    setNames(tmp1[,,"FE|Solids|Biomass|Traditional (EJ/yr)"],"FE|Other Sector|Solids|Biomass|Traditional (EJ/yr)"),
    setNames(tmp1[,,"FE|Solids|Coal (EJ/yr)"],"FE|Other Sector|Solids|Coal (EJ/yr)"),
    setNames(tmp1[,,"FE|Other Sector (EJ/yr)"] + tmp1[,,"FE|Transport (EJ/yr)"], "FE (EJ/yr)"),
    setNames(tmp1[,,"FE|+|Electricity (EJ/yr)"] / TWa_2_EJ * 1000 * 1.1 * 1.4, "Cap|Electricity|Peak Demand|Estimated from FE x 1p4 x 1p1 (GW)") 
   )
  
  tmp2a <- mbind(
    tmp2,
    setNames(tmp2[,,"FE (EJ/yr)"],"FE|Gross with CDR (EJ/yr)"),
    setNames(tmp2[,,"FE (EJ/yr)"] - tmp2[,,"FE|CDR (EJ/yr)"], "FE|Net without CDR (EJ/yr)")
  )
  

  if (tran_mod == "complex") {  
    tmp2 <- mbind(tmp2a,
                  setNames(p35_pass_FE_share_transp * tmp1[,, "FE|Transport|non-LDV (EJ/yr)"], "FE|Transport|Pass|non-LDV (EJ/yr)"),
                  setNames((1- p35_pass_FE_share_transp) * tmp1[,, "FE|Transport|non-LDV (EJ/yr)"], "FE|Transport|Freight (EJ/yr)"),
                  setNames(p35_pass_FE_share_transp * tmp1[,, "UE|Transport|HDV (EJ/yr)"], "UE|Transport|Pass|non-LDV (EJ/yr)"),
                  setNames((1- p35_pass_FE_share_transp) * tmp1[,, "UE|Transport|HDV (EJ/yr)"], "UE|Transport|Freight (EJ/yr)"),
                  NULL
    )
  
  # 1.1 is loss for SE2FE, 1.4 is peak to average (US 1.5, IND/CHN/AFR 1.2, EUR/JPN 1.4)
    
    tmp3 <- mbind(tmp2,
                  setNames(p35_freight_ES_efficiency * tmp2[,,"UE|Transport|Freight (EJ/yr)"],"ES|Transport|Freight (bn tkm/yr)"),
                  setNames(p35_pass_nonLDV_ES_efficiency * tmp2[,,"UE|Transport|Pass|non-LDV (EJ/yr)"],"ES|Transport|Pass|non-LDV (bn pkm/yr)"),
                  setNames(tmp2[,,"FE|Transport|Pass|non-LDV (EJ/yr)"] + setNames(tmp2[,,"FE|Transport|Pass|Road|LDV (EJ/yr)"],NULL), "FE|Transport|Pass (EJ/yr)")
    )  

    tmp4 <- mbind(tmp3,
                  setNames(tmp3[,,"ES|Transport|Pass|LDV (bn pkm/yr)"] + tmp3[,,"ES|Transport|Pass|non-LDV (bn pkm/yr)"],"ES|Transport|Pass (bn pkm/yr)")
    )     
  }else{
    # we add no entries here for now. *TODO* check if these entries are used, e.g., in exoGAINSairpollutants, to sum up fes.
    tmp4 <- tmp2
  }
  
  #--- Disaggregate solids between coal, modern biomass and traditional biomass
  if (stat_mod == "off"){

    tmp4 <-  mbind(tmp4,  setNames(asS4(pmin(tmp4[,,"FE|Solids|Biomass|Traditional (EJ/yr)"],tmp4[,,"FE|Buildings|Solids (EJ/yr)"]))  ,"FE|Buildings|Solids|Biomass|Traditional (EJ/yr)"))
    
    tmp4 <-  mbind(tmp4,  setNames(tmp4[,,"FE|Solids|Biomass|Traditional (EJ/yr)"] - tmp4[,,"FE|Buildings|Solids|Biomass|Traditional (EJ/yr)"],"FE|Industry|Solids|Biomass|Traditional (EJ/yr)" ))
    
    share_sol_noTrad_buil = (tmp4[,,"FE|Buildings|Solids (EJ/yr)"] - tmp4[,,"FE|Buildings|Solids|Biomass|Traditional (EJ/yr)"]) / (tmp1[,,"FE|+|Solids (EJ/yr)"] - tmp4[,,"FE|Solids|Biomass|Traditional (EJ/yr)"] )
    share_sol_noTrad_indu = (tmp4[,,"FE|Industry|Solids (EJ/yr)"] - tmp4[,,"FE|Industry|Solids|Biomass|Traditional (EJ/yr)"]) / (tmp1[,,"FE|+|Solids (EJ/yr)"] - tmp4[,,"FE|Solids|Biomass|Traditional (EJ/yr)"] )
    
    tmp4 <- mbind(tmp4,
                  setNames(tmp4[,,"FE|Solids|Biomass|Modern (EJ/yr)"] *share_sol_noTrad_buil,  "FE|Buildings|Solids|Biomass|Modern (EJ/yr)"),
                  setNames(tmp4[,,"FE|Solids|Coal (EJ/yr)"]  * share_sol_noTrad_buil,"FE|Buildings|Solids|Coal (EJ/yr)"),
                  setNames(tmp4[,,"FE|Solids|Biomass|Modern (EJ/yr)"] * share_sol_noTrad_indu  ,"FE|Industry|Solids|Biomass|Modern (EJ/yr)"),
                  setNames(tmp4[,,"FE|Solids|Coal (EJ/yr)"] *  share_sol_noTrad_indu,"FE|Industry|Solids|Coal (EJ/yr)")
    )
    
    tmp4 <-  mbind(tmp4,
                   setNames(tmp4[,,"FE|Buildings|Solids|Biomass|Traditional (EJ/yr)"] +  tmp4[,,"FE|Buildings|Solids|Biomass|Modern (EJ/yr)"],"FE|Buildings|Solids|Biomass (EJ/yr)"),
                   setNames(tmp4[,,"FE|Industry|Solids|Biomass|Traditional (EJ/yr)"] +tmp4[,,"FE|Industry|Solids|Biomass|Modern (EJ/yr)"],"FE|Industry|Solids|Biomass (EJ/yr)")
    )
    
    # Add fuel (vs elec) computation
    tmp4 = mbind(tmp4,
    setNames(tmp4[,,"FE|Buildings (EJ/yr)"] - tmp4[,,"FE|Buildings|Electricity (EJ/yr)"] - tmp4[,,"FE|Buildings|Heat (EJ/yr)"], "FE|Buildings|Fuels (EJ/yr)"),
    setNames(tmp4[,,"FE|Industry (EJ/yr)"] - tmp4[,,"FE|Industry|Electricity (EJ/yr)"] - tmp4[,,"FE|Industry|Heat (EJ/yr)"], "FE|Industry|Fuels (EJ/yr)")  
    )
  }
  
 
  
  tmp5 <- mbind(tmp4,
                setNames(tmp4[,,"FE|Transport|Liquids (EJ/yr)"] + tmp4[,,"FE|Transport|Hydrogen (EJ/yr)"], "FE|Transport|Fuels (EJ/yr)"),
                setNames(tmp4[,,"FE (EJ/yr)"] - tmp4[,,"FE|+|Electricity (EJ/yr)"] - tmp4[,,"FE|+|Heat (EJ/yr)"], "FE|Fuels (EJ/yr)") 
  )
  out <- tmp5
  # add global values
  out <- mbind(out,dimSums(out,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out,do.call("mbind",lapply(names(regionSubsetList), function(x) { result <- dimSums(out[regionSubsetList[[x]],,],dim=1); getRegions(result) <- x ; return(result) })))
  
  out2 <- mbind(out,
                setNames(100 * out[,,"FE|Transport|Fuels (EJ/yr)"] / out[,,"FE|Transport (EJ/yr)"], "FE|Transport|Fuels|Share (Percent)"),
                setNames(100 * out[,,"FE|Fuels (EJ/yr)"] / out[,,"FE (EJ/yr)"], "FE|Fuels|Share (Percent)") 
  )
  
  # change Other Sector to Buildings and Industry if this is the structure used
  if (stat_mod == "off"){ 
    magclass::getNames(out2) <- sub("Other Sector","Buildings and Industry", magclass::getNames(out2))
    
  # Add fuel shares  
    out2 = mbind(out2, 
                 setNames(100 * out[,,"FE|Buildings|Fuels (EJ/yr)"] / out[,,"FE|Buildings (EJ/yr)"], "FE|Buildings|Fuels|Share (Percent)"),
                 setNames(100 * out[,,"FE|Industry|Fuels (EJ/yr)"] / out[,,"FE|Industry (EJ/yr)"], "FE|Industry|Fuels|Share (Percent)")
                 )
  }
  return(out2)
}
