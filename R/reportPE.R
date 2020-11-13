#' Read in GDX and calculate primary energy, used in convGDX2MIF.R for the
#' reporting
#' 
#' Read in primary energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{reportPE(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mselect getYears dimSums getNames<- mbind setNames

reportPE <- function(gdx,regionSubsetList=NULL){
  ####### conversion factors ##########
  TWa_2_EJ     <- 31.536
  ####### read in needed data #########
  ## sets
  pe2se    <- readGDX(gdx,"pe2se")
  tefosccs <- readGDX(gdx,c("teFosCCS","tefosccs"),format="first_found")
  teccs    <- readGDX(gdx,c("teCCS","teccs"),format="first_found")
  tenoccs  <- readGDX(gdx,c("teNoCCS","tenoccs"),format="first_found")
  pebio    <- readGDX(gdx,c("peBio","pebio"),format="first_found")
  sety     <- readGDX(gdx,c("entySe","sety"),format="first_found")
  oc2te    <- readGDX(gdx,c("pc2te","oc2te"),format="first_found")
  
  # the set liquids changed from sepet+sedie to seLiq in REMIND 1.7. Seliq, sega and seso changed to include biomass or Fossil origin after REMIND 2.0
  se_Liq    <- intersect(c("seliqfos", "seliqbio", "seliq", "sepet","sedie"),sety)
  se_Gas    <- intersect(c("segafos", "segabio", "sega"),sety)
  se_Solids <- intersect(c("sesofos", "sesobio", "seso"),sety)
  
  enty2rlf <- readGDX(gdx,c("pe2rlf","enty2rlf"),format="first_found")
  ## parameter
  dataoc_tmp       <- readGDX(gdx,c("pm_prodCouple","p_prodCouple","p_dataoc"),restore_zeros=FALSE,format="first_found") 
  dataoc_tmp[is.na(dataoc_tmp)] <- 0
  p_costsPEtradeMp <- readGDX(gdx,c("pm_costsPEtradeMp","p_costsPEtradeMp"),restore_zeros=FALSE)
  p_macBase        <- readGDX(gdx,c("p_macBaseMagpie","p_macBase"),format="first_found")*TWa_2_EJ
#  p_macEmi         <- readGDX(gdx,c("p_macEmi"),format="first_found")*TWa_2_EJ
  ## variables
  demPE  <- readGDX(gdx,name=c("vm_demPe","v_pedem"),field="l",restore_zeros=FALSE,format="first_found")*TWa_2_EJ
  demPE  <- demPE[pe2se]
  prodSE <- readGDX(gdx,name=c("vm_prodSe","v_seprod"),field="l",restore_zeros=FALSE,format="first_found")*TWa_2_EJ
  prodSE <- mselect(prodSE,all_enty1=sety)
  fuelex <- readGDX(gdx,c("vm_fuExtr","vm_fuelex"),field="l",format="first_found")*TWa_2_EJ
  Mport  <- readGDX(gdx,c("vm_Mport"),field="l",format="first_found")*TWa_2_EJ
  Xport  <- readGDX(gdx,c("vm_Xport"),field="l",format="first_found")*TWa_2_EJ
  ####### calculate minimal temporal resolution #####
  y <- Reduce(intersect,list(getYears(demPE),getYears(prodSE)))
  demPE  <- demPE[,y,]
  prodSE <- prodSE[,y,]
  fuelex <- fuelex[,y,]
  Mport  <- Mport[,y,]
  Xport  <- Xport[,y,]
  p_macBase <- p_macBase[,y,]
#  p_macEmi  <- p_macEmi[,y,]
  ####### fix negative values to 0 ##################
  #### adjust regional dimension of dataoc
  dataoc <- new.magpie(getRegions(prodSE),getYears(dataoc_tmp),magclass::getNames(dataoc_tmp),fill=0)
  dataoc[getRegions(dataoc_tmp),,] <- dataoc_tmp
  getSets(dataoc) <- getSets(dataoc_tmp)
  dataoc[dataoc<0] <- 0
  ####### internal function for reporting ###########
  pe_carrier <- function(demPE,dataoc,oc2te,sety,pecarrier,secarrier,te=pe2se$all_te,name=NULL){
    ## identify all techs with secarrier as a main product    
    sub1_oc2te <- oc2te[(oc2te$all_enty %in% pecarrier) & (oc2te$all_enty1 %in% secarrier) & (oc2te$all_enty2 %in% sety)    & (oc2te$all_te %in% te),]
    ## identify all techs with secarrier as a couple product        
    sub2_oc2te <- oc2te[(oc2te$all_enty %in% pecarrier) & (oc2te$all_enty1 %in% sety)    & (oc2te$all_enty2 %in% secarrier) & (oc2te$all_te %in% te),]
    ## all primary energy demand with secarrier as a main product
    x1 <- dimSums(mselect(demPE,all_enty=pecarrier,all_enty1=secarrier,all_te=te),dim=3)  
    ## negative term for couple products by technologies with secarrier as a main product
    x2 <- dimSums(demPE[sub1_oc2te]*dataoc[sub1_oc2te]/(1+dataoc[sub1_oc2te]),dim=3)
    ## additional pe demand for technologies with secarrier as a couple product
    x3 <- dimSums(demPE[sub2_oc2te]*dataoc[sub2_oc2te]/(1+dataoc[sub2_oc2te]),dim=3)
    out <- (x1-x2+x3)
    if(!is.null(name)) magclass::getNames(out) <- name
    return(out)
  }
  ####### calculate reporting parameters ############
  tmp1 <- NULL 
  tmp1 <- mbind(
          setNames(dimSums(demPE[,,c("peoil","pecoal","pegas")],dim=3),          "PE|Fossil (EJ/yr)"),
          setNames(dimSums(demPE[,,tefosccs],dim=3),                             "PE|Fossil|w/ CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,c("peoil","pecoal","pegas")],dim=3) 
                 - dimSums(demPE[,,tefosccs],dim=3),                             "PE|Fossil|w/o CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,"pecoal"],dim=3),                             "PE|+|Coal (EJ/yr)"),
          setNames(dimSums(mselect(demPE,all_enty="pecoal",all_te=teccs),dim=3), "PE|Coal|w/ CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,"pecoal"],dim=3)
                 - dimSums(mselect(demPE,all_enty="pecoal",all_te=teccs),dim=3), "PE|Coal|w/o CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,"peoil"],dim=3),                              "PE|+|Oil (EJ/yr)"),
          setNames(dimSums(mselect(demPE,all_enty="peoil",all_te=tenoccs),dim=3),"PE|Oil|w/o CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,"pegas"],dim=3),                              "PE|+|Gas (EJ/yr)"),
          setNames(dimSums(mselect(demPE,all_enty="pegas",all_te=teccs),dim=3),  "PE|Gas|w/ CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,"pegas"],dim=3)
                 - dimSums(mselect(demPE,all_enty="pegas",all_te=teccs),dim=3),  "PE|Gas|w/o CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,pebio],dim=3),                                "PE|+|Biomass (EJ/yr)"),
          setNames(dimSums(mselect(demPE,all_enty=pebio,all_te=teccs),dim=3),    "PE|Biomass|w/ CCS (EJ/yr)"),
          setNames(dimSums(demPE[,,pebio],dim=3)
                 - dimSums(mselect(demPE,all_enty=pebio,all_te=teccs),dim=3),    "PE|Biomass|w/o CCS (EJ/yr)"),
          setNames(dimSums(mselect(demPE,all_enty=pebio,all_te="biotr"),dim=3),  "PE|Biomass|Traditional (EJ/yr)"),
          setNames(dimSums(demPE[,,c("pebios","pebioil")],dim=3),                "PE|Biomass|1st Generation (EJ/yr)"),
          setNames(dimSums(demPE[,,pebio],dim=3)
                 - dimSums(mselect(demPE,all_enty=pebio,all_te="biotr"),dim=3),  "PE|Biomass|Modern (EJ/yr)")
          )
  # Nuclear and Renewables
  tmp2 <- NULL 
  tmp2 <- mbind(tmp2,setNames(dimSums(mselect(prodSE,all_enty="peur"),dim=3),     "PE|+|Nuclear (EJ/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(mselect(prodSE,all_enty=c("pegeo","pehyd","pewin","pesol")),dim=3),"PE|Non-Biomass Renewables (EJ/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(mselect(prodSE,all_enty="pehyd"),dim=3),    "PE|+|Hydro (EJ/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(mselect(prodSE,all_enty="pewin"),dim=3),    "PE|+|Wind (EJ/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(mselect(prodSE,all_enty="pesol"),dim=3),    "PE|+|Solar (EJ/yr)"))
  tmp2 <- mbind(tmp2,setNames(dimSums(mselect(prodSE,all_enty="pegeo"),dim=3),    "PE|+|Geothermal (EJ/yr)"))
                
  # primary energy consumption per carrier  --- use function declared above
  tmp3 <- mbind(pe_carrier(demPE,dataoc,oc2te,sety,pebio,"seel",                     name="PE|Biomass|Electricity (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,"seel",teccs,               name="PE|Biomass|Electricity|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,"seel",tenoccs,             name="PE|Biomass|Electricity|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,se_Gas,                     name="PE|Biomass|Gases (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,"seh2",                     name="PE|Biomass|Hydrogen (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,"seh2",teccs,               name="PE|Biomass|Hydrogen|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,"seh2",tenoccs,             name="PE|Biomass|Hydrogen|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,se_Liq ,                    name="PE|Biomass|Liquids (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,se_Liq,teccs,               name="PE|Biomass|Liquids|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,se_Liq,tenoccs,             name="PE|Biomass|Liquids|w/o CCS (EJ/yr)"),
                
                pe_carrier(demPE,dataoc,oc2te,sety,"pebiolc",se_Liq ,                name="PE|Biomass|Liquids|Cellulosic (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pebiolc",se_Liq ,teccs,          name="PE|Biomass|Liquids|Cellulosic|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pebiolc",se_Liq ,tenoccs,        name="PE|Biomass|Liquids|Cellulosic|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,c("pebioil", "pebios"), se_Liq ,  name="PE|Biomass|Liquids|Non-Cellulosic (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pebios",se_Liq ,                 name="PE|Biomass|Liquids|Conventional Ethanol (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,se_Liq , c("bioftrec", "bioftcrec","biodiesel"),
                                                                                     name="PE|Biomass|Liquids|Biodiesel (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,se_Liq , c("bioftcrec"),
                                                                                     name="PE|Biomass|Liquids|Biodiesel|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,se_Liq , c("bioftrec","biodiesel"),
                                                                                     name="PE|Biomass|Liquids|Biodiesel|w/o CCS (EJ/yr)"),
                
                
                
                
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,c(se_Solids),                  name="PE|Biomass|Solids (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,pebio,c("sehe"),                  name="PE|Biomass|Heat (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal","seel",                  name="PE|Coal|Electricity (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal","seel",teccs,            name="PE|Coal|Electricity|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal","seel",tenoccs,          name="PE|Coal|Electricity|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",se_Gas,                  name="PE|Coal|Gases (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",se_Gas,teccs,            name="PE|Coal|Gases|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",se_Gas,tenoccs,          name="PE|Coal|Gases|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",se_Liq,                  name="PE|Coal|Liquids (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",se_Liq,teccs,            name="PE|Coal|Liquids|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",se_Liq,tenoccs,          name="PE|Coal|Liquids|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal","seh2",                  name="PE|Coal|Hydrogen (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal","seh2",teccs,            name="PE|Coal|Hydrogen|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal","seh2",tenoccs,          name="PE|Coal|Hydrogen|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",c("sehe"),               name="PE|Coal|Heat (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pecoal",c(se_Solids),               name="PE|Coal|Solids (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas","seel",                   name="PE|Gas|Electricity (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas","seel",teccs,             name="PE|Gas|Electricity|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas","seel",tenoccs,           name="PE|Gas|Electricity|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",c(se_Gas),                name="PE|Gas|Gases (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",se_Liq,                   name="PE|Gas|Liquids (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",se_Liq,teccs,             name="PE|Gas|Liquids|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",se_Liq,tenoccs,           name="PE|Gas|Liquids|w/o CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",c("sehe"),                name="PE|Gas|Heat (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",c("seh2"),                name="PE|Gas|Hydrogen (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",c("seh2"),teccs,          name="PE|Gas|Hydrogen|w/ CCS (EJ/yr)"),
                pe_carrier(demPE,dataoc,oc2te,sety,"pegas",c("seh2"),tenoccs,        name="PE|Gas|Hydrogen|w/o CCS (EJ/yr)"),
                setNames(dimSums(mselect(prodSE,all_enty="pesol",all_enty1="seel"),dim=3),"PE|Solar|Electricity (EJ/yr)"))
  tmp4 <- NULL
  tmp4 <- mbind(tmp4, setNames( dimSums(demPE[,,c("peoil","pecoal","pegas")],dim=3) 
                              + dimSums(demPE[,,c("pebiolc","pebios","pebioil")],dim=3)   
                              + dimSums(prodSE[,,c("pegeo","pehyd","pewin","pesol","peur")],dim=3),"PE (EJ/yr)"))
  tmp4 <- mbind(tmp4,setNames(fuelex[,,"pebiolc.2"],"PE|Biomass|Residues (EJ/yr)"))
# Moved to reportExtraction tmp4 <- mbind(tmp4,setNames(dimSums(fuelex[,,c("pebioil","pebios")][enty2rlf],dim=3),"PE|Production|Biomass|1st Generation (EJ/yr)")) 
  tmp4 <- mbind(tmp4,setNames(( fuelex[,,"pebiolc.1"]
                             + (1-p_costsPEtradeMp[,,"pebiolc"]) * Mport[,,"pebiolc"] - Xport[,,"pebiolc"]
                             ),"PE|Biomass|Energy Crops (EJ/yr)"))
#  tmp4 <- mbind(tmp4,setNames(0.001638 * (p_macBase[,,"ch4gas"] - p_macEmi[,,"ch4gas"]),"PE|Gas|fromCH4MAC|Gas (EJ/yr)"))
#  tmp4 <- mbind(tmp4,setNames(0.001638 * 0.5 *(p_macBase[,,"ch4coal"] - p_macEmi[,,"ch4coal"]),"PE|Gas|fromCH4MAC|Coalbed (EJ/yr)"))
#  tmp4 <- mbind(tmp4,setNames(0.001638 * (p_macBase[,,"ch4wstl"] - p_macEmi[,,"ch4wstl"]) ,"PE|Gas|fromCH4MAC|Waste (EJ/yr)"))
  
  out <- mbind(tmp1,tmp2,tmp3,tmp4)
  # add global values
  out <- mbind(out,dimSums(out,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))
  
  return(out)
}
