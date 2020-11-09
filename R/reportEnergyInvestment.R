#' Read in GDX and calculate prices, used in convGDX2MIF.R for the reporting
#' 
#' Read in price information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @return MAgPIE object - contains the price variables
#' @author Anastasis Giannousaki
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportEnergyInvestment(gdx)}
#' 
#' @export
#' @importFrom magclass dimSums mbind
reportEnergyInvestment <- function(gdx,regionSubsetList=NULL) {
  
  # read sets
  adjte   <- readGDX(gdx,name=c("teAdj","adjte"),format="first_found")
  petyf   <- readGDX(gdx,c("peFos","petyf"),format="first_found")
  perenew <- readGDX(gdx,c("peRe","perenew"),format="first_found")
  pe2se   <- readGDX(gdx,name=c("pe2se"),format="first_found")
  se2fe   <- readGDX(gdx,name=c("se2fe"),format="first_found")
  se2se   <- readGDX(gdx,name=c("se2se"),format="first_found")
  teccs   <- readGDX(gdx,name=c("teCCS","teccs"),format="first_found")
  tenoccs <- readGDX(gdx,name=c("teNoCCS","tenoccs"),format="first_found")
  sety       <- readGDX(gdx,c("entySe","sety"),format="first_found")
  
  # the set liquids changed from sepet+sedie to seLiq in REMIND 1.7. Seliq, sega and seso changed to include biomass or Fossil origin after REMIND 2.0
  se_Liq    <- intersect(c("seliqfos", "seliqbio", "seliq", "sepet","sedie"),sety)
  se_Gas    <- intersect(c("segafos", "segabio", "sega"),sety)
  se_Solids <- intersect(c("sesofos", "sesobio", "seso"),sety)
  
  all_te  <- readGDX(gdx,name=c("all_te"),format="first_found")
  tenotransform  <- readGDX(gdx,c("teNoTransform","tenotransform"),format="first_found")
  teue2rlf  <- readGDX(gdx,name=c("teue2rlf", "tees2rlf"),format="first_found")
  temapall  <- readGDX(gdx,name=c("en2en","temapall"),format="first_found")
  stor  <- readGDX(gdx,name=c("teStor","stor"),format="first_found")
  grid  <- readGDX(gdx,name=c("teGrid","grid"),format="first_found")
  pebio  <- readGDX(gdx,c("peBio","pebio"),format="first_found")
  ttot        <- readGDX(gdx,name="ttot",format="first_found")
  # read variables
  v_directteinv <- readGDX(gdx,name=c("v_costInvTeDir","v_directteinv"),field="l",format="first_found")
  v_adjustteinv <- readGDX(gdx,name=c("v_costInvTeAdj","v_adjustteinv"),field="l",format="first_found")
    
  # data preparation
  ttot <- as.numeric(as.vector(readGDX(gdx,"ttot",format="first_found")))
  v_directteinv <- v_directteinv[,ttot,]
  v_adjustteinv <- v_adjustteinv[,ttot,]

  
  ####### internal function for reporting ########### 
  ## "ie" stands for input energy, "oe" for output energy
  inv_se <- function(ie,oe,settofilter=pe2se,adjte,v_directteinv,v_adjustteinv,te=pe2se$all_te){
    if (!is.character(settofilter)){
      if (attr(settofilter,which="gdxdata")$name %in% c("pe2se","se2fe","se2se","temapall","en2en")){
        sub1_pe2se <- settofilter[((settofilter$all_enty %in% ie) & (settofilter$all_enty1 %in% oe) & (settofilter$all_te %in% te) & !(settofilter$all_te %in% adjte)),]
        
        sub2_pe2se <- settofilter[((settofilter$all_enty %in% ie) & (settofilter$all_enty1 %in% oe) & (settofilter$all_te %in% te) & (settofilter$all_te %in% adjte)),]
        x1 <- dimSums(v_directteinv[,,sub1_pe2se$all_te],dim=3) * 1000
        x2 <- dimSums(v_directteinv[,,sub2_pe2se$all_te] + v_adjustteinv[,,sub2_pe2se$all_te],dim=3) * 1000
      } 
    } else {
      sub1_pe2se <- settofilter[((settofilter %in% te) & !(settofilter %in% adjte))]
      
      sub2_pe2se <- settofilter[((settofilter %in% te) & (settofilter %in% adjte))]
      x1 <- dimSums(v_directteinv[,,sub1_pe2se],dim=3) * 1000
      x2 <- dimSums(v_directteinv[,,sub2_pe2se] + v_adjustteinv[,,sub2_pe2se],dim=3) * 1000
    }
    out <- (x1+x2)
    return(out)
  }
  
  # build reporting 
  tmp <- NULL
  tmp <- mbind(tmp,setNames(inv_se(ie=petyf,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),               "Energy Investments|Elec|Fossil (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pecoal",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),            "Energy Investments|Elec|Coal (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pecoal",te=teccs,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),   "Energy Investments|Elec|Coal|w/ CCS (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pecoal",te=tenoccs,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv), "Energy Investments|Elec|Coal|w/o CCS (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pegas",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),             "Energy Investments|Elec|Gas (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pegas",te=teccs,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),    "Energy Investments|Elec|Gas|w/ CCS (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pegas",te=tenoccs,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),  "Energy Investments|Elec|Gas|w/o CCS (billion US$2005/yr)"))
  if ("h2turb" %in% all_te) {tmp <- mbind(tmp,setNames(inv_se(ie="seh2",oe="seel",se2se,adjte,v_directteinv,v_adjustteinv,te=se2se$all_te), "Energy Investments|Elec|Hydrogen (billion US$2005/yr)")) }
  tmp <- mbind(tmp,setNames(inv_se(ie="peoil",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),  "Energy Investments|Elec|Oil (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="peoil",te=tenoccs,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),  "Energy Investments|Elec|Oil|w/o CCS (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pebiolc",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),           "Energy Investments|Elec|Biomass (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pebiolc",te=teccs,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),  "Energy Investments|Elec|Biomass|w/ CCS (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pebiolc",te=tenoccs,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),"Energy Investments|Elec|Biomass|w/o CCS (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="peur",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),              "Energy Investments|Elec|Nuclear (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pesol",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),             "Energy Investments|Elec|Solar (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pewin",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),             "Energy Investments|Elec|Wind (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pehyd",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),             "Energy Investments|Elec|Hydro (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pegeo",oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),             "Energy Investments|Elec|Geothermal (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=NULL,oe=NULL,stor,adjte,v_directteinv,v_adjustteinv,te=all_te),         "Energy Investments|Elec|Storage (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=NULL,oe=NULL,c("tdels","tdelt",grid),adjte,v_directteinv,v_adjustteinv,te=all_te), "Energy Investments|Elec|Grid (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=pe2se$all_enty[-which(pe2se$all_enty %in% petyf)],oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),"Energy Investments|Elec|Non-Fossil (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=pe2se$all_enty[which(pe2se$all_enty %in% perenew)][-which(pe2se$all_enty[which(pe2se$all_enty %in% perenew)] %in% pebio)],oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv),"Energy Investments|Elec|Non-Bio Re (billion US$2005/yr)"))
  
  tmp <- mbind(tmp,setNames(inv_se(ie=pe2se$all_enty,oe="seh2",pe2se,adjte,v_directteinv,v_adjustteinv)
                          + inv_se(ie="seel",oe="seh2",se2se,adjte,v_directteinv,v_adjustteinv,te=se2se$all_te)
                          + inv_se(ie="seh2",oe=se2fe$all_enty1,se2fe,adjte,v_directteinv,v_adjustteinv,te=se2fe$all_te), "Energy Investments|Hydrogen (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=petyf,oe="seh2",pe2se,adjte,v_directteinv,v_adjustteinv),             "Energy Investments|Hydrogen|Fossil (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=perenew,oe="seh2",pe2se,adjte,v_directteinv,v_adjustteinv),           "Energy Investments|Hydrogen|RE (billion US$2005/yr)"))
  
  tmp <- mbind(tmp,setNames(inv_se(ie=pe2se$all_enty,oe=se_Liq,pe2se,adjte,v_directteinv,v_adjustteinv),    "Energy Investments|Liquids (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="peoil",oe=se_Liq,pe2se,adjte,v_directteinv,v_adjustteinv),           "Energy Investments|Liquids|Oil Ref (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=petyf,oe=se_Liq,pe2se,adjte,v_directteinv,v_adjustteinv),             "Energy Investments|Liquids|Fossil|w/ oil (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie="pecoal",oe=se_Liq,pe2se,adjte,v_directteinv,v_adjustteinv),          "Energy Investments|Liquids|Fossil (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames(inv_se(ie=perenew,oe=se_Liq,pe2se,adjte,v_directteinv,v_adjustteinv),           "Energy Investments|Liquids|Bio (billion US$2005/yr)"))

  tmp <- mbind(tmp,setNames((inv_se(ie=pe2se$all_enty,oe="seel",pe2se,adjte,v_directteinv,v_adjustteinv)
                           +inv_se(ie=se2se$all_enty,oe="seel",se2se,adjte,v_directteinv,v_adjustteinv, te=se2se$all_te)
                           +inv_se(ie=NULL,oe=NULL,c("tdels","tdelt",tenotransform),adjte,v_directteinv,v_adjustteinv,te=all_te)),"Energy Investments|Electricity (billion US$2005/yr)"))
  
  tmp <- mbind(tmp,setNames(v_directteinv[,,"ccsinje"] + v_adjustteinv[,,"ccsinje"],"Energy Investments|CO2 Trans&Stor (billion US$2005/yr)")*1000)
  tmp <- mbind(tmp,setNames(dimSums(v_directteinv[,,teue2rlf$all_te]+v_adjustteinv[,,teue2rlf$all_te],dim=3),"Energy Investments|Demand (billion US$2005/yr)")*1000)
  tmp <- mbind(tmp,setNames((inv_se(ie=temapall$all_enty,oe=temapall$all_enty1,temapall[-which(temapall$all_te %in% teue2rlf$all_te),],adjte,v_directteinv,v_adjustteinv,te=all_te)
                            +inv_se(ie=NULL,oe=NULL,tenotransform,adjte,v_directteinv,v_adjustteinv,te=all_te)),              "Energy Investments|Supply (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames((inv_se(ie=pe2se$all_enty,oe=se_Solids,pe2se,adjte,v_directteinv,v_adjustteinv)
                             +inv_se(ie=se_Solids,oe=se2fe$all_enty1,se2fe,adjte,v_directteinv,v_adjustteinv,te=se2fe$all_te)),  "Energy Investments|Solids (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames((inv_se(ie=pe2se$all_enty,oe=se_Gas,pe2se,adjte,v_directteinv,v_adjustteinv)
                             +inv_se(ie=se_Gas,oe=se2fe$all_enty1,se2fe,adjte,v_directteinv,v_adjustteinv,te=se2fe$all_te)),  "Energy Investments|Gases (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames((inv_se(ie=pe2se$all_enty,oe="sehe",pe2se,adjte,v_directteinv,v_adjustteinv)
                             +inv_se(ie="sehe",oe=se2fe$all_enty1,se2fe,adjte,v_directteinv,v_adjustteinv,te=se2fe$all_te)),  "Energy Investments|Heat (billion US$2005/yr)"))
  tmp <- mbind(tmp,setNames((tmp[,,"Energy Investments|Supply (billion US$2005/yr)"]
                            -tmp[,,"Energy Investments|Electricity (billion US$2005/yr)"]
                            -tmp[,,"Energy Investments|Hydrogen (billion US$2005/yr)"]
                            -tmp[,,"Energy Investments|Liquids (billion US$2005/yr)"]
                            -tmp[,,"Energy Investments|Heat (billion US$2005/yr)"]
                            -tmp[,,"Energy Investments|CO2 Trans&Stor (billion US$2005/yr)"]),  "Energy Investments|Other (billion US$2005/yr)"))
  
  
  # add global values
  tmp <- mbind(tmp,dimSums(tmp,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    tmp <- mbind(tmp, calc_regionSubset_sums(tmp, regionSubsetList))
  
  return(tmp)
}
