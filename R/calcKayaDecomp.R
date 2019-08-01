#' Calculates Kaya decomposition
#' 
#' Calculates Kaya decomposition normaized to a ref_year: CO2 FF&I/FE, FE/GDP,
#' GDP/Population, Population
#' 
#' 
#' @param gdx path to the gdx
#' @param mif path to a mif or data in the mif-format (if provided, the mif will be used instead of the gdx)
#' @param ref_year reference year for the normalization, NULL for no normalization
#' @return magpie object with the kaya decomposition
#' @author Lavinia Baumstark
#' @examples
#' 
#'   \dontrun{
#'     kaya <- calcKayaDecomp(gdx,mif=NULL,ref_year=2005)
#'   }
#' 
#' @export
#' @importFrom magclass is.magpie read.report getRegions setCells getYears getNames<- collapseNames mbind getSets getSets<- setYears
calcKayaDecomp <- function(gdx,mif=NULL,ref_year=2005){
  
  if(is.null(mif)){
    # read data from gdx
    emiCO2 <- readEmissions(gdx,emiengregi="co2",eminegregi="co2cement")
    fe     <- readFE(gdx) 
    gdp    <- readGDPMER(gdx)
    pop    <- readPopulation(gdx)
  } else{
    if(!is.magpie(mif)){
      # read data from mif
      mif    <- read.report(mif,as.list=FALSE)
    }
    emiCO2 <- mif[,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"]
    fe     <- mif[,,"FE (EJ/yr)"]
    gdp    <- mif[,,"GDP|MER (billion US$2005/yr)"]
    pop    <- mif[,,"Population (million)"]
  }  
  
  # quick fix for the right regions
  reg <- intersect(getRegions(gdp),getRegions(fe))
  
  # calculate kaya decomposition
  Emi_FE <- setCells(emiCO2[reg,intersect(getYears(fe),getYears(gdp)),],getRegions(gdp[reg,,]))/setCells(fe[reg,intersect(getYears(fe),getYears(gdp)),],getRegions(gdp[reg,,]))
  getNames(Emi_FE) <- "CO2 FF&I/FE"
  if(is.null(ref_year)) { getNames(Emi_FE) <- "CO2 FF&I [Mt CO2/yr]/FE [EJ/yr]" }
  FE_GDP <- setCells(fe[reg,intersect(getYears(fe),getYears(gdp)),],getRegions(gdp[reg,,]))/setCells(gdp[reg,intersect(getYears(fe),getYears(gdp)),],getRegions(gdp[reg,,]))
  getNames(FE_GDP) <- "FE/GDP"
  if(is.null(ref_year)) { getNames(FE_GDP) <- "FE [EJ/yr]/GDP [billion US$2005/yr]" }
  GDP_POP <- (gdp[reg,intersect(getYears(fe),getYears(gdp)),])/pop[reg,intersect(getYears(fe),getYears(gdp)),]
  getNames(GDP_POP) <- "GDP/Population"
  if(is.null(ref_year)) { getNames(GDP_POP) <- "GDP [billion US$2005/yr]/Population [million]" }
  POP <- pop[reg,intersect(getYears(fe),getYears(gdp)),]
  getNames(POP) <- "Population"
  if(is.null(ref_year)) { getNames(POP) <- "Population [million]" }
  
  # combine to kaya
  kaya <- collapseNames(mbind(Emi_FE,FE_GDP,GDP_POP,POP))
  getSets(kaya)[length(getSets(kaya))] <- "data1"
  
  if(!is.null(ref_year)) {
    # nomalize to reference year
    kaya <- kaya / setYears(kaya[,ref_year,],NULL)
  }
  
  return(kaya)
}
