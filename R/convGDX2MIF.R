#' Read in GDX and write *.mif reporting
#' 
#' Read in all information from GDX file and create
#' the *.mif reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param gdx_ref reference-gdx for policy costs, a GDX as created by readGDX, or the file name of a gdx
#' @param file name of the mif file which will be written, if no name is
#' provided a magpie object containing all the reporting information is
#' returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{convGDX2MIF(gdx,gdx_ref,file="REMIND_generic_default.csv",scenario="default")}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind write.report

convGDX2MIF <- function(gdx,gdx_ref=NULL,file=NULL,scenario="default",t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)) {
 
  # Define region subsets
  regionSubsetList <- toolRegionSubsets(gdx)
    
  # make the reporting
  output <- NULL
  output <- mbind(output,reportMacroEconomy(gdx,regionSubsetList)[,t,])
  output <- mbind(output,reportTrade(gdx,regionSubsetList)[,t,])
  output <- mbind(output,reportTax(gdx,regionSubsetList)[,t,])
    
  output <- mbind(output,reportPE(gdx,regionSubsetList)[,t,])
  output <- mbind(output,reportSE(gdx,regionSubsetList)[,t,])
  output <- mbind(output,reportFE(gdx,regionSubsetList)[,t,])
  output <- mbind(output,reportExtraction(gdx,regionSubsetList)[,t,])
  output <- mbind(output,reportCapacity(gdx,regionSubsetList)[,t,])
  #output <- mbind(output,reportLCOE(gdx)[,t,])     now moved to additional LCOE.mif file because many variables
  output <- mbind(output,reportCapitalStock(gdx,regionSubsetList)[,t,])
  output <- mbind(output,reportEnergyInvestment(gdx,regionSubsetList)[,t,])
  tmp <- try(reportEmiAirPol(gdx,regionSubsetList))  # test whether reportEmiAirPol works
  if(class(tmp)!="try-error") {
     if(!is.null(reportEmiAirPol(gdx,regionSubsetList))) output <- mbind(output, reportEmiAirPol(gdx,regionSubsetList)[,t,])
  } else {cat("function reportEmiAirPol does not work and is skipped \n")}   
  
  # reporting of variables that need variables from different other report functions
  output <- mbind(output,reportEmi(gdx,output,regionSubsetList)[,t,])    # needs output from reportFE
  output <- mbind(output,reportTechnology(gdx,output,regionSubsetList)[,t,])    # needs output from reportSE
  output <- mbind(output,reportPrices(gdx,output,regionSubsetList)[,t,]) # needs output from reportSE, reportFE, reportEmi, reportExtraction, reportMacroEconomy  
  output <- mbind(output,reportCosts(gdx,output,regionSubsetList)[,t,])  # needs output from reportEnergyInvestment, reportPrices, reportEnergyInvestments

  # reporting of cross variables - needs variables from different other report* functions
  output <- mbind(output,reportCrossVariables(gdx,output,regionSubsetList)[,t,])

  if(!is.null(gdx_ref)) {
    if (file.exists(gdx_ref)) {
      output <- mbind(output,reportPolicyCosts(gdx,gdx_ref,regionSubsetList)[,t,])
    } else {
      warning(paste0("File ",gdx_ref," not found. Did not execute 'reportPolicyCosts'!"))
    }
  }

  # Add dimension names "scenario.model.variable"
  getSets(output)[3] <- "variable"
  output <- add_dimension(output,dim=3.1,add = "model",nm = "REMIND")
  output <- add_dimension(output,dim=3.1,add = "scenario",nm = scenario)
  
  # either write the *.mif or return the magpie object
  if(!is.null(file)) {
    write.report(output,file=file,ndigit=7)
    # write same reporting without "+" or "++" in variable names
    deletePlus(file,writemif=TRUE)
  } else {
    return(output)
  }  
}
