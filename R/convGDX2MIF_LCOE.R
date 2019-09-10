#' Read in GDX and write LCOE .mif reporting
#' 
#' Read in all information from GDX file, calculate the LCOE for pe2se technologies and create
#' the LCOE .mif reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param gdx_ref a GDX as created by readGDX of the reference run
#' @param file name of the mif file which will be written, if no name is
#' provided a magpie object containing all the reporting information is
#' returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2030,2050)
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{convGDX2MIF(gdx,gdx_ref,file="REMIND_generic_LCOE.csv",scenario="default")}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind write.report

convGDX2MIF_LCOE <- function(gdx,gdx_ref,file=NULL,scenario="default",t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)) {
 

  # make the reporting
  output <- NULL
  output <- mbind(output,reportLCOE(gdx)[,t,])   

  # write the LCOE.mif or give back the magpie opject output
  if(!is.null(file)) {
    write.report(output,model="REMIND",scenario=scenario,file=file,ndigit=7)
  } else {
    return(output)
  }
  
}
