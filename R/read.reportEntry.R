#' Read entry in file of report format
#' 
#' This function reads one entry of a reporting file (a file in the model
#' intercomparison file format *.mif) into a MAgPIE object. This function can
#' be used by read_all() to read in the data for more than one output folder
#' 
#' 
#' @param outputdir output folder which contains the reporting
#' @param entry entry of the reporting that you want to read in
#' @param type type of the reporting that you want to read in
#' @author Lavinia Baumstark, David Klein
#' @examples
#' 
#' \dontrun{
#'   read.reportEntry("output/SSP2-ref",entry="Emi|Kyoto Gases (Mt CO2eq/yr)")
#'   read_all(outputdirs,read.reportEntry,entry="Emi|Kyoto Gases (Mt CO2eq/yr)",as.list=FALSE)
#' }
#' @export
#' @importFrom lucode2 getScenNames
#' @importFrom magclass read.report collapseNames
read.reportEntry <- function(outputdir,entry,type=NULL){
  fname <- file.path(outputdir,paste0("REMIND_generic_",getScenNames(outputdir)))
  if(is.null(type)){
    if (file.exists(paste0(fname,".mif"))) {
       mif_path <- paste0(fname,".mif")
    } else {
       mif_path <- paste0(fname,".csv")
    }
  }else{
    mif_path <- paste0(fname,type)
  }  
  mif <- read.report(mif_path,as.list=FALSE)
  mif <- collapseNames(mif)
  mif <- mif[,,entry]
  return(mif)
}









