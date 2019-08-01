


#' Delete "+" in a *.mif reporting and save it with the addition "withoutPlus"
#' 
#' 
#' @param mif a path to a mif-file (might be created by confGDX2MIF) or a magpie object containing reporting
#' @param writemif a logical indicator whether the new mif with the adjusted names is saved under a new name containing "withoutPlus", if FALSE the new reporting is returnes as magpie object
#' @return MAgPIE object - contains the reporting without "+" and "++" in the variable names
#'  
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{deletePlus(mif)}
#'
#'
#' @export
#' @importFrom utils write.table

deletePlus <- function(mif,writemif=FALSE) {
  
  if(writemif == TRUE) {
    # read in mif-perorting
    rep <- read.csv(mif,sep=";",check.names=FALSE)
    
    # drop the empty last column of the stupid, no-good .mif format
    if (all(is.na(rep[,ncol(rep)])))
      rep[,ncol(rep)] <- ''
    
    # delete all "+" and "++" in variable names
    rep$Variable <- gsub("(\\+|\\++)\\|","",rep$Variable)
    # save reporting with new names under a new name
    oldName <- gsub(".mif","",mif)
    newName <- paste0(oldName,"_withoutPlus.mif")
    write.table(rep,file=newName,quote=FALSE,sep = ";",row.names = FALSE)
  } else if (is.magpie(mif)){
    # delete all "+" and "++" in variable names
    getNames(mif) <- gsub("(\\+|\\++)\\|","",getNames(mif))
    return(mif)
  } else {
    # read in mif-perorting
    rep <- read.report(mif,as.list=FALSE)
    # delete all "+" and "++" in variable names
    getNames(rep) <- gsub("(\\+|\\++)\\|","",getNames(rep))
    return(rep)
  }  
  
}