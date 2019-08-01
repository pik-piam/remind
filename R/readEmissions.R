#' Read Emissions from GDX file
#' 
#' Read emission data from a GDX file into a magpie object. summs automatically
#' over all kinds of enty and vm_emiengregi and vm_emiengregi
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @param emiengregi enty that is read in from vm_emiengregi, vector is
#' possible e.g. c("co2","n2o"). If you do not want to add an enty from
#' vm_emiengregi use emiengregi=NULL
#' @param eminegregi enty that is read in from vm_eminegregi, vector is
#' possible e.g. c("co2","co2cement"). If you do not want to add an enty from
#' vm_emiengregi use emiengregi=NULL
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{emi <- readEmissions(gdx,emiengregi=c("n2o","co2"),eminegregi=NULL)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass dimSums setNames getNames<-
readEmissions <- function(gdx,emiengregi,eminegregi){
  if(!is.null(emiengregi)){
    tmp1 <- readGDX(gdx, name = c("vm_emiTe","vm_emiengregi"), field="l", format="first_found")[ , , emiengregi]
    tmp1 <- dimSums(tmp1,dim=3.1)
  }
  if(!is.null(eminegregi)){
    tmp2 <- readGDX(gdx, name = c("vm_emiMacSector","vm_eminegregi"), field="l", format="first_found")[ , , eminegregi]
    tmp2 <- dimSums(tmp2,dim=3.1)
  }
  if( !is.null(emiengregi) && !is.null(eminegregi)){
    tmp  <- setNames(tmp1,NULL) + setNames(tmp2,NULL)
  }
  else if (!is.null(emiengregi) && is.null(eminegregi)){
    tmp  <- tmp1
  }
  else if (is.null(emiengregi) && !is.null(eminegregi)){
    tmp <- tmp2
  }  
  else if (is.null(emiengregi) && is.null(eminegregi)){
    stop("please choose at least one emisison type")
  } 
  names_emiengregi <- paste(emiengregi,collapse="+")
  names_eminegregi <- paste(eminegregi,collapse="+")
  getNames(tmp) <- paste(names_emiengregi,names_eminegregi,sep="&")
  return(tmp)
}

