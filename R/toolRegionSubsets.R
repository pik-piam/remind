#' toolRegionSubsets
#' Returns a list of parent regions that are equal to a child mapping union of region mappings.
#'  
#' @param gdx a GDX as created by readGDX, or the file name of a gdx.
#' Gdx file containing child mapping regions list.
#' @param map alternative to gdx file. You can also provide a child mapping csv file.
#' @param parentMapping parent mapping or csv file (if NULL it uses the "regionmappingH12.csv").
#' @param singleMatches if true, includes single element matches in the return list.
#' Default: FALSE
#' @param removeDuplicates if true, removes from the return list identic matches (a region with
#' the same name and countries for both parent and child mappings). Default: TRUE
#' @param regionIndex Name or index of the region column to be used in the parent mapping (if not
#' set the column with less unique elements from the last two columns will be used). 
#' @param countryIndex Name or index of the country column to be used in  parent mapping (if not
#' set the column with more unique elements from the last two columns will be used). 
#'
#' @return return: returns a list of parent regions that are equal to a child mapping union
#' of region mappings. 
#' 
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{toolRegionSubsets(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom madrat toolGetMapping
#' @importFrom utils read.csv
#' 
#' @export

toolRegionSubsets <- function(gdx=NULL,map=NULL,parentMapping=NULL,singleMatches=FALSE, removeDuplicates=TRUE, regionIndex=NULL, countryIndex=NULL) {
  
  # default to "regionmappingH12.csv" as default
  if (is.null(parentMapping))
    parentMapping <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", returnPathOnly = TRUE)
  
  # reading parent mapping
  if(!(is.data.frame(parentMapping))) {
    if(!file.exists(parentMapping)) stop("Cannot find given region mapping file!")
    parentMapping <- read.csv(parentMapping, as.is = TRUE, sep = ";")     
  }
  if(is.null(regionIndex)) {
    regionIndex <- ifelse(dim(unique(parentMapping[ncol(parentMapping)]))[1] < dim(unique(parentMapping[ncol(parentMapping)-1]))[1],ncol(parentMapping),ncol(parentMapping)-1)
  }
  if(is.null(countryIndex)) {
    countryIndex <- ifelse(dim(unique(parentMapping[ncol(parentMapping)]))[1] > dim(unique(parentMapping[ncol(parentMapping)-1]))[1],ncol(parentMapping),ncol(parentMapping)-1)
  }
  
  #child mapping
  if (!(is.null(gdx))) {
    childMapping <- readGDX(gdx, name = "regi2iso")
  } else if (!(is.null(map))) {
    childMapping <- read.csv(map,sep=";")[,c(3,2)]
  } else {
    stop("You need to provide a gdx or a mapping file!")
  }
  
  # creating subsets list
  list <- list() 
  for (parentRegion in unique(parentMapping[[regionIndex]])){
    parentCountries <- parentMapping[parentMapping[[regionIndex]]==parentRegion,][[countryIndex]]
    regionsSubset <- NULL
    for (childRegion in unique(childMapping[[1]])){
      if ((removeDuplicates == FALSE) || (parentRegion != childRegion)) {
        childCountries <- childMapping[childMapping[[1]]==childRegion,][[2]]
        if (all(childCountries %in% parentCountries)){
          regionsSubset <- c(regionsSubset,childRegion)  
        }
      }
    }
    if ((length(regionsSubset) > 1) || ((length(regionsSubset) == 1) & (singleMatches==TRUE)))
      list[[parentRegion]] <- regionsSubset
  }
  if (length(list)==0) list <- NULL
  return(list)
}