#' Reads in all valid MIF reporting files in a folder
#' 
#' read in all valid MIF reporting files in a folder and return data as a
#' quitte-object. If no mif exists it searches for CSV
#' 
#' 
#' @param dir character. Directory containing the MIF files.
#' @param RData logical. If true data is saved in Rdata format to save read-in
#' time.
#' @param verbose logical. If true (un)helpful information will be display.
#' @return quitte object
#' @author Anselm Schultes, Jerome Hilaire, Lavinia Baumstark, David Klein
#' @examples
#' 
#'   \dontrun{
#'     qd <- readAllReportingMIFinFolder('/my/magic/remind/run/')
#'   }
#' 
#' @export
#' @importFrom tools md5sum

readAllReportingMIFinFolder <- function(dir, RData=FALSE, verbose=TRUE){
  #Returns a quitte object
  filelist <- list.files(dir, '*.mif', full.names=TRUE)

  #error handling
  if (length(filelist)==0) {
    filelist <- list.files(dir, '*.csv', full.names=TRUE)
  }
  
  if (length(filelist)==0) {
    stop(paste("No mif or csv files found in directory", dir))
  }
  

  #if the RData flag has been switched on
  if (RData) {
    #generate RData and md5 file paths
    pathToRData <- file.path(dir, paste0(strsplit(basename(dir),".",fixed=T)[[1]][1], ".RData"))
    pathToMD5   <- file.path(dir, paste0(strsplit(basename(dir),".",fixed=T)[[1]][1], ".md5"))

    oldmd5ums <- rep("", length(filelist)) 
    if(file.exists(pathToMD5)) oldmd5ums <- readLines(pathToMD5)
    
    #if a RData file already exists and the flag overwrite has not been activated
    if (file.exists(pathToRData) && all(sapply(filelist, md5sum) == oldmd5ums)) {
      if (verbose) cat(paste("Reading data from the RData file: ", pathToRData, "\n"))

      #load data from the RData file
      load(pathToRData)      
    } else {
      if (verbose) cat("Reading data from mif files and save them in the RData format...\n")
      
      #read all those files
      data <- do.call("rbind", lapply(filelist, readReportingMIF, RData=FALSE, verbose=verbose))
      
      #save data in the RData file
      save(data, file=pathToRData)
      
      #save md5sum of mif files
      md5sums <- sapply(filelist, md5sum)
      write(md5sums, file=pathToMD5)
    }
    
  } else {
    if (verbose) cat("Reading data from mif files...\n")
    
    #read all those files
    data <- do.call("rbind", lapply(filelist, readReportingMIF, RData=FALSE, verbose=verbose))
  }
  
  
  #return quitte
  return(data)  
}
