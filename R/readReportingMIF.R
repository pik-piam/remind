#' Reads in a valid MIF (or CSV) reporting file
#' 
#' read in a valid MIF (or CSV) reporting file and return data as a
#' quitte-object
#' 
#' 
#' @param pathToMIF character. Path to a valid MIF reporting file.
#' @param RData logical. if true data is saved in Rdata format to save read-in
#' time
#' @param verbose logical. If true (un)helpful information will be display.
#' @return quitte object
#' @author Anselm Schultes, Jerome Hilaire, Lavinia Baumstark, David Klein
#' @examples
#' 
#'   \dontrun{
#'     qd <- readReportingMIF('/my/magic/remind/run/REMIND_generic.mif')
#'     qd <- readReportingMIF('/my/magic/remind/run/REMIND_generic.csv')
#'   }
#' 
#' @export
#' @importFrom tools md5sum
#' @importFrom utils read.csv
#' @importFrom reshape2 melt
readReportingMIF <- function(pathToMIF, RData=FALSE, verbose=TRUE){
  #error handling
  #check that the file exists
  if(!file.exists(pathToMIF)) stop(paste("The specified file", pathToMIF, "could not be found!"))
  #check that the file is a valid mif file
  flag_valid <- TRUE
  colnames <- gsub("\"", "", strsplit(readLines(pathToMIF, n=1),';|,')[[1]])
  if(!all(c("model","scenario","region","variable", "unit") %in% tolower(colnames))) {
    warning(paste("The specified file", pathToMIF, "is not a valid mif file!"))
    flag_valid <- FALSE
  }
  
  #if mif file is valid
  if(flag_valid) {  
    #helper function to count occurences of a charcater in a string
    nchar2 <- function(char, s) {
      s2 <- gsub(char,"",s)
      return (nchar(s) - nchar(s2))
    }
    
    #if the RData flag has been switched on
    if (RData) {
      #generate RData and md5 file paths
      pathToRData <- file.path(dirname(pathToMIF), paste0(strsplit(basename(pathToMIF),".",fixed=T)[[1]][1], ".RData"))
      pathToMD5   <- file.path(dirname(pathToMIF), paste0(strsplit(basename(pathToMIF),".",fixed=T)[[1]][1], ".md5"))
      
      #if a RData file already exists and the flag overwrite has not been activated
      if (file.exists(pathToRData) && md5sum(pathToMIF) == scan(file=pathToMD5, what=character(), n=1, quiet=T)) {
        if (verbose) cat(paste0("Reading data from the RData file: ", pathToRData, "\n"))
  
        #load data from the RData file
        load(pathToRData)
      } else {
        if (verbose) cat(paste0("Reading data from mif file: ", pathToMIF, " (an RData file will be generated)\n"))
        
        #determine seperator and number of columns
        line1 <- readLines(pathToMIF,n=1)
        sep   <- c(';',',')[which.max(sapply(c(';',','), function (f) nchar2(f,line1)))]
        nbcol <- length(strsplit(line1, sep, fixed=TRUE)[[1]])
        
        #read in mif
        tmp <- read.csv(pathToMIF,check.names=FALSE,row.names=NULL,header=TRUE,
                        strip.white=TRUE,na.strings=c('n_a','UNDF','N/A'),
                        sep=sep,stringsAsFactors=FALSE, 
                        colClasses=c(rep("character", 5), rep("numeric", nbcol-5)))
                       
        #remove last colum, if empty:
        if(names(tmp)[[length(tmp)]] == "") tmp = tmp[,-length(tmp)]
        #convert to long format
        tmp <- melt(tmp,id.vars=names(tmp)[1:5])
        #rename colum names accordingly:
        names(tmp) <- c('model','scenario','region','variable','unit','period','value')
        #convert time to integers
        tmp$period = as.integer(as.character(tmp$period))
        #rename glob to GLO
        tmp$region = factor(tmp$region)
        levels(tmp$region)[levels(tmp$region) == 'glob'] <- 'GLO'
        #convert to quitte
        tmp <- quitte::as.quitte(tmp)
        
        #save data in the RData file
        save(tmp, file=pathToRData)
        
        #save md5 sum
        write(md5sum(pathToMIF), file=pathToMD5)
      }
    } else {
      if (verbose) cat(paste0("Reading data from mif file: ", pathToMIF, "\n"))
      
      #determine seperator and number of columns
      line1 <- readLines(pathToMIF,n=1)
      sep   <- c(';',',')[which.max(sapply(c(';',','), function (f) nchar2(f,line1)))]
      nbcol <- length(strsplit(line1, sep, fixed=TRUE)[[1]])
      
      #read in mif
      tmp <- read.csv(pathToMIF,check.names=FALSE,row.names=NULL,header=TRUE,
                      strip.white=TRUE,na.strings=c('n_a','UNDF','N/A'),
                      sep=sep,stringsAsFactors=FALSE, 
                      colClasses=c(rep("character", 5), rep("numeric", nbcol-5)))
                     
      #remove last colum, if empty:
      if(names(tmp)[[length(tmp)]] == "") tmp = tmp[,-length(tmp)]
      #convert to long format
      tmp <- melt(tmp,id.vars=names(tmp)[1:5])
      #rename colum names accordingly:
      names(tmp) <- c('model','scenario','region','variable','unit','period','value')
      #convert time to integers
      tmp$period = as.integer(as.character(tmp$period))
      #rename glob to GLO
      tmp$region = factor(tmp$region)
      #TODO We may want to change 'GLO' by 'World' at some point 
      levels(tmp$region)[levels(tmp$region) == 'glob'] <- 'GLO'
      #convert to quitte
      tmp <- quitte::as.quitte(tmp)
    }
  
    #return quitte object
    return(tmp)
  } else {
    if (verbose) cat(paste0("Discarding mif file: ", pathToMIF, "\n"))
    return(NULL)
  }
}
