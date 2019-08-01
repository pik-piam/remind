#' Run Statistics
#' 
#' Support function to collect run statistics. 
#' 
#' @param file file name the statistics are/should be stored
#' @param overwrite boolean deciding whether entries should be overwritten, if already existing
#' in the file. If set to FALSE an error will be thrown in case that an overwrite is attempted.
#' @param ... entries that should be added to the run statistics file. Standard entries are: 
#' model, config, runtime, user, date, modelstat, version_management, revision and status.
#' @param submit path to a folder the run statistics should be submitted to. As soon as the path is
#' set the data will be submitted, so please only set the path as soon as the run statistics are 
#' complete
#' @return An invisible list containing run statistics as stored in the given file
#' @author Jan Philipp Dietrich
#' @examples
#'  f <- tempfile()
#'  runstatistics(file=f, user=Sys.info()[["user"]])
#'  print(runstatistics(file=f))
#'  runstatistics(file=f,submit=tempdir())
#' @export

runstatistics <- function(file="runstatistics.Rda", overwrite=FALSE, submit=NULL, ...) {
  x <- list(...)
  stats <- list(submitted=FALSE)
  if(file.exists(file)) load(file)
  
  overlap <- intersect(names(x), names(stats))
  if(length(overlap)>0 & !overwrite) {
    stop("Attempt to overwrite the following entries with overwrite=FALSE: ", paste(overlap,collapse=", "))
  }
  stats[names(x)] <- x[names(x)]

  if(!is.null(submit)) {
    if(stats$submitted) stop("Statistics had been submitted already. Resubmission is currently not allowed. Please only submit your statistics when they are complete!")
    if(!dir.exists(submit)) {
      cat("Cannot access run statistics repository", submit, "Run statistics are not submitted.\n")
    } else {
      stats$submitted <- TRUE
      stats$id <- as.character(round(as.numeric(Sys.time())*100000))
      fname <- paste0(stats$id,".Rda")
      save(stats, file=paste0(submit,"/",fname), compress="xz")
      cat("Submitted run statistics to",submit,"\n")
    }
  }
    
  if(length(x)>0) save(stats, file=file, compress="xz")

  invisible(stats)  
}