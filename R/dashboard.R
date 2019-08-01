#' @title REMIND dashboard
#' @description Create REMIND dashboard results for single runs
#'
#' @author Renato Rodrigues
#' 
#' @param gdx GDX (fulldata.gdx) file path
#' @param statsFile run statistics (runstatistics.rda) file path
#' @param regionMapping regionMapping file name or file path (ex. "regionmappingH12.csv")
#' @param hist historical mif file path
#' @param reportfile REMIND mif report file path
#' @param output_file file name to save the html dashboard
#' 
#' @examples
#'   \dontrun{
#'     # loading required libraries
#'     library(remind)
#'     # creating the REMINd dashboard
#'     dashboard(gdx="./output/Base/fulldata.gdx",statsFile="./output/Base/runstatistics.rda",
#'               output_file="./output/Base/REMIND_dashboard_Base.html")
#'   }
#'
#' @importFrom rmarkdown render
#'
#' @export
#' 

dashboard <- function(gdx=NULL, statsFile=NULL, regionMapping=NULL, hist=NULL, reportfile=NULL, output_file=NULL) {

  # error checking
  if(!(!(is.null(gdx)) && file.exists(gdx))){
    warning("REMIND dashboard require a valid gdx file path.")
    return()
  }
  if(!(!(is.null(statsFile)) && file.exists(statsFile))){
    warning("REMIND dashboard require a valid run statistics (runstatistics.rda) file path.")
    return()
  }
  if(is.null(regionMapping)){
    warning("REMIND dashboard require a regionMapping file.")
    return()
  }
  
  stats <- NULL
  load(file = statsFile)
  if(stats$config$gms$optimization == "testOneRegi"){
    warning("REMIND dashboard does not support yet testOneRegi runs.")
    return()
  }
  
  #dashboard markdown file path
  markdownPath <- system.file("markdown","dashboard.Rmd",package = "remind")
  
  #set output file if null
  if(is.null(output_file))
     output_file <- file.path(getwd(),"REMIND_dashboard.html")
  
  # generate dashboard for REMIND
  rmarkdown::render(markdownPath, output_file = output_file, params = list(gdx=gdx, statsFile=statsFile, regionMapping=regionMapping, hist=hist, reportfile=reportfile))

}
