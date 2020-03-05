
#' Copy fulldata.gdx and mif files from a suite of runs into one folder. This function creates the folder "./data/" in
#' your working directory if such folder does not exist. It will furthermore create a subfolder with the experiment name where the gdx and mif-files 
#' will be copied to. Care: If this subfolder is already existing, old files in this subfolder will be overwritten!
#' 
#' 
#' @param output.folder a vector with the paths to REMIND output folders of the desired runs
#' @param experiment name of this experiment (suite of runs)
#' @return no return, function creates data folder and copies output files into it
#'  
#' @author Felix Schreyer
#' @examples
#' 
#' \dontrun{copyMIFGDX(c("Z:/Modeling/REMIND/output/BAU_2019-12-03_14.30.56/",
#'                        "Z:/Modeling/REMIND/output/NDC_2019-12-03_15.20.53/"),
#'                      experiment = "NewFeature_03_12_19")}
#' @export
#' 


getRunsMIFGDX <- function(output.folder, experiment) {
  
  # create data folder if it does not exist
  if (!dir.exists("./data")) {
    dir.create(paste0("./data"))
  }
  
  download.folder <- paste0("./data/", experiment)
  
  if (dir.exists(download.folder)) {
    # delete existing files in folder
    file.remove(paste0(download.folder, "/", list.files(download.folder)))
  } else {
    # create experiments folder
    dir.create(paste0("./data/", experiment))
  }

 
  # copy .mif and .gdx files to folder
  output.files <- unlist(c(lapply(output.folder, 
                                  FUN=function(x){list.files(x,pattern="REMIND_generic*", full.names = T)}),
                           lapply(output.folder, 
                                  FUN=function(x){list.files(x,pattern="REMIND_LCOE*", full.names = T)}),
                           lapply(output.folder, 
                                  FUN=function(x){list.files(x,pattern="historical.mif", full.names = T)}),
                           lapply(output.folder, 
                                  FUN=function(x){list.files(x,pattern="fulldata*", full.names = T)})))
  output.files <- output.files[ !grepl("withoutPlus", output.files) ]
  output.files <- output.files[ !grepl("climate", output.files) ]

  for (i in 1:length(output.files)) {
    file.copy(output.files[i], paste0("./data/", experiment), overwrite = T)
    
    scen.name <- sub(".*\\boutput/*(\\w+).*","\\1",output.files[i])
    scen.name <- substr(scen.name,1, nchar(scen.name)-5)
    file.rename(paste0( paste0("./data/", experiment), "/fulldata.gdx"),
                paste0( paste0("./data/", experiment), "/fulldata_",scen.name,".gdx"))
  }
  
  print(paste0("Data were copied into ", "./data/", experiment,"/"))
}


