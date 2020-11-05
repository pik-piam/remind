context("REMIND reporting")

test_that("Test if REMIND reporting is produced as it should", {

  library(gdx)

  ## add GDXs for comparison here:
  my_gdxs <- c(
    # system.file('extdata/old.gdx', package = 'remind'),
    NULL)

  runParallelTests <- function(gdxs=NULL,cores=0,par=FALSE){

    new_gdxs <- NULL
    if (is.null(gdxs) & file.exists("/p/projects/")) {
      gdxs <- system2("find","/p/projects/remind/runs/r* -name fulldata.gdx",stdout=T,stderr = FALSE)
      gdxs <- grep("magpie",gdxs,v=T,invert=T,ignore.case = T)
      gd <- length(gdxs)
      gdxs <- gdxs[c(gd)] # floor(gd/2),floor(3*gd/4),
      gdxs <- c(gdxs,"/p/projects/remind/runs/0AA_DONTDELETEME_MO/fulldata.gdx")
      brnam <- unlist(strsplit(gdxs,split="/"))
      new_gdxs <- paste0(brnam[grep("fulldata.gdx",brnam)-1],".gdx")
      file.copy(gdxs,new_gdxs,overwrite = TRUE)
      gdxs <- new_gdxs
    }
    i <- NULL
    # if (Sys.info()["sysname"]=="Linux") {
    #   par <- TRUE
    #   library(foreach)
    #   doMC::registerDoMC(cores = 2)
    # }
    if (par) {
      foreach (i = gdxs) %dopar% {
        cat(paste0(i,"\n"))
        a <- convGDX2MIF(i)
        cat("\n")
      }
    } else {
      for (i in gdxs) {
        cat(paste0(i,"\n"))
        a <- convGDX2MIF(i)
        cat("\n")
      }
    }
    unlink(new_gdxs)
  }

  expect_error(runParallelTests(my_gdxs),regexp = NA)

})
