#' @title validation
#' @description Create Validation pdf from REMIND output and corresponding historical.mif
#'
#' @export
#'
#' @param gdx GDX file
#' @param hist  Validation data.All formats allowed which can be converted to quitte (including characters containing the path to a mif file)
#' @param file a file name the output pdf
#' @param reportfile file name to which a backup of the magpie reporting should be written (file ending should be ".mif"). No report written if set to NULL.
#' @param ... additional arguments supplied to the validationpdf function
#' @author Lavinia Baumstark
#' @examples
#'
#'   \dontrun{
#'     validation(gdx="fulldata.gdx",hist="historical.mif")
#'   }
#'
#' @importFrom lusweave swopen swlatex swclose swR swtable swfigure
#' @importFrom magclass getYears getRegions
#' @importFrom luplot plotcountrymap
#' @importFrom utils capture.output
#' @importFrom magclass write.report2
#' @importFrom mip plotstyle
#' @importFrom iamc iamCheck

validationREMIND <- function(gdx, hist, file="validation.pdf", reportfile=NULL, ...) {

  template <-  c("\\documentclass[a4paper, portrait ]{article}",
                 "\\setlength{\\parindent}{0in}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={PIK Landuse group}}",
                 "\\usepackage{graphicx}",
                 "\\usepackage{rotating}",
                 "\\usepackage[strings]{underscore}",
                 "\\usepackage[margin=2cm]{geometry}",
                 "\\usepackage{fancyhdr}",
                 "\\pagestyle{fancy}",
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=90)",
                 "@")

  sw <- swopen(outfile = file, template = template)
  swlatex(sw,c("\\title{REMIND run analysis}","\\maketitle","\\tableofcontents"))
  on.exit(swclose(sw, clean_output=FALSE, engine="knitr"))


  swlatex(sw,"\\part{Basics}")

  ######### Region map##############
  swlatex(sw,"\\subsection{World regions}")
  regi2iso <- readGDX(gdx,"regi2iso", react="silent")
  if(!is.null(regi2iso)) {
    map <- as.magpie(regi2iso[2:1],spatial=1)
    col <- plotstyle(levels(as.factor(regi2iso[[1]])))
    tmpplot <- function(...)  {
      a <- capture.output(plotcountrymap(...))
    }
    swfigure(sw,tmpplot,map,colourPalette=col,catMethod="categorical", mapTitle="", fig.orientation="landscape")
  } else {
    swlatex(sw,"Could not find mapping (regi2iso) between countries and regions in gdx file!")
  }

  ######### Modelstat and objective function value##############
  swlatex(sw,"\\subsection{Modelstat}")
  #Modelstatus
  repy <- readGDX(gdx,"p80_repy", react="silent")
  if(!is.null(repy)) {
    modstat <- repy[,,"modelstat"]
    swtable(sw,modstat,table.placement="H",caption.placement="top",transpose=TRUE,caption="main",vert.lines=1,align="c")
  } else {
    swlatex(sw,"Could not find p80_repy (only in Nash available) in gdx file!")
  }
  #global/regional welfare
  swlatex(sw,"\\subsection{Goal function value}")
  welf <- readGDX(gdx,"v_welfare", react="silent")
  if(!is.null(welf)) {
    welf <- welf[,,"l"]
    swtable(sw,welf,table.placement="H",caption.placement="top",transpose=TRUE,caption="Welfare",vert.lines=1,align="c")
  } else {
    swlatex(sw,"Could not find welfare value (v_welfare) in gdx file!")
  }

  ################### Validation output ################################
  # if no reporting-mif-file is provided calculate reporting
  if(is.null(reportfile)){ reportfile <- convGDX2MIF(gdx)}
  # generate list of variables based on the existing reporting
  rep <- read.report(reportfile,as.list=FALSE)
  rep <- getNames(rep,dim=3)
  rep <- as.data.frame(rep)
  names(rep) <- "variable"
  rep$unit <- rep$variable
  rep$variable <- gsub(" \\(.+$","",rep$variable)
  rep$unit     <- gsub("^.+ \\(","",rep$unit)
  rep$unit     <- gsub(")","",rep$unit)
  iamCheck(reportfile,pdf=sw, cfg=rep, refData=hist,pdfStyle = list("plot.priority"=c("x_hist","x","x_proj"),"years"=c(seq(2005,2060,5),seq(2070,2100,10))))


}
