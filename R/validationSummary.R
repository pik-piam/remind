#' @title validationSummary
#' @description Create validationSummary.pdf from REMIND output and corresponding historical.mif with most important variables
#'
#' @export
#'
#' @param gdx GDX file
#' @param hist  Validation data.All formats allowed which can be converted to quitte (including characters containing the path to a mif file)
#' @param outfile file name of the output pdf
#' @param reportfile file name to which a backup of the magpie reporting should be written (file ending should be ".mif"). No report written if set to NULL.
#' @param ... additional arguments supplied to the validationpdf function
#' @author David Klein, Lavinia Baumstark
#' @examples
#'
#'   \dontrun{
#'     validationSummary(gdx="fulldata.gdx",hist="historical.mif")
#'   }
#'
#' @importFrom lusweave swopen swlatex swclose swtable swfigure
#' @importFrom luplot plotcountrymap
#' @importFrom utils capture.output
#' @importFrom mip plotstyle mipArea
#' @importFrom ggplot2 theme
#' @importFrom magclass getYears

validationSummary <- function(gdx, hist, reportfile=NULL, outfile="validationSummary.pdf", ...) {

  # ---- If not provided produce REMIND mif now ----
  # if no reporting-mif-file is provided c reporting
  if(is.null(reportfile)){ 
    cat("No REMIND reporting file required for producing the validationSummary.pdf was provided. Calling convGDX2MIF to produce it.\n")
    reportfile <- convGDX2MIF(gdx)
  }
  
  data <- read.report(reportfile,as.list=FALSE)
  data <- collapseNames(data)
  data <- data[,getYears(data)<="y2100",]

  # ---- Start PDF ----
  template <-  c("\\documentclass[a4paper, portrait ]{article}",
                 "\\setlength{\\oddsidemargin}{-0.8in}",                                                                              
                 "\\setlength{\\evensidemargin}{-0.5in}",                                                                             
                 "\\setlength{\\topmargin}{-0.8in}",                                                                                  
                 "\\setlength{\\parindent}{0in}",                                                                                     
                 "\\setlength{\\headheight}{0in}",                                                                                    
                 "\\setlength{\\topskip}{0in}",                                                                                       
                 "\\setlength{\\headsep}{0in}",                                                                                       
                 "\\setlength{\\footskip}{0.2in}",                                                                                    
                 "\\setlength\\textheight{0.95\\paperheight}",                                                                        
                 "\\setlength\\textwidth{0.95\\paperwidth}",                                                                          
                 "\\setlength{\\parindent}{0in}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section,colorlinks=true,linkbordercolor={0.9882353 0.8352941 0.7098039}]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={GES group, PIK}}",
                 "\\usepackage{graphicx}",
                 "\\usepackage[strings]{underscore}",
                 "\\usepackage{Sweave}",                                                                                              
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=110)",
                 "@")
  
  sw <- swopen(outfile = outfile, template = template)
  swlatex(sw,c("\\title{Summary of REMIND results}","\\maketitle","\\tableofcontents"))
  #on.exit(swclose(sw, clean_output=FALSE, engine="knitr"))

  # ---- ToDo ----

  # - GLO-Figure zu gross: fig.width funktioniert nicht

  ### Area plots (including history if available)
  ### Seitenaufbau: oben: grosser Plot mit globalen Werten
  ###               unten: Regionen (per automatischem facet_wrap in mipArea)
  # - FE by sector
  # - non-electric SE
  # - Trade
  # - CH4 and N2O by source
  
  ### Line plots

  
  ### Further information
  # - Mitigation costs
  
  ### Wenn es Ampeln zur Bewertung der Uebereinstimmung mit der Historie gibt, die wichtigsten auf einer Seite sammeln

  # ---- Overview ----
  swlatex(sw,"\\section{Overview}")
  
  # ---- CO2 price (log) ----
  swlatex(sw,"\\subsection{CO2 prices}")
  swfigure(sw,mipLineHistorical,data["GLO",,"Price|Carbon (US$2005/t CO2)"],x_hist=NULL,
           ylab='Price|Carbon_log [US$2005/t CO2]',ybreaks=c(20,30,40,50,60,75,100,200,500,1000,2000,3000), 
           ylim=c(20,3000),ylog=TRUE,sw_option="height=10,width=9")
  

  # ---- CO2 emissions ----
  swlatex(sw,"\\subsection{CO2 emissions}")
    
  # calculate new variables -  now moved to reportCrossVariables.R

  var.tot <-"Emi|CO2 (Mt CO2/yr)"
  vars <- c("Emi|CO2|Energy|Supply|Non-Elec (Mt CO2/yr)",
            "Emi|CO2|Energy|Supply|Electricity|Gross (Mt CO2/yr)",
            "Emi|CO2|Energy|Demand|Industry|Gross (Mt CO2/yr)",
            "Emi|CO2|FFaI|Industry|Process (Mt CO2/yr)",
            #          "Emi|CO2|Industrial Processes (Mt CO2/yr)",
            "Emi|CO2|Buildings|Direct (Mt CO2/yr)",
            "Emi|CO2|Transport|Demand (Mt CO2/yr)",
             "Emi|CO2|Carbon Capture and Storage|Biomass|Neg (Mt CO2/yr)",
             "Emi|CO2|Land-Use Change (Mt CO2/yr)",
            NULL)
  
  
   p <- mipArea(data["GLO",,vars], total = data["GLO",,var.tot]) + theme(legend.position="none")
   swfigure(sw,print,p,fig.width=0.5)
   
   p <- mipArea(data[,,vars]["GLO",,invert=TRUE], total = data[,,var.tot]["GLO",,invert=TRUE]) 
   swfigure(sw,print,p,fig.width=1)


  # ---- PE ----
  swlatex(sw,"\\subsection{PE}")
  
  #var.tot <-"PE (EJ/yr)"
  vars <- c("PE|Coal|w/ CCS (EJ/yr)", 
            "PE|Coal|w/o CCS (EJ/yr)",
            "PE|+|Oil (EJ/yr)",
            "PE|Gas|w/ CCS (EJ/yr)", 
            "PE|Gas|w/o CCS (EJ/yr)",
            "PE|Biomass|w/ CCS (EJ/yr)", 
            "PE|Biomass|w/o CCS (EJ/yr)",
            "PE|+|Nuclear (EJ/yr)", 
            "PE|+|Hydro (EJ/yr)",
            "PE|+|Geothermal (EJ/yr)",
            "PE|+|Solar (EJ/yr)",  
            "PE|+|Wind (EJ/yr)")

  p <- mipArea(data["GLO",,vars], total = FALSE) + theme(legend.position="none")
  swfigure(sw,print,p,fig.width=0.5)
  
  p <- mipArea(data[,,vars]["GLO",,invert=TRUE], total = FALSE)
  swfigure(sw,print,p,fig.width=1)
  
  
  # ---- SE Electricity ----
  swlatex(sw,"\\subsection{SE Electricity}")
  
  var.tot <-"SE|Electricity (EJ/yr)"
  vars <- c("SE|Electricity|Coal|w/ CCS (EJ/yr)", 
            "SE|Electricity|Coal|w/o CCS (EJ/yr)",
            "SE|Electricity|Oil (EJ/yr)",
            "SE|Electricity|Gas|w/ CCS (EJ/yr)", 
            "SE|Electricity|Gas|w/o CCS (EJ/yr)",
            "SE|Electricity|Biomass|w/ CCS (EJ/yr)",  
            "SE|Electricity|Biomass|w/o CCS (EJ/yr)",
            "SE|Electricity|Nuclear (EJ/yr)", 
            "SE|Electricity|Hydrogen (EJ/yr)",
            "SE|Electricity|Solar (EJ/yr)", 
            "SE|Electricity|Wind (EJ/yr)",
            "SE|Electricity|Geothermal (EJ/yr)", 
            "SE|Electricity|Hydro (EJ/yr)"
  )
  
  p <- mipArea(data["GLO",,vars], total = data["GLO",,var.tot]) + theme(legend.position="none")
  swfigure(sw,print,p,fig.width=0.5)
  
  p <- mipArea(data[,,vars]["GLO",,invert=TRUE], total = data[,,var.tot]["GLO",,invert=TRUE])
  swfigure(sw,print,p,fig.width=1)

  # ---- FE ----
  swlatex(sw,"\\subsection{FE}")
  
  #var.tot <-"FE (EJ/yr)"
  vars <- c("FE|+|Solids (EJ/yr)",
            "FE|+|Liquids (EJ/yr)",
            "FE|+|Gases (EJ/yr)",
            "FE|+|Heat (EJ/yr)",
            "FE|+|Electricity (EJ/yr)",
            "FE|+|Hydrogen (EJ/yr)"#,
            #"FE|Solar (EJ/yr)",
            #"FE|Other (EJ/yr)"
  )
  
  p <- mipArea(data["GLO",,vars], total = FALSE) + theme(legend.position="none")
  swfigure(sw,print,p,fig.width=0.5)
  
  p <- mipArea(data[,,vars]["GLO",,invert=TRUE], total = FALSE)
  swfigure(sw,print,p,fig.width=1)
  
  
  # ---- FE Transport ----
  swlatex(sw,"\\subsection{FE Transport}")
  
  var.tot <-"FE|Transport (EJ/yr)"
  vars <- c("FE|Transport|Liquids|Oil (EJ/yr)",
            "FE|Transport|Liquids|Coal (EJ/yr)",
            "FE|Transport|Liquids|Biomass (EJ/yr)",
            #"FE|Transport|Gases (EJ/yr)",
            "FE|Transport|Electricity (EJ/yr)",
            "FE|Transport|Hydrogen (EJ/yr)"
  )
  
  p <- mipArea(data["GLO",,vars], total = data["GLO",,var.tot]) + theme(legend.position="none")
  swfigure(sw,print,p,fig.width=0.5)
  
  p <- mipArea(data[,,vars]["GLO",,invert=TRUE], total = data[,,var.tot]["GLO",,invert=TRUE])
  swfigure(sw,print,p,fig.width=1)

  #---- FE Industry ----
  swlatex(sw,"\\subsection{FE Industry}")
  
  var.tot <-"FE|Industry (EJ/yr)"
  vars <- c("FE|Industry|Electricity (EJ/yr)",
            "FE|Industry|Heat (EJ/yr)",
            "FE|Industry|Gases (EJ/yr)",
            "FE|Industry|Hydrogen (EJ/yr)",
            "FE|Industry|Liquids (EJ/yr)",
            "FE|Industry|Solids|Coal (EJ/yr)",
            "FE|Industry|Solids|Biomass (EJ/yr)"#,
            #"FE|Industry|Other (EJ/yr)"
  )
  
  p <- mipArea(data["GLO",,vars], total = data["GLO",,var.tot]) + theme(legend.position="none")
  swfigure(sw,print,p,fig.width=0.5)
  
  p <- mipArea(data[,,vars]["GLO",,invert=TRUE], total = data[,,var.tot]["GLO",,invert=TRUE])
  swfigure(sw,print,p,fig.width=1)

  #---- FE Residential and Commercial ----
  swlatex(sw,"\\subsection{FE Buildings}")
  
  var.tot <-"FE|Buildings (EJ/yr)"
  vars <- c("FE|Buildings|Electricity (EJ/yr)",
            "FE|Buildings|Heat (EJ/yr)",
            "FE|Buildings|Gases (EJ/yr)",
            "FE|Buildings|Hydrogen (EJ/yr)",
            "FE|Buildings|Liquids (EJ/yr)",
            "FE|Buildings|Solids|Coal (EJ/yr)",
            "FE|Buildings|Solids|Biomass (EJ/yr)"
  )
  
  # Global plot, FE
  p <- mipArea(data["GLO",,vars], total = data["GLO",,var.tot]) + theme(legend.position="none")
  swfigure(sw,print,p,fig.width=0.5)
  
  #Regional plots, FE
  p <- mipArea(data[,,vars]["GLO",,invert=TRUE], total = data[,,var.tot]["GLO",,invert=TRUE])
  swfigure(sw,print,p,fig.width=1)
 
  # ---- FE prices ----
  swlatex(sw,"\\subsection{FE prices}")
  vars_price <- c(grep("Price\\|Final Energy\\|(Transport|Buildings|Industry) \\(US\\$2005/GJ\\)$", getNames(data),value = T),
                  grep("Price\\|Final Energy\\|Electricity\\|(Transport|Buildings|Industry) \\(US\\$2005/GJ\\)$", getNames(data),value = T),
                  grep("Price\\|Final Energy\\|Liquids\\|(Transport|Buildings|Industry) \\(US\\$2005/GJ\\)$", getNames(data),value = T),
                  grep("Price\\|Final Energy\\|Heating Oil\\|(Transport|Buildings|Industry) \\(US\\$2005/GJ\\)$", getNames(data),value = T),
                  grep("Price\\|Final Energy\\|Gases\\|(Transport|Buildings|Industry) \\(US\\$2005/GJ\\)$", getNames(data),value = T)
  )
  
  for (var_price in   vars_price){
    if ( var_price %in% magclass::getNames(data))
      var_price_title = gsub("\\(" , "[" , var_price)
    var_price_title = gsub("\\)" , "]" , var_price_title)
    
    swfigure(sw,mipLineHistorical, data[,,var_price],x_hist=NULL,
             ylab = var_price_title, color.dim = "region", facet.dim = "scenario",
             color.dim.name = "Region",
             legend.ncol=8,plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
    
  }
  
  # ---- Meta Information ----
  swlatex(sw,"\\section{Meta information}")
  
  # ---- Region map ----
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

  # ---- Modelstatus ----
  swlatex(sw,"\\subsection{Modelstat}")
  repy <- readGDX(gdx,"p80_repy", react="silent")
  if(!is.null(repy)) {
    modstat <- repy[,,"modelstat"]
    swtable(sw,modstat,table.placement="H",caption.placement="top",transpose=TRUE,caption="main",vert.lines=1,align="c")
  } else {
    swlatex(sw,"Could not find p80_repy (only in Nash available) in gdx file!")
  }

  # ---- Objective function (global/regional welfare) ----
  swlatex(sw,"\\subsection{Objective function value}")
  welf <- readGDX(gdx,"v_welfare", react="silent")
  if(!is.null(welf)) {
    welf <- welf[,,"l"]
    swtable(sw,welf,table.placement="H",caption.placement="top",transpose=TRUE,caption="Welfare",vert.lines=1,align="c")
  } else {
    swlatex(sw,"Could not find welfare value (v_welfare) in gdx file!")
  }

  swclose(sw, clean_output=FALSE, engine="knitr")
}
