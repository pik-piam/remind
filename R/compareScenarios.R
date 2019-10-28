#' Read in mif and write comparison.pdf
#'
#' Read in all information from mif file and create
#' the comparison.pdf
#'
#' @param mif a path to one or more mif-files (might be created by confGDX2MIF)
#' @param hist a path to one mif-file containing historical data
#' @param y time span for the data in line plots, default: y=c(seq(2005,2060,5),seq(2070,2100,10))
#' @param y_hist time span for the historical data in the line plots, default: c(seq(1960,2015,1))
#' @param y_bar time slides for bar plots, default: y_bar=c(2010,2030,2050,2100)
#' @param reg region(s) in focus, reg ="all_regi" shows all regions if the mifs contain different regions
#' @param mainReg region to be underlined
#' @param fileName name of the pdf, default = "CompareScenarios.pdf"
#' @param sr15marker_RCP if given, show the corresponding marker scenarios (SSP1-5) from the SR15 database in some plots. Requires the sr15data package.
#'
#' @author Lavinia Baumstark
#' @examples
#'
#' \dontrun{compareScenarios(mif_path)}
#'
#' @export
#' @importFrom magclass read.report mbind getRegions new.magpie getYears add_dimension setNames getNames<- time_interpolate
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom mip mipLineHistorical mipBarYearData plotstyle
#' @importFrom luplot magpie2ggplot2
#' @importFrom ggplot2 facet_grid ggplot geom_col facet_wrap geom_point aes_ geom_ribbon
#' @importFrom quitte as.quitte
#' @importFrom data.table as.data.table setnames := data.table
#' @importFrom utils installed.packages

compareScenarios <- function(mif, hist,
                             y=c(seq(2005,2060,5),seq(2070,2100,10)),
                             y_hist=c(seq(1960,2015,1)),
                             y_bar=c(2010,2030,2050,2100),
                             reg=NULL, mainReg="GLO", fileName="CompareScenarios.pdf",
                             sr15marker_RCP=NULL) {

  lineplots_perCap <- function(data, vars, percap_factor, ylabstr,
                               global=FALSE, per_gdp=FALSE, histdata=NULL){
    items <- c(vars,
               "Population (million)",
               "GDP|PPP (billion US$2005/yr)")
    var <- as.data.table(as.quitte(data[,, items]))[, "unit" := NULL]

    if(!is.null(histdata)){
      if(all(items %in% getNames(histdata, dim=3))){
        hist_dt <- as.data.table(as.quitte(histdata[,, items]))
        varhist <- hist_dt[model != "EDGE_SSP1"][, c("unit", "model") := list(NULL, "REMIND")]
        var <- rbind(var, varhist)
      }else{
        print(sprintf("Items %s not found in historical data.", items))
      }
    }

    plain_vars <- gsub("(.+) \\(.+\\)", "\\1", vars)

    variable <- Population <- NULL
    region <- `GDP|PPP` <- model <- value <- scenario <- NULL

    hvar <- data.table::dcast(var, ... ~ variable)

    for(fe in plain_vars){
      hvar[, (fe) := get(fe)/Population*percap_factor]
    }

    if(per_gdp){
      hvar[, `GDP|PPP` := `GDP|PPP`/Population]
      var <- data.table::melt(hvar, id.vars=c("model", "scenario", "region", "period", "GDP|PPP"))
    }else{
      hvar[, `GDP|PPP` := NULL]
      var <- data.table::melt(hvar, id.vars=c("model", "scenario", "region", "period"))
    }

    var <- var[variable != "Population"][
    , variable := factor(variable, levels=plain_vars)]

    highlight_yrs <- c(2030, 2050, 2070)
    highlights <- var[scenario != "historical" & period %in% highlight_yrs]

    reg_cols <- plotstyle(as.character(unique(var$region)))
    reg_labels <- plotstyle(as.character(unique(var$region)), out="legend")

    var <- var[value > 0]
    if(per_gdp){
      if(global){
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region == "GLO"],
                    aes(x=`GDP|PPP`, y=value, linetype=scenario)) +
          geom_point(data=var[scenario == "historical" & region == "GLO"],
                     aes(x=`GDP|PPP`, y=value), shape=4) +
          geom_point(data=highlights[region == "GLO"], aes(x=`GDP|PPP`, y=value), shape=1)
      }else{
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region != "GLO"],
                    aes(x=`GDP|PPP`, y=value, linetype=scenario, color=region)) +
          geom_point(data=var[scenario == "historical" & region != "GLO"],
                     aes(x=`GDP|PPP`, y=value, color=region), shape=4) +
          geom_point(data=highlights[region != "GLO"], aes(x=`GDP|PPP`, y=value, color=region), shape=1) +
          scale_color_manual(values = reg_cols,  labels = reg_labels)
      }

      p <- p +
        facet_wrap(~ variable, scales="free_y") +
        ylab(ylabstr) +
        xlab("GDP PPP per Cap. (kUS$2005)") +
        theme_minimal()

    }else{
      if(global){
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region == "GLO"],
                    aes(x=period, y=value, linetype=scenario)) +
          geom_point(data=var[scenario == "historical" & region == "GLO"],
                     aes(x=period, y=value), shape=4)
      }else{
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region != "GLO"],
                    aes(x=period, y=value, linetype=scenario, color=region)) +
          geom_point(data=var[scenario == "historical" & region != "GLO"],
                     aes(x=period, y=value, color=region), shape=4) +
          scale_color_manual(values = reg_cols,  labels = reg_labels)
      }
      p <- p +
        facet_wrap(~ variable, scales="free_y") +
        xlab("year") +
        ylab(ylabstr) +
        theme_minimal()

    }
    return(p)
  }


  ## ---- Read data ----

  ## read model results
  data <- NULL
  for(i in 1:length(mif)){
    data_new <- read.report(mif[i],as.list=FALSE)
    if (magclass::getNames(data_new,fulldim = TRUE)[["scenario"]] %in% magclass::getNames(data,fulldim = TRUE)[["scenario"]]) magclass::getNames(data_new) <- gsub(magclass::getNames(data_new,fulldim = TRUE)["scenario"],paste0(magclass::getNames(data_new,fulldim = TRUE)["scenario"],i),magclass::getNames(data_new))
    if(all(getRegions(data) %in% getRegions(data_new))) {
      data <- mbind(data,data_new)
    } else {
      if(is.null(reg)){
        stop("the regional aggregation of the results are different, you might use reg='all_reg'")
      } else if(reg=="all_reg"){
        if(all(getRegions(data_new) %in% getRegions(data))) {
          ## expand data_new by old regions from data
          oldReg         <- getRegions(data)[-which(getRegions(data) %in% getRegions(data_new))]
          dummy_data_new <- new.magpie(oldReg,getYears(data_new),getNames(data_new),fill=NA)
          data_new       <- mbind(data_new,dummy_data_new)
          ## compine old and new data
          data <- mbind(data,data_new)
        } else {
          ## expand data by new regions from data_new
          newReg     <- getRegions(data_new)[-which(getRegions(data_new) %in% getRegions(data))]
          dummy_data <- new.magpie(newReg,getYears(data),getNames(data),fill=NA)
          data       <- mbind(data,dummy_data)
          ## expand data_new by old regions from data
          oldReg         <- getRegions(data)[-which(getRegions(data) %in% getRegions(data_new))]
          dummy_data_new <- new.magpie(oldReg,getYears(data_new),getNames(data_new),fill=NA)
          data_new       <- mbind(data_new,dummy_data_new)
          ## compine old and new data
          data <- mbind(data,data_new)
        }

      } else {
        stop("the regional aggregation of the results are different, you might use reg='all_reg'")
      }
    }
  }


  if (!(is.null(reg))) {
    if (!reg=="all_reg") {
      data <- data[reg,y,]
    } else {
      data <- data[,y,]
    }
  } else {
    data <- data[,y,]
  }
  ## delete "+" and "++" from variable names
  data <- deletePlus(data)

  ## read historical data
  hist <- read.report(hist,as.list=FALSE)
  if(all(getRegions(data) %in% getRegions(hist))) {
    hist = hist[getRegions(data),,]
    if ( any(grepl("EDGE_SSP2",getNames(hist)))){
      hist_edge = hist[,union(y_hist,y),]
      hist = hist[,,"EDGE_SSP2", invert = T]
    }
    hist <- hist[,y_hist,]
  } else {
    if(!is.null(reg)){
      ## fill up historical data for additional regions with 0
      dataReg    <- getRegions(data)[-which(getRegions(data) %in% getRegions(hist))]
      dummy_hist <- new.magpie(dataReg,getYears(hist),getNames(hist),fill=NA)
      hist       <- mbind(hist,dummy_hist)
      hist = hist[getRegions(data),,]
      if ( any(grepl("EDGE_SSP2",getNames(hist)))){
        ##EDGE projections are stored in hist. Retrieve them
        hist_edge = hist[,union(y_hist,y),]
        hist = hist[,,"EDGE_SSP2", invert = T]
      }
      hist <- hist[,y_hist,]


    } else {
      stop("historical data do not contain the choosen region")
    }
  }

  ## ---- Open output-pdf ----

  template <-  c("\\documentclass[a4paper,landscape,twocolumn]{article}",
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

  sw <- swopen(fileName,template = template)
  swlatex(sw,"\\tableofcontents\\newpage")

  ## ---- ++++ S U M M A R Y ++++ ----

  swlatex(sw,"\\section{Summary}")

  ## ---- GHG total ----

  swlatex(sw,"\\subsection{GHG Emissions}")

  GWP <- c("CO2"=1,"CH4"=28,"N2O"=265)
  var <- NULL
  var <- mbind(var,data[,,"Emi|CO2|Land-Use Change (Mt CO2/yr)"]                   *GWP["CO2"])
  var <- mbind(var,data[,,"Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)"]   *GWP["CO2"])
  var <- mbind(var,data[,,"Emi|CO2|Carbon Capture and Storage|Biomass (Mt CO2/yr)"]*-GWP["CO2"])
  var <- mbind(var,data[,,"Emi|CH4|Energy Supply and Demand (Mt CH4/yr)"]          *GWP["CH4"])
  var <- mbind(var,data[,,"Emi|CH4|Land Use (Mt CH4/yr)"]                          *GWP["CH4"])
  var <- mbind(var,data[,,"Emi|CH4|Other (Mt CH4/yr)"]                             *GWP["CH4"])
  var <- mbind(var,data[,,"Emi|CH4|Waste (Mt CH4/yr)"]                             *GWP["CH4"])
  var <- mbind(var,data[,,"Emi|N2O|Land Use (kt N2O/yr)"]                          *GWP["N2O"]/1000)
  var <- mbind(var,data[,,"Emi|N2O|Energy Supply and Demand (kt N2O/yr)"]          *GWP["N2O"]/1000)
  var <- mbind(var,data[,,"Emi|N2O|Waste (kt N2O/yr)"]                             *GWP["N2O"]/1000)
  var <- mbind(var,data[,,"Emi|N2O|Industry (kt N2O/yr)"]                          *GWP["N2O"]/1000)
  var <- setNames(var,gsub(" \\(.*\\)"," (Mt CO2eq/yr)",magclass::getNames(var)))

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- CO2 by sector ----

  swlatex(sw,"\\subsection{CO2 by sector}")

  tot <-"Emi|CO2 (Mt CO2/yr)"
  items <- c("Emi|CO2|Land-Use Change (Mt CO2/yr)",
             "Emi|CO2|Energy|Supply|Non-Elec (Mt CO2/yr)",
             "Emi|CO2|Energy|Supply|Electricity|Gross (Mt CO2/yr)",
             "Emi|CO2|Energy|Demand|Industry|Gross (Mt CO2/yr)",
             "Emi|CO2|FFaI|Industry|Process (Mt CO2/yr)",
             ##          "Emi|CO2|Industrial Processes (Mt CO2/yr)",
             "Emi|CO2|Buildings|Direct (Mt CO2/yr)",
             "Emi|CO2|Transport|Demand (Mt CO2/yr)",
             "Emi|CO2|Carbon Capture and Storage|Biomass|Neg (Mt CO2/yr)",
             "Emi|CO2|CDR|DAC (Mt CO2/yr)",
             "Emi|CO2|CDR|EW (Mt CO2/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- CO2 by sector cumulated ----

  items <- c("Emi|CO2|Land-Use Change|Cumulated (Mt CO2/yr)",
             "Emi|CO2|Energy|Supply|Non-Elec|Cumulated (Mt CO2/yr)",
             "Emi|CO2|Energy|Supply|Electricity|Gross|Cumulated (Mt CO2/yr)",
             "Emi|CO2|Energy|Demand|Industry|Gross|Cumulated (Mt CO2/yr)",
             "Emi|CO2|FFaI|Industry|Process|Cumulated (Mt CO2/yr)",
             "Emi|CO2|Buildings|Direct|Cumulated (Mt CO2/yr)",
             "Emi|CO2|Transport|Demand|Cumulated (Mt CO2/yr)",
             "Emi|CO2|CDR|BECCS|Cumulated (Mt CO2/yr)",
             "Emi|CO2|CDR|DAC|Cumulated (Mt CO2/yr)",
             "Emi|CO2|CDR|EW|Cumulated (Mt CO2/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- FE by sector ----

  swlatex(sw,"\\subsection{FE by sector}")

  items<- c("FE|Transport (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE per capita by sector (time domain, area graph)----

  swlatex(sw,"\\subsection{FE per capita (by sector, time domain, area plot)}")

  items<- c("FE|Transport (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)")
  var <- data[,,intersect(items, getNames(data,dim=3))]/data[,, "Population", pmatch=T]*1e3

  p <- mipArea(var["GLO",,], scales="free_y")
  p <- p + theme(legend.position="none") + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var["GLO",y_bar,])
  p <- p + theme(legend.position="none") + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,]["GLO",,,invert=TRUE]) + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var["GLO",,,invert=TRUE],scales="free_y") + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE per capita by sector (time domain, line graph)----

  swlatex(sw,"\\onecolumn")
  swlatex(sw,"\\subsection{FE per capita (by sector, time domain, line graph)}")

  items<- c(
    "FE (EJ/yr)",
    "FE|Transport (EJ/yr)",
    "FE|Buildings (EJ/yr)",
    "FE|Industry (EJ/yr)")

  p <- lineplots_perCap(data, items, 1e3, "FE per Cap. (GJ/yr)", global=T, histdata=hist)

  if("sr15data" %in% rownames(installed.packages()) & is.character(sr15marker_RCP)){

    variable <- Population <- `FE|Transport` <- `FE|Buildings` <- `FE|Industry` <- NULL
    region <- period <- `GDP|PPP` <- model <- value <- scenario <- FE <- NULL

    ## get marker scenario data
    marker_items <- c(
      "Final Energy",
      paste0("Final Energy|", c("Industry", "Residential and Commercial", "Transportation")),
      "Population")
    sr15dt <- as.data.table(sr15data::sr15data)[
      region == "World"][
    , region := "GLO"][
      data.table(period=y), on="period"]
    sr15scens <- data.table(scenario=paste0("SSP", 1:5, "-", sr15marker_RCP),
                            model=c("IMAGE 3.0.1",
                                    "MESSAGE-GLOBIOM 1.0",
                                    "AIM/CGE 2.0",
                                    "GCAM 4.2",
                                    "REMIND-MAgPIE 1.5"))
    markers <- sr15dt[sr15scens, on=c("model", "scenario")][
      variable %in% marker_items][
    , variable := gsub("Final Energy", "FE", variable)][
    , variable := gsub("Transportation", "Transport", variable)][
    , variable := gsub("Residential and Commercial", "Buildings", variable)][
    , scenario := gsub("Baseline", "Base", scenario)][, unit := NULL]

    markers_wide <- data.table::dcast(markers, ... ~ variable)

    markers_wide[, `:=`("FE|Transport" = `FE|Transport`/Population*1e3,
                        "FE|Buildings" = `FE|Buildings`/Population*1e3,
                        "FE|Industry" = `FE|Industry`/Population*1e3,
                        "FE" = `FE`/Population*1e3), by=c("model")]

    markers <- data.table::melt(markers_wide, id.vars=c("model", "scenario", "region", "period"))[
                             variable != "Population"]

    p <- p + geom_line(data=markers, aes(x=period, y=value, group=scenario, color=scenario),
                       size=2, alpha=1/4)

  }

  swfigure(sw,print,p,sw_option="height=9,width=16")

  ## Second page, with color coded regions

  p <- lineplots_perCap(data, items, 1e3, "FE per Cap. (GJ/yr)", global = F, histdata = hist)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\twocolumn")


  ## ---- FE per capita by sector (GDP domain)----

  swlatex(sw,"\\onecolumn")
  swlatex(sw,"\\subsection{FE per capita (by sector, GDP)}")

  items<- c("FE|Transport (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)")

  p <- lineplots_perCap(data, items, 1e3, "FE per Cap. (GJ/yr)", global = T,
                        per_gdp = T, histdata = hist)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  p <- lineplots_perCap(data, items, 1e3, "FE per Cap. (GJ/yr)", per_gdp = T,
                        histdata = hist)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\twocolumn")


  ## ---- FE by carrier ----

  swlatex(sw,"\\subsection{FE by carrier}")

  items<- c("FE|Solids (EJ/yr)",
            "FE|Liquids (EJ/yr)",
            "FE|Gases (EJ/yr)",
            "FE|Heat (EJ/yr)",
            "FE|Hydrogen (EJ/yr)",
            "FE|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Industry by carrier ----

  swlatex(sw,"\\subsection{FE Industry by carrier}")

  items<- c("FE|Industry|Solids (EJ/yr)",
            "FE|Industry|Liquids (EJ/yr)",
            "FE|Industry|Gases (EJ/yr)",
            "FE|Industry|Heat (EJ/yr)",
            "FE|Industry|Hydrogen (EJ/yr)",
            "FE|Industry|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Buildings by carrier ----

  swlatex(sw,"\\subsection{FE Buildings by carrier}")

  items<- c("FE|Buildings|Solids (EJ/yr)",
            "FE|Buildings|Liquids (EJ/yr)",
            "FE|Buildings|Gases (EJ/yr)",
            "FE|Buildings|Heat (EJ/yr)",
            "FE|Buildings|Hydrogen (EJ/yr)",
            "FE|Buildings|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Transport by carrier ----

  swlatex(sw,"\\subsection{FE Transport by carrier}")
  items<- c (
    "FE|Transport|Electricity (EJ/yr)",
    "FE|Transport|Hydrogen (EJ/yr)",
    "FE|Transport|Liquids (EJ/yr)"
  )

  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE CDR by carrier ----

  swlatex(sw,"\\subsection{FE CDR by carrier}")

  items<- c("FE|CDR|Liquids (EJ/yr)",
            "FE|CDR|Gases (EJ/yr)",
            "FE|CDR|Hydrogen (EJ/yr)",
            "FE|CDR|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")


  ## ---- SE Electricity by carrier ----

  swlatex(sw,"\\subsection{SE Electricity by carrier}")

  items<- c ("SE|Electricity|Coal|w/ CCS (EJ/yr)",
             "SE|Electricity|Coal|w/o CCS (EJ/yr)",
             "SE|Electricity|Oil (EJ/yr)",
             "SE|Electricity|Gas|w/ CCS (EJ/yr)",
             "SE|Electricity|Gas|w/o CCS (EJ/yr)",
             "SE|Electricity|Geothermal (EJ/yr)",
             "SE|Electricity|Hydro (EJ/yr)",
             "SE|Electricity|Nuclear (EJ/yr)",
             "SE|Electricity|Biomass|w/ CCS (EJ/yr)",
             "SE|Electricity|Biomass|w/o CCS (EJ/yr)",
             "SE|Electricity|Solar|CSP (EJ/yr)",
             "SE|Electricity|Solar|PV (EJ/yr)",
             "SE|Electricity|Wind (EJ/yr)",
             "SE|Electricity|Hydrogen (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE non-electric by carrier ----

  swlatex(sw,"\\subsection{SE non-electric by carrier}")
  ## XXX

  ## ---- PE by sector ----

  swlatex(sw,"\\subsection{PE by sector}")
  ## XXX

  ## ---- PE by carrier ----

  swlatex(sw,"\\subsection{PE by carrier}")

  items <-c("PE|Coal (EJ/yr)",
            "PE|Oil (EJ/yr)",
            "PE|Gas (EJ/yr)",
            "PE|Biomass (EJ/yr)",
            "PE|Nuclear (EJ/yr)",
            "PE|Solar (EJ/yr)",
            "PE|Wind (EJ/yr)",
            "PE|Hydro (EJ/yr)",
            "PE|Geothermal (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- CO2 Prices ----

  swlatex(sw,"\\subsection{CO2 Prices}")

  p <- mipLineHistorical(data[mainReg,,"Price|Carbon (US$2005/t CO2)"],x_hist=NULL,
                         ylab='Price|Carbon [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  p <- mipLineHistorical(data[mainReg,,"Price|Carbon (US$2005/t CO2)"],x_hist=NULL,
                         ylab='Price|Carbon_log [US$2005/t CO2]',ybreaks=c(20,30,40,50,60,75,100,200,500,1000,2000,3000),
                         ylim=c(20,3000),ylog=TRUE)
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  p <- mipLineHistorical(data[,,"Price|Carbon (US$2005/t CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Carbon [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")

  p <- mipLineHistorical(
    data[,,"Price|Carbon (US$2005/t CO2)"][
      mainReg,,,invert=TRUE],
    ylab='Price|Carbon [US$2005/t CO2]',
    scales="free_y",
    plot.priority=c("x_hist","x","x_proj"),
    color.dim="region",
    facet.dim="scenario",
    facet.ncol=2) +
    theme(legend.position="right")

  swfigure(sw,print,p,sw_option="height=9,width=16")
  swlatex(sw,"\\twocolumn")

  if("Policy Cost|Consumption Loss (billion US$2005/yr)" %in% magclass::getNames(data,dim=3)) {
    ## ---- Policy Cost|Consumption Loss ----
    swlatex(sw,"\\subsection{Policy Costs}")

    p <- mipLineHistorical(
      data[mainReg,,"Policy Cost|Consumption Loss (billion US$2005/yr)"],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss [billion US$2005/yr]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")

    p <- mipLineHistorical(
      data[,,"Policy Cost|Consumption Loss (billion US$2005/yr)"][
        mainReg,,,invert=TRUE],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss [billion US$2005/yr]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"),
      facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(
      data[mainReg,,"Policy Cost|Consumption Loss|Relative to Reference Consumption (percent)"],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss|Relative to Reference Consumption [%]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(
      data[,,"Policy Cost|Consumption Loss|Relative to Reference Consumption (percent)"][
        mainReg,,,invert=TRUE],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss|Relative to Reference Consumption [%]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"),
      facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
}

  ## ---- Mitigation Indicators of demand-side transformation in 2050 ----
  ##varis <- c("FE|Industry (EJ/yr)",
  ##           "FE|Buildings (EJ/yr)",
  ##           "FE|Transport (EJ/yr)",
  ##           "FE (EJ/yr)",
  ##           "FE|Industry|Fuels|Share (Percent)",
  ##           "FE|Buildings|Fuels|Share (Percent)",
  ##           "FE|Transport|Fuels|Share (Percent)",
  ##           "FE|Fuels|Share (Percent)",
  ##           "FE|Industry|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "FE|Buildings|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "FE|Transport|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "FE|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "Emi|CO2|Energy|Demand|Industry|Gross (Mt CO2/yr)",
  ##           "Emi|CO2|Buildings|Direct and Indirect (Mt CO2/yr)",
  ##           "Emi|CO2|Transport|Demand (Mt CO2/yr)",
  ##           "Emi|CO2|Fossil Fuels and Industry|Demand (Mt CO2/yr)")
  ##
  ##if (all(varis%in%getNames(data,dim=3))) {  # only plot if the variables exist in the data
  ##  swlatex(sw,"\\subsection{Mitigation Indicators of demand-side transformation in 2050}")
  ##
  ##  dataq50 <- as.quitte(data[mainReg,2050,intersect(varis,getNames(data,dim=3))]) # filter data for 2050 and GLO
  ##
  ##  # trick to make rows of the 4x4 grid have constant scaling in the sectoral plots (first 3 of each row)
  ##  varis <- unlist(strsplit(varis,split = " \\("))[seq(1,2*length(varis),2)] # remove units from object varis for matching with column "variable" in quitte object
  ##  dataq50$maxval <- dataq50$value
  ##  dataq50[which(dataq50$variable%in%varis[1:3]),"maxval"] <- max(dataq50[which(dataq50$variable%in%varis[1:3]),"value"])
  ##  dataq50[which(dataq50$variable%in%varis[5:8]),"maxval"] <- 100
  ##  dataq50[which(dataq50$variable%in%varis[9:11]),"maxval"] <- max(dataq50[which(dataq50$variable%in%varis[9:11]),"value"])
  ##  dataq50[which(dataq50$variable%in%varis[13:15]),"maxval"] <- max(dataq50[which(dataq50$variable%in%varis[13:15]),"value"])
  ##
  ##  # add units to variable names
  ##  levels(dataq50$variable)[1:4] <- paste0(levels(dataq50$variable)[1:4],"\n",levels(dataq50$unit)[[1]])
  ##  levels(dataq50$variable)[5:8] <- paste0(levels(dataq50$variable)[5:8],"\n",levels(dataq50$unit)[[2]])
  ##  levels(dataq50$variable)[9:12] <- paste0(levels(dataq50$variable)[9:12],"\n",levels(dataq50$unit)[[3]])
  ##  levels(dataq50$variable)[13:16] <- paste0(levels(dataq50$variable)[13:16],"\n",levels(dataq50$unit)[[4]])
  ##
  ##  # generate plot
  ##  p <- ggplot(dataq50, aes_(x = ~scenario, y = ~value))
  ##  p <- p + geom_col(aes_(fill=~scenario),width =0.5) +
  ##     facet_wrap("variable",scales = "free_y") +
  ##     geom_point(aes_(y=~maxval),alpha=0) # add invisible points to make first 3 items in each row have constant scaling
  ##
  ##  swfigure(sw,print,p,sw_option="height=16,width=16")
  ##}

#### Macro ####
  ## ---- ++++ M A C R O ++++ ----

  swlatex(sw,"\\section{Macro}")

  swlatex(sw,"\\subsection{Consumption}")
  p <- mipLineHistorical(data[mainReg,,"Consumption (billion US$2005/yr)"],x_hist=NULL,
                         ylab='Consumption [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Consumption (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Consumption [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Population}")
  p <- mipLineHistorical(data[mainReg,,"Population (million)"],x_hist=hist[mainReg,,"Population (million)"],
                         ylab='Population [million]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Population (million)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Population (million)"][mainReg,,,invert=TRUE],
                         ylab='Population [million]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{GDP - MER}")
  p <- mipLineHistorical(data[mainReg,,"GDP|MER (billion US$2005/yr)"],x_hist=NULL,
                         ylab='GDP|MER [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"GDP|MER (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='GDP|MER [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{GDP - PPP}")
  p <- mipLineHistorical(data[mainReg,,"GDP|PPP (billion US$2005/yr)"],x_hist=hist[mainReg,,"GDP|PPP (billion US$2005/yr)"],
                         ylab='GDP|PPP [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"GDP|PPP (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"GDP|PPP (billion US$2005/yr)"][mainReg,,,invert=TRUE],
                         ylab='GDP|PPP [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{GDP - PPP per Capita}")
  gdpcap <- data[,,"GDP|PPP (billion US$2005/yr)"]/data[,,"Population (million)"]*1e3
  gdpcap_hist <- collapseNames(hist[,,"GDP|PPP (billion US$2005/yr)"]/hist[,,"Population (million)"]*1e3, collapsedim=4)

  p <- mipLineHistorical(gdpcap[mainReg,,], x_hist=gdpcap_hist[mainReg,,],
                         ylab='GDP|PPP per Cap. [US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(gdpcap[mainReg,,,invert=TRUE],x_hist=gdpcap_hist[mainReg,,,invert=TRUE],
                         ylab='GDP|PPP per Cap. [US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Capital Stock}")
  p <- mipLineHistorical(data[mainReg,,"Capital Stock|Non-ESM (billion US$2005)"],x_hist=NULL,
                         ylab='Macro-economic Capital Stock [billion US$2005]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Capital Stock|Non-ESM (billion US$2005)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Macro-economic Capital Stock [billion US$2005]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Investments}")
  p <- mipLineHistorical(data[mainReg,,"Investments|Non-ESM (billion US$2005/yr)"],x_hist=NULL,
                         ylab='Macro-economic Investments [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Investments|Non-ESM (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Macro-economic Investments [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Interest Rate}")
  p <- mipLineHistorical(data[mainReg,,"Interest Rate (t+1)/(t-1)|Real ()"],x_hist=NULL,
                         ylab='Interest Rate',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Interest Rate (t+1)/(t-1)|Real ()"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Interest Rate',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Prices}")

  ## ---- Prices PE ----

  swlatex(sw,"\\subsubsection{PE Prices}")

  p <- mipLineHistorical(data[mainReg,,"Price|Natural Gas|Primary Level (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Natural Gas|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Natural Gas|Primary Level (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Natural Gas|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Crude Oil|Primary Level (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Crude Oil|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Crude Oil|Primary Level (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Crude Oil|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Coal|Primary Level (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Coal|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Coal|Primary Level (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Coal|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Biomass|Primary Level (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Biomass|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Biomass|Primary Level (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Biomass|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Prices SE ----

  swlatex(sw,"\\subsubsection{SE Prices}")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Electricity (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Electricity (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Liquids (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Liquids (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Gases (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Gases (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Solids (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Solids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Solids (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Solids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Hydrogen (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Hydrogen (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Heat (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Heat (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Prices FE

  swlatex(sw,"\\subsubsection{FE Prices}")

  ## ---- Prices FE Liquids ----

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Liquids|Transport (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Liquids|Transport [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Liquids|Transport (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Liquids|Transport [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  if("Price|Final Energy|Heating Oil|Buildings (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Heating Oil|Buildings (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Heating Oil|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Heating Oil|Buildings (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Heating Oil|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  if("Price|Final Energy|Heating Oil|Industry (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Heating Oil|Industry (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Heating Oil|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Heating Oil|Industry (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Heating Oil|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  ## ---- Prices FE Gas ----

  if("Price|Final Energy|Gases|Buildings (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Gases|Buildings (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Gases|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Gases|Buildings (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Gases|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  if("Price|Final Energy|Gases|Industry (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Gases|Industry (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Gases|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Gases|Industry (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Gases|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  ## ---- Prices FE Solids ----

  if("Price|Final Energy|Solids|Buildings (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Solids|Buildings (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Solids|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Solids|Buildings (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Solids|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  if("Price|Final Energy|Solids|Industry (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Solids|Industry (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Solids|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Solids|Industry (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Solids|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  ## ---- Prices FE Electricity ----

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Electricity|Transport (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Electricity|Transport [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Electricity|Transport (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Electricity|Transport [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  if("Price|Final Energy|Electricity|Buildings (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Electricity|Buildings (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Electricity|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Electricity|Buildings (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Electricity|Buildings [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  if("Price|Final Energy|Electricity|Industry (US$2005/GJ)" %in% magclass::getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Electricity|Industry (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Electricity|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Electricity|Industry (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Electricity|Industry [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  ## ---- Trade ----

  swlatex(sw,"\\subsection{Trade}")

  p <- mipLineHistorical(data[mainReg,,"Trade|Coal (EJ/yr)"],x_hist=hist[mainReg,,"Trade|Coal (EJ/yr)"],
                         ylab='Trade|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Trade|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='Trade|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Trade|Oil (EJ/yr)"],x_hist=hist[mainReg,,"Trade|Oil (EJ/yr)"],
                         ylab='Trade|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Oil (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Trade|Oil (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='Trade|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Trade|Gas (EJ/yr)"],x_hist=hist[mainReg,,"Trade|Gas (EJ/yr)"],
                         ylab='Trade|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Trade|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='Trade|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Trade|Biomass (EJ/yr)"],x_hist=NULL,
                         ylab='Trade|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Trade|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Trade|Goods (billion US$2005/yr)"],x_hist=NULL,
                         ylab='Trade|Goods [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Goods (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Trade|Goods [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- FE intensity of GDP ----

  swlatex(sw,"\\subsection{FE intensity of GDP}")

  items<- c("FE|Transport (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)")
  gdp <- mselect(data, variable="GDP|PPP (billion US$2005/yr)")
  var <- data[,,intersect(items, getNames(data,dim=3))]/gdp*1e3 # EJ/bil.$ -> GJ/$ -> 1e3 MJ/$

  p <- mipArea(var["GLO",,], scales="free_y")
  p <- p + theme(legend.position="none") + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var["GLO",y_bar,])
  p <- p + theme(legend.position="none") + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,]["GLO",,,invert=TRUE]) + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var["GLO",,,invert=TRUE],scales="free_y") + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- Kaya decomposition ----

  swlatex(sw,"\\subsection{Kaya-Decomposition}")
  ## calculate Kaya-Decomposition
  kaya <- new.magpie(getRegions(data),getYears(data),magclass::getNames(data,dim=1))
  kaya <- add_dimension(kaya,dim=3.2,add="kaya",nm=c("CO2 FF&I/FE","FE/GDP","GDP/Population","Population"))
  for (i in magclass::getNames(data,dim=1)) {
    kaya[,,i] <- calcKayaDecomp(mif=data[,,i])
  }
  p <- magpie2ggplot2(kaya,facet_y="Data2",facet_x="Data1",color="Region",group=NULL,
                      scales="free_y",show_grid=TRUE,ylab='Kaya Decomposition [%]',
                      color_pal=plotstyle(getRegions(kaya)))
  swfigure(sw,print,p,sw_option="height=10,width=9")

  kaya <- new.magpie(getRegions(data),getYears(data),magclass::getNames(data,dim=1))
  kaya <- add_dimension(kaya,dim=3.2,add="kaya",nm=c("CO2 FF&I [Mt CO2/yr]/FE [EJ/yr]","FE [EJ/yr]/GDP [billion US$2005/yr]",
                                                     "GDP [billion US$2005/yr]/Population [million]","Population [million]"))
  for (i in magclass::getNames(data,dim=1)) {
    kaya[,,i] <- calcKayaDecomp(mif=data[,,i],ref_year=NULL)
  }
  p <- magpie2ggplot2(kaya,facet_y="Data2",facet_x="Data1",color="Region",group=NULL,
                      scales="free_y",show_grid=TRUE,ylab='Kaya Decomposition',
                      color_pal=plotstyle(getRegions(kaya)))
  swfigure(sw,print,p,sw_option="height=10,width=9")

  ## ---- ++++ E M I S S I O N S ++++ ----

  swlatex(sw,"\\section{Emissions}")

  ## ---- Emissions GHG Total ----

  swlatex(sw,"\\subsection{GHGtot}")

  p <- mipLineHistorical(data[mainReg,,"Emi|GHGtot (Mt CO2-equiv/yr)"],x_hist=hist[mainReg,,"Emi|GHGtot (Mt CO2-equiv/yr)"],
                         ylab='Emi|GHGtot [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|GHGtot (Mt CO2-equiv/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|GHGtot (Mt CO2-equiv/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|GHGtot [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Emissions CO2 ----

  swlatex(sw,"\\subsection{CO2}")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2 (Mt CO2/yr)"],x_hist=hist[mainReg,,"Emi|CO2 (Mt CO2/yr)"],
                         ylab='Emi|CO2 [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2 (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CO2 (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2 [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy (Mt CO2/yr)"],x_hist=hist[mainReg,,"Emi|CO2|Energy (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy  [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CO2|Energy (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy  [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy|Supply|Electricity (Mt CO2/yr)"],x_hist=hist[mainReg,,"Emi|CO2|Energy|Supply|Electricity (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy|Supply|Electricity [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy|Supply|Electricity (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CO2|Energy|Supply|Electricity (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy|Supply|Electricity [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)"],x_hist=hist[mainReg,,"Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"],
                         ylab='Emi|CO2|Gross Fossil Fuels and Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Gross Fossil Fuels and Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"],x_hist=hist[mainReg,,"Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"],
                         ylab='Emi|CO2|Fossil Fuels and Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Fossil Fuels and Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Land-Use Change (Mt CO2/yr)"],x_hist=hist[mainReg,,"Emi|CO2|Land Use (Mt CO2/yr)"],
                         ylab='Emi|CO2|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Land-Use Change (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CO2|Land Use (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Carbon Capture and Storage|Fossil|Pe2Se (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|Carbon Capture and Storage|Fossil|Pe2Se [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Carbon Capture and Storage|Fossil|Pe2Se (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|Carbon Capture and Storage|Fossil|Pe2Se [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Carbon Capture and Storage (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|Carbon Capture and Storage [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Carbon Capture and Storage (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|Carbon Capture and Storage [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Emissions CDR ----

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|Land-Use Change (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|Land-Use Change (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|BECCS (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|BECCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|BECCS (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|BECCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  if("Emi|CO2|CDR|DAC (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|DAC (Mt CO2/yr)"],x_hist=NULL,
                           ylab='Emi|CO2|CDR|DAC [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Emi|CO2|CDR|DAC (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|CDR|DAC [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  if("Emi|CO2|CDR|EW (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|EW (Mt CO2/yr)"],x_hist=NULL,
                           ylab='Emi|CO2|CDR|EW [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Emi|CO2|CDR|EW (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|CDR|EW [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Emissions CDR per sector ----

  tot <-"Emi|CO2 (Mt CO2/yr)"
  items <- c("Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)",
             "Emi|CO2|Carbon Capture and Storage|Fossil (Mt CO2/yr)",
             "Emi|CO2|Land-Use Change (Mt CO2/yr)",
             "Emi|CO2|CDR|BECCS (Mt CO2/yr)",
             "Emi|CO2|CDR|DAC (Mt CO2/yr)",
             "Emi|CO2|CDR|EW (Mt CO2/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- Emissions CO2 cumulated ----

  swlatex(sw,"\\subsubsection{Cumulated}")

  toplot <- data[,,"Emi|CO2|Cumulated (Mt CO2/yr)"]

  toplot2010 <- setYears(toplot[,2010,], NULL)
  toplot <- toplot - toplot2010

  p <- mipLineHistorical(toplot[mainReg,,"Emi|CO2|Cumulated (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(toplot[mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|Land-Use Change|Cumulated (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Land-Use Change|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|Land-Use Change|Cumulated (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Land-Use Change|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|BECCS|Cumulated (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|BECCS|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|BECCS|Cumulated (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|BECCS|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  if("Emi|CO2|CDR|DAC (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|DAC|Cumulated (Mt CO2/yr)"],x_hist=NULL,
                           ylab='Emi|CO2|CDR|DAC|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Emi|CO2|CDR|DAC|Cumulated (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|CDR|DAC|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  if("Emi|CO2|CDR|EW (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|EW|Cumulated (Mt CO2/yr)"],x_hist=NULL,
                           ylab='Emi|CO2|CDR|EW|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Emi|CO2|CDR|EW|Cumulated (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|CDR|EW|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|Cumulated (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|Cumulated (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


  ## ---- Emissions CDR by sector cumulated

  tot <-"Emi|CO2|Cumulated (Mt CO2/yr)"
  items <- c("Emi|CO2|Gross Fossil Fuels and Industry|Cumulated (Mt CO2/yr)",
             "Emi|CO2|Carbon Capture and Storage|Fossil|Cumulated (Mt CO2/yr)",
             "Emi|CO2|Land-Use Change|Cumulated (Mt CO2/yr)",
             "Emi|CO2|CDR|BECCS|Cumulated (Mt CO2/yr)",
             "Emi|CO2|CDR|DAC|Cumulated (Mt CO2/yr)",
             "Emi|CO2|CDR|EW|Cumulated (Mt CO2/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- Emissions CH4 ----

  swlatex(sw,"\\subsection{CH4}")

  p <- mipLineHistorical(data[mainReg,,"Emi|CH4 (Mt CH4/yr)"],x_hist=hist[mainReg,,"Emi|CH4 (Mt CH4/yr)"],
                         ylab='Emi|CH4 [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CH4 (Mt CH4/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CH4 (Mt CH4/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CH4 [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CH4|Land Use (Mt CH4/yr)"],x_hist=hist[mainReg,,"Emi|CH4|Land Use (Mt CH4/yr)"],
                         ylab='Emi|CH4|Land Use [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CH4|Land Use (Mt CH4/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CH4|Land Use (Mt CH4/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CH4|Land Use [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CH4|Energy Supply and Demand (Mt CH4/yr)"],x_hist=hist[mainReg,,"Emi|CH4|Energy Supply and Demand (Mt CH4/yr)"],
                         ylab='Emi|CH4|Energy Supply and Demand [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CH4|Energy Supply and Demand (Mt CH4/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|CH4|Energy Supply and Demand (Mt CH4/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CH4|Energy Supply and Demand [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Emissions N2O ----

  swlatex(sw,"\\subsection{N2O}")

  p <- mipLineHistorical(data[mainReg,,"Emi|N2O (kt N2O/yr)"],x_hist=hist[mainReg,,"Emi|N2O (kt N2O/yr)"],
                         ylab='Emi|N2O [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|N2O (kt N2O/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|N2O (kt N2O/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|N2O [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|N2O|Land Use (kt N2O/yr)"],x_hist=hist[mainReg,,"Emi|N2O|Land Use (kt N2O/yr)"],
                         ylab='Emi|N2O|Land Use [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|N2O|Land Use (kt N2O/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|N2O|Land Use (kt N2O/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|N2O|Land Use [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|N2O|Energy Supply and Demand (kt N2O/yr)"],x_hist=hist[mainReg,,"Emi|N2O|Energy Supply and Demand (kt N2O/yr)"],
                         ylab='Emi|N2O|Energy Supply and Demand [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|N2O|Energy Supply and Demand (kt N2O/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"Emi|N2O|Energy Supply and Demand (kt N2O/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|N2O|Energy Supply and Demand [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- ++++ E N E R G Y ++++ ----

  swlatex(sw,"\\section{Energy}")

  ## ---- Investments Electricity ----

  swlatex(sw,"\\subsection{Investments Electricity}")

  items <- c ("Energy Investments|Elec|Coal|w/ CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Coal|w/o CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Gas|w/ CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Gas|w/o CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Oil (billion US$2005/yr)",
              "Energy Investments|Elec|Biomass|w/ CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Biomass|w/o CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Nuclear (billion US$2005/yr)",
              "Energy Investments|Elec|Solar (billion US$2005/yr)",
              "Energy Investments|Elec|Wind (billion US$2005/yr)",
              "Energy Investments|Elec|Hydro (billion US$2005/yr)",
              "Energy Investments|Elec|Geothermal (billion US$2005/yr)",
              "Energy Investments|Elec|Hydrogen (billion US$2005/yr)",
              "Energy Investments|Elec|Grid (billion US$2005/yr)",
              "Energy Investments|Elec|Storage (billion US$2005/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- Capacities Electricity ----

  swlatex(sw,"\\subsection{Capacities Electricity}")

  items <- c ("Cap|Electricity|Coal|w/ CCS (GW)",
              "Cap|Electricity|Coal|w/o CCS (GW)",
              "Cap|Electricity|Gas|w/ CCS (GW)",
              "Cap|Electricity|Gas|w/o CCS (GW)",
              "Cap|Electricity|Oil|w/o CCS (GW)",
              "Cap|Electricity|Biomass (GW)",
              "Cap|Electricity|Nuclear (GW)",
              "Cap|Electricity|Hydro (GW)",
              "Cap|Electricity|Geothermal (GW)",
              "Cap|Electricity|Hydrogen (GW)",
              "Cap|Electricity|Storage|Battery (GW)",
              "Cap|Electricity|Solar (GW)",
              "Cap|Electricity|Wind (GW)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- PE Mix ----

  ## ---- PE Mix Coal ----

  swlatex(sw,"\\subsubsection{PE|Coal}")

  items<- c ("PE|Coal|Gases (EJ/yr)",
             "PE|Coal|Liquids (EJ/yr)",
             "PE|Coal|Solids (EJ/yr)",
             "PE|Coal|Heat (EJ/yr)",
             "PE|Coal|Electricity (EJ/yr)",
             "PE|Coal|Hydrogen (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- PE Mix Gas ----

  swlatex(sw,"\\subsubsection{PE|Gas}")

  items<- c ("PE|Gas|Gases (EJ/yr)",
             "PE|Gas|Heat (EJ/yr)",
             "PE|Gas|Electricity|w/ CCS (EJ/yr)",
             "PE|Gas|Electricity|w/o CCS (EJ/yr)",
             "PE|Gas|Liquids|w/ CCS (EJ/yr)",
             "PE|Gas|Liquids|w/o CCS (EJ/yr)",
             "PE|Gas|Hydrogen|w/ CCS (EJ/yr)",
             "PE|Gas|Hydrogen|w/o CCS (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- PE Mix Biomass ----

  swlatex(sw,"\\subsubsection{PE|Biomass}")

  items<- c ("PE|Biomass|Solids (EJ/yr)",
             "PE|Biomass|Heat (EJ/yr)",
             "PE|Biomass|Liquids|w/ CCS (EJ/yr)",
             "PE|Biomass|Liquids|w/o CCS (EJ/yr)",
             "PE|Biomass|Gases (EJ/yr)",
             "PE|Biomass|Electricity|w/ CCS (EJ/yr)",
             "PE|Biomass|Electricity|w/o CCS (EJ/yr)",
             "PE|Biomass|Hydrogen|w/ CCS (EJ/yr)",
             "PE|Biomass|Hydrogen|w/o CCS (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- PE Line ----

  swlatex(sw,"\\subsection{Primary Energy line plots}")

  swlatex(sw,"\\subsubsection{PE|Coal}")
  p <- mipLineHistorical(data[mainReg,,"PE|Coal (EJ/yr)"],x_hist=hist[mainReg,,"PE|Coal (EJ/yr)"],
                         ylab='PE|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"PE|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{PE|Oil}")
  p <- mipLineHistorical(data[mainReg,,"PE|Oil (EJ/yr)"],x_hist=hist[mainReg,,"PE|Oil (EJ/yr)"],
                         ylab='PE|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Oil (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"PE|Oil (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{PE|Gas}")
  p <- mipLineHistorical(data[mainReg,,"PE|Gas (EJ/yr)"],x_hist=hist[mainReg,,"PE|Gas (EJ/yr)"],
                         ylab='PE|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"PE|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{PE|Biomass}")
  p <- mipLineHistorical(data[mainReg,,"PE|Biomass (EJ/yr)"],x_hist=hist[mainReg,,"PE|Biomass (EJ/yr)"],
                         ylab='PE|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"PE|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{Primary Energy Production|Biomass|Energy Crops}")
  p <- mipLineHistorical(data[mainReg,,"Primary Energy Production|Biomass|Energy Crops (EJ/yr)"],x_hist=NULL,
                         ylab='Primary Energy Production|Biomass|Energy Crops [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Primary Energy Production|Biomass|Energy Crops (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Primary Energy Production|Biomass|Energy Crops [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- SE Mix ----

  swlatex(sw,"\\subsection{Secondary Energy Mixes}")

  ## ---- SE Mix Liquids ----

  swlatex(sw,"\\subsubsection{SE|Liquids}")

  items<- c ("SE|Liquids|Oil (EJ/yr)",
             "SE|Liquids|Biomass|w/ CCS (EJ/yr)",
             "SE|Liquids|Biomass|w/o CCS (EJ/yr)",
             "SE|Liquids|Coal|w/ CCS (EJ/yr)",
             "SE|Liquids|Coal|w/o CCS (EJ/yr)",
             "SE|Liquids|Gas|w/ CCS (EJ/yr)",
             "SE|Liquids|Gas|w/o CCS (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE Mix Gases ----

  swlatex(sw,"\\subsubsection{SE|Gases}")

  items<- c ("SE|Gases|Natural Gas (EJ/yr)",
             "SE|Gases|Biomass|w/ CCS (EJ/yr)",
             "SE|Gases|Biomass|w/o CCS (EJ/yr)",
             "SE|Gases|Coal|w/ CCS (EJ/yr)",
             "SE|Gases|Coal|w/o CCS (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE Mix Hydrogen ----

  swlatex(sw,"\\subsubsection{SE|Hydrogen}")

  items<- c ("SE|Hydrogen|Biomass|w/ CCS (EJ/yr)",
             "SE|Hydrogen|Biomass|w/o CCS (EJ/yr)",
             "SE|Hydrogen|Coal|w/ CCS (EJ/yr)",
             "SE|Hydrogen|Coal|w/o CCS (EJ/yr)",
             "SE|Hydrogen|Gas|w/ CCS (EJ/yr)",
             "SE|Hydrogen|Gas|w/o CCS (EJ/yr)",
             "SE|Hydrogen|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE Mix Solids ----

  swlatex(sw,"\\subsubsection{SE|Solids}")

  items<- c ("SE|Solids|Biomass (EJ/yr)",
             "SE|Solids|Traditional Biomass (EJ/yr)",
             "SE|Solids|Coal (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE Line ----

  swlatex(sw,"\\subsection{Secondary Energy line plots}")

  ## ---- SE Line Gases ----

  swlatex(sw,"\\subsubsection{SE|Gases}")

  p <- mipLineHistorical(data[mainReg,,"SE|Gases|Natural Gas (EJ/yr)"],x_hist=hist[mainReg,,"SE|Gases|Gas (EJ/yr)"],
                         ylab='SE|Gases|(Natural)Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Gases|Natural Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Gases|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Gases|(Natural)Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Gases|Coal (EJ/yr)"],x_hist=hist[mainReg,,"SE|Gases|Coal (EJ/yr)"],
                         ylab='SE|Gases|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Gases|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Gases|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Gases|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Gases|Biomass (EJ/yr)"],x_hist=hist[mainReg,,"SE|Gases|Biomass (EJ/yr)"],
                         ylab='SE|Gases|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Gases|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Gases|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Gases|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- SE Line Electricity ----

  swlatex(sw,"\\subsubsection{SE|Electricity}")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Gas (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Gas (EJ/yr)"],
                         ylab='SE|Electricity|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Coal (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Coal (EJ/yr)"],
                         ylab='SE|Electricity|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Oil (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Oil (EJ/yr)"],
                         ylab='SE|Electricity|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Oil (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Oil (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Hydro (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Hydro (EJ/yr)"],
                         ylab='SE|Electricity|Hydro [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Hydro (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Hydro (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Hydro [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Wind (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Wind (EJ/yr)"],
                         ylab='SE|Electricity|Wind [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Wind (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Wind (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Wind [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Nuclear (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Nuclear (EJ/yr)"],
                         ylab='SE|Electricity|Nuclear [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Nuclear (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Nuclear (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Nuclear [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Solar (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Solar (EJ/yr)"],
                         ylab='SE|Electricity|Solar [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Solar (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Solar (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Solar [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Biomass (EJ/yr)"],x_hist=hist[mainReg,,"SE|Electricity|Biomass (EJ/yr)"],
                         ylab='SE|Electricity|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Electricity|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- SE Line Solids ----

  swlatex(sw,"\\subsubsection{SE|Solids}")

  p <- mipLineHistorical(data[mainReg,,"SE|Solids|Coal (EJ/yr)"],x_hist=hist[mainReg,,"SE|Solids|Coal (EJ/yr)"],
                         ylab='SE|Solids|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Solids|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Solids|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Solids|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Solids|Biomass (EJ/yr)"],x_hist=hist[mainReg,,"SE|Solids|Biomass (EJ/yr)"],
                         ylab='SE|Solids|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Solids|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Solids|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Solids|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Solids|Traditional Biomass (EJ/yr)"],x_hist=hist[mainReg,,"SE|Solids|Traditional Biomass (EJ/yr)"],
                         ylab='SE|Solids|Traditional Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Solids|Traditional Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"SE|Solids|Traditional Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Solids|Traditional Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ----  FE Line ----

  swlatex(sw,"\\subsection{Final Energy line plot}")

  ## ----  FE Line Total ----

  swlatex(sw,"\\subsubsection{Total}")

  p <- mipLineHistorical(data[mainReg,,"FE (EJ/yr)"],x_hist=hist[mainReg,,"FE (EJ/yr)"],
                         ylab='FE [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- FE Line by Carrier ----

  swlatex(sw,"\\subsubsection{FE|Electricity}")

  p <- mipLineHistorical(data[mainReg,,"FE|Electricity (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Electricity (EJ/yr)"],
                         ylab='FE|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Gases}")

  p <- mipLineHistorical(data[mainReg,,"FE|Gases (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Gases (EJ/yr)"],
                         ylab='FE|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Heat}")

  p <- mipLineHistorical(data[mainReg,,"FE|Heat (EJ/yr)"],x_hist=NULL,
                         ylab='FE|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Heat (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='FE|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Solids}")

  p <- mipLineHistorical(data[mainReg,,"FE|Solids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Solids (EJ/yr)"],
                         ylab='FE|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Solids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Solids (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Liquids}")

  p <- mipLineHistorical(data[mainReg,,"FE|Liquids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Liquids (EJ/yr)"],
                         ylab='FE|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Hydrogen}")

  p <- mipLineHistorical(data[mainReg,,"FE|Hydrogen (EJ/yr)"],x_hist=NULL,
                         ylab='FE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='FE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- FE Line Buildings ----

  if("FE|Buildings (EJ/yr)" %in% magclass::getNames(data,dim=3)){
    swlatex(sw,"\\subsection{Buildings}")
    p <- mipLineHistorical(data[mainReg,,"FE|Buildings (EJ/yr)"],x_hist=hist[mainReg,,"FE|Buildings (EJ/yr)"],
                           ylab='FE|Buildings [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE|Buildings (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Electricity (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Electricity (EJ/yr)"],
                           ylab='FE|Buildings|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Gases (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Gases (EJ/yr)"],
                           ylab='FE|Buildings|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Heat (EJ/yr)"],x_hist=hist[mainReg,,"FE|Buildings|Heat (EJ/yr)"],
                           ylab='FE|Buildings|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Heat (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE|Buildings|Heat (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Solids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Solids (EJ/yr)"],
                           ylab='FE|Buildings|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Solids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Solids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Liquids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Liquids (EJ/yr)"],
                           ylab='FE|Buildings|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

  }

  ## ---- FE Line Industry ----

  if("FE|Industry (EJ/yr)" %in% magclass::getNames(data,dim=3)){
    swlatex(sw,"\\subsection{Industry}")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry (EJ/yr)"],x_hist=hist[mainReg,,"FE|Industry (EJ/yr)"],
                           ylab='FE|Industry [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE|Industry (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Electricity (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Electricity (EJ/yr)"],
                           ylab='FE|Industry|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Gases (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Gases (EJ/yr)"],
                           ylab='FE|Industry|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Heat (EJ/yr)"],x_hist=hist[mainReg,,"FE|Industry|Heat (EJ/yr)"],
                           ylab='FE|Industry|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Heat (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE|Industry|Heat (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Solids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Solids (EJ/yr)"],
                           ylab='FE|Industry|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Solids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Solids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Liquids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Liquids (EJ/yr)"],
                           ylab='FE|Industry|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

  }

  ## ---- FE Line Transport ----

  swlatex(sw,"\\subsection{Transport}")
  p <- mipLineHistorical(data[mainReg,,"FE|Transport (EJ/yr)"],x_hist=hist[mainReg,,"FE|Transport (EJ/yr)"],
                         ylab='FE|Transport [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"FE|Transport (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE|Transport (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|Liquids (EJ/yr)"],x_hist=hist[mainReg,,"FE|Transport|Liquids (EJ/yr)"],
                         ylab='FE|Transport|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE|Transport|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|Electricity (EJ/yr)"],x_hist=hist[mainReg,,"FE|Transport|Electricity (EJ/yr)"],
                         ylab='FE|Transport|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist[,,"FE|Transport|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  var <- "FE|Transport|Gases (EJ/yr)"

  if (var %in% getNames(data,dim=3)) {
    p <- mipLineHistorical(data[mainReg,,var],x_hist=hist[mainReg,,var],
                           ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")

    p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=hist[,,var][mainReg,,,invert=TRUE],
                           ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  ## ---- ++++ USEFUL ENERGY + ENERGY SERVICES ++++ ----

  swlatex(sw,"\\section{Useful Energy and Energy Services}")

  swlatex(sw,"\\subsection{Transport}")


  ## ---- UE transport per capita (time domain, line graph)----

  swlatex(sw,"\\onecolumn")
  swlatex(sw,"\\subsubsection{UE for Transport (per Capita, year)}")

  items<- c(
    "UE|Transport|LDV (EJ/yr)",
    "UE|Transport|Pass|non-LDV (EJ/yr)",
    "UE|Transport|Freight (EJ/yr)")

  p <- lineplots_perCap(data, items, 1e3, "UE per Cap. (GJ/yr)", global = T)

  swfigure(sw,print,p,sw_option="height=9,width=16")

  ## Second page, with color coded regions

  p <- lineplots_perCap(data, items, 1e3, "UE per Cap. (GJ/yr)", global = F)

  swfigure(sw,print,p,sw_option="height=9,width=16")


  ## ---- UE per capita for transport (GDP domain)----

  swlatex(sw,"\\subsubsection{UE for Transport (per Capita, GDP)}")

  items<- c(
    "UE|Transport|LDV (EJ/yr)",
    "UE|Transport|Pass|non-LDV (EJ/yr)",
    "UE|Transport|Freight (EJ/yr)")

  p <- lineplots_perCap(data, items, 1e3, "UE per Cap. (GJ/yr)", global = T, per_gdp = T)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  p <- lineplots_perCap(data, items, 1e3, "UE per Cap. (GJ/yr)", global = F, per_gdp = T)
  swfigure(sw,print,p,sw_option="height=9,width=16")


  ## ---- ES passenger transport per capita (time domain, line graph)----

  swlatex(sw,"\\subsubsection{Energy Services for Passenger Transport (per Capita, year)}")

  items<- c(
    "ES|Transport|Pass (bn pkm/yr)",
    "ES|Transport|Pass|LDV (bn pkm/yr)",
    "ES|Transport|Pass|non-LDV (bn pkm/yr)")

  p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                        global = T, per_gdp = F)

  swfigure(sw,print,p,sw_option="height=9,width=16")

  p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                        global = F, per_gdp = F)
  swfigure(sw,print,p,sw_option="height=9,width=16")


  ## ---- ES per capita for transport (GDP domain)----

  swlatex(sw,"\\subsubsection{Energy Services for Transport (per Capita, GDP)}")

  p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                        global = T, per_gdp = T)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                        global = F, per_gdp = T)
  swfigure(sw,print,p,sw_option="height=9,width=16")


  ## ---- ES freight transport per capita (time domain, line graph)----

  swlatex(sw,"\\subsubsection{Energy Services for Freight Transport (per Capita, year)}")

  items<- c(
    "ES|Transport|Freight (bn tkm/yr)")

  p <- lineplots_perCap(data, items, 1e3, "Freight Demand per Cap. (tkm/yr)",
                        global = T, per_gdp = F)

  swfigure(sw,print,p,sw_option="height=9,width=16")

  p <- lineplots_perCap(data, items, 1e3, "Freight Demand per Cap. (tkm/yr)",
                        global = F, per_gdp = F)

  swfigure(sw,print,p,sw_option="height=9,width=16")


  ## ---- ES per capita for transport (GDP domain)----

  swlatex(sw,"\\subsubsection{Energy Services for Freight Transport (per Capita, GDP)}")

  p <- lineplots_perCap(data, items, 1e3, "Freight Demand per Cap. (tkm/yr)",
                        global = T, per_gdp = T)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  p <- lineplots_perCap(data, items, 1e3, "Freight Demand per Cap. (tkm/yr)",
                        global = F, per_gdp = T)

  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\twocolumn")



  ## ---- ++++ C L I M A T E ++++ ----

  swlatex(sw,"\\section{Climate}")

  swfigure(sw,mipLineHistorical,data[,,"Forcing (W/m2)"],x_hist=NULL,
           ylab='Forcing [W/m2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

  swfigure(sw,mipLineHistorical,data[,,"Temperature|Global Mean (K)"],x_hist=NULL,
           ylab='Temperature|Global Mean [K]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

  ## Close output-pdf
  swclose(sw)
}
