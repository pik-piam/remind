#' Read in LCOE mif and write LCOE_plots.pdf
#' 
#' Read in all information from LCOE mif file and create
#' the LCOE_plots.pdf
#' 
#' @param mif a path to one or more mif-files (might be created by confGDX2MIF)
#' @param y time span for the data in line plots, default: y=c(2015, 2020,2030,2040,2050,2060) 
#' @param reg region(s) in focus, reg ="all_regi" shows all regions if the mifs contain different regions
#' @param fileName name of the pdf, default = "LCOE_plots.pdf" 
#' 
#' @author Felix Schreyer
#' @export
#' @importFrom magclass read.report mbind 
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom ggplot2 ggplot facet_wrap geom_errorbar ggtitle xlab scale_y_continuous scale_fill_discrete geom_col aes element_text
#' @importFrom dplyr left_join
#' @importFrom quitte order.levels sum_total

plotLCOE <- function(mif,y=c(2015,2020,2030,2040,2050,2060),reg=NULL,fileName="LCOE_plots.pdf") {
  
  # read model results 
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
          # expand data_new by old regions from data
          oldReg         <- getRegions(data)[-which(getRegions(data) %in% getRegions(data_new))]
          dummy_data_new <- new.magpie(oldReg,getYears(data_new),getNames(data_new),fill=NA)
          data_new       <- mbind(data_new,dummy_data_new)
          # compine old and new data
          data <- mbind(data,data_new) 
        } else {
          # expand data by new regions from data_new
          newReg     <- getRegions(data_new)[-which(getRegions(data_new) %in% getRegions(data))]
          dummy_data <- new.magpie(newReg,getYears(data),getNames(data),fill=NA)
          data       <- mbind(data,dummy_data)
          # expand data_new by old regions from data
          oldReg         <- getRegions(data)[-which(getRegions(data) %in% getRegions(data_new))]
          dummy_data_new <- new.magpie(oldReg,getYears(data_new),getNames(data_new),fill=NA)
          data_new       <- mbind(data_new,dummy_data_new)
          # compine old and new data
          data <- mbind(data,data_new) 
        }
        
      } else { 
        stop("the regional aggregation of the results are different, you might use reg='all_reg'")
      }
    }  
  }
  
  if (!(is.null(reg))) {
    if (!reg=="all_regi") {
      data <- data[reg,y,]
    } else {
      data <- data[,y,]
    }  
  } else {
    data <- data[,y,]
  }
  
  # ---- Open output-pdf ----
  template <-  c("\\documentclass[a4paper,landscape]{article}",
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
  
  # initialize variables used in dplyr pipes and ggplot below
  scenario <- NULL
  tech <- NULL
  period <- NULL
  region <- NULL
  variable <- NULL
  value <- NULL
  Total <- NULL
  
  aes <- NULL
  element_text <- NULL
  
  
  # plot configurations
  techs <- c("pc", "igcc", "ngt","ngcc", "pcc", "ngccc","tnrs","hydro","spv","wind","csp", "bioigccc")
  ylimit.up <- 150
  ylimit.lo <- -100
 
  ## plot ##

  # loop over regions
  for (i in 1:length(getRegions(data))) {
    reg <- getRegions(data)[i]
    #loop over scenarios
    for (j in 1:length(getNames(data, dim = 1))) {
      plot.scen <- getNames(data, dim = 1)[j]
      
      # rearrange data
      
      df.LCOE <- as.quitte(data[reg,,])
      
      # extract tech and cost dimensions from LCOE|Tech|Cost character strucutre
      df.LCOE$tech <- strsplit(as.character(df.LCOE$variable), "\\|") 
      df.LCOE$tech <- sapply(df.LCOE$tech, "[[", 2)
      
      df.LCOE$variable <- strsplit(as.character(df.LCOE$variable), "\\|") 
      df.LCOE$variable <- sapply(df.LCOE$variable, "[[", 3)
      
      df.LCOE <- df.LCOE %>%
        filter( scenario %in% plot.scen & tech %in% techs) %>% 
        order.levels(variable = c("Curtailment Cost","CCS Cost","CO2 Cost","Grid Cost","Storage Cost",
                                  "OMV Cost", "OMF Cost", "Fuel Cost",  "Investment Cost"))
      
      df.LCOE.total <- df.LCOE %>%
        sum_total(variable, value) %>%
        filter( variable == "Total") %>%
        rename(Total=value) %>%
        select(period, region, tech, Total)
      
      # if total LCOE is above ylimit, set Investment Cost to ylimit such that 
      # the stacked bar is not cuff off before 
      df.LCOE <- df.LCOE %>%
        left_join(df.LCOE.total) %>%
        mutate(value = ifelse( variable == "Investment Cost" & Total > ylimit.up, ylimit.up, value))
      
      
      p <- ggplot(cex=2) +
        geom_col(data=df.LCOE, aes(tech, value, fill=variable), alpha=0.6, position = "stack") +
        geom_errorbar(data=filter(df.LCOE.total, Total!=0), aes(x=tech, ymin=Total, ymax=Total)) +
        theme(legend.position="bottom") +
        xlab("Technology") +
        scale_y_continuous("Levelized Cost of Electricity\n [US$2005/MWh]", limits=c(ylimit.lo,ylimit.up)) +
        facet_wrap(~period, ncol=2) +
        scale_fill_discrete(name = "Cost Part") +
        theme(text = element_text(size=50), 
              axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3), 
              legend.position="bottom" , legend.key.size = unit(2, "cm")) +
        ggtitle(paste0("Region: ",reg,", Scenario: ", plot.scen))
     
      #swfigure(sw,print,p,sw_option="height=20,width=35", dpi=1800) 
      swfigure(sw,print,p, sw_option="height=25,width=40") 
    }
  }

  # Close output-pdf
  swclose(sw)
}
  