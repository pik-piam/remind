#' Read in LCOE mif and write LCOE_plots.pdf
#' 
#' Read in all information from LCOE mif file and create
#' the LCOE_plots.pdf
#' 
#' @param LCOEfile a path to the LCOE reporting csv file of the scenario to be plotted
#' @param gdx gdx file of run
#' @param y time span for the data in line plots, default: y=c(2015, 2020,2030,2040,2050,2060) 
#' @param reg region(s) in focus, reg ="all_regi" shows all regions if the mifs contain different regions
#' @param fileName name of the pdf, default = "LCOE_plots.pdf" 
#' 
#' @author Felix Schreyer
#' @export
#' @importFrom magclass read.report mbind 
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom ggplot2 ggplot facet_wrap geom_errorbar ggtitle xlab scale_y_continuous scale_fill_discrete geom_col aes element_text theme_bw sec_axis scale_x_discrete scale_linetype_identity
#' @importFrom dplyr left_join
#' @importFrom quitte order.levels sum_total getRegs revalue.levels
#' @importFrom tidyr spread gather
#' @importFrom data.table frollmean


plotLCOE <- function(LCOEfile, gdx, y=c(2015,2020,2030,2040,2050,2060),reg="all_regi",fileName="LCOE_plots.pdf") {
  
  
  df.LCOE.in <- read.csv(LCOEfile, sep = ";") 
  
  # # read LCOE reporting files
  # df.LCOE.fromFiles <- NULL
  # for (i in 1:length(reportFiles)) {
  #   df.LCOE.in <- read.csv(reportFiles[i], sep = ";")  
  #   df.LCOE.fromFiles <- rbind(df.LCOE.fromFiles, df.LCOE.in)
  # }
  
  
  # # read model results 
  # data <- NULL
  # for(i in 1:length(mif)){
  #   data_new <- read.report(mif[i],as.list=FALSE)
  #   if (magclass::getNames(data_new,fulldim = TRUE)[["scenario"]] %in% magclass::getNames(data,fulldim = TRUE)[["scenario"]]) magclass::getNames(data_new) <- gsub(magclass::getNames(data_new,fulldim = TRUE)["scenario"],paste0(magclass::getNames(data_new,fulldim = TRUE)["scenario"],i),magclass::getNames(data_new))
  #   if(all(getRegions(data) %in% getRegions(data_new))) {
  #     data <- mbind(data,data_new)
  #   } else {
  #     if(is.null(reg)){
  #       stop("the regional aggregation of the results are different, you might use reg='all_regi'")
  #     } else if(reg=="all_regi"){
  #       if(all(getRegions(data_new) %in% getRegions(data))) {
  #         # expand data_new by old regions from data
  #         oldReg         <- getRegions(data)[-which(getRegions(data) %in% getRegions(data_new))]
  #         dummy_data_new <- new.magpie(oldReg,getYears(data_new),getNames(data_new),fill=NA)
  #         data_new       <- mbind(data_new,dummy_data_new)
  #         # compine old and new data
  #         data <- mbind(data,data_new) 
  #       } else {
  #         # expand data by new regions from data_new
  #         newReg     <- getRegions(data_new)[-which(getRegions(data_new) %in% getRegions(data))]
  #         dummy_data <- new.magpie(newReg,getYears(data),getNames(data),fill=NA)
  #         data       <- mbind(data,dummy_data)
  #         # expand data_new by old regions from data
  #         oldReg         <- getRegions(data)[-which(getRegions(data) %in% getRegions(data_new))]
  #         dummy_data_new <- new.magpie(oldReg,getYears(data_new),getNames(data_new),fill=NA)
  #         data_new       <- mbind(data_new,dummy_data_new)
  #         # compine old and new data
  #         data <- mbind(data,data_new) 
  #       }
  #       
  #     } else { 
  #       stop("the regional aggregation of the results are different, you might use reg='all_regi'")
  #     }
  #   }  
  # }
  # 
  # if (!(is.null(reg))) {
  #   if (!reg=="all_regi") {
  #     data <- data[reg,y,]
  #   } else {
  #     data <- data[,y,]
  #   }  
  # } else {
  #   data <- data[,y,]
  # }
  # 
  
  
  # initialize variables used in dplyr pipes and ggplot below
  scenario <- NULL
  tech <- NULL
  period <- NULL
  region <- NULL
  variable <- NULL
  value <- NULL
  Total <- NULL
  type <- NULL
  vm_deltaCap <- NULL
  LCOE <- NULL
  all_te <- NULL
  output <- NULL
  aes <- NULL
  element_text <- NULL
  cost <- NULL
  Total <- NULL
  `CO2 Tax Cost` <- NULL
  `Second Fuel Cost` <- NULL
  TooHigh <- NULL
  `Total LCOE` <- NULL
  
  

  # ---- plot settings ----
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

  
  ylimit.up <- 300 # y axis max
  plot.period <- y
  plot.scen <- getScenarios(df.LCOE.in)
  #plot.period <- c(2020,2030,2040,2050)
  plot_theme <- theme_bw() + theme(axis.text.x = element_text(angle=90, size=40, vjust=0.3),
                                   text = element_text(size=50),
                                   axis.text.y = element_text(size=60),
                                   legend.text = element_text(size=50),
                                   legend.key.size = unit(6,"line"))
  
  
  # plot_theme <- theme_bw() + theme(axis.text.x = element_text(angle=90, size=20, vjust=0.3),
  #                                  text = element_text(size=20),
  #                                  axis.text.y = element_text(size=20),
  #                                  legend.text = element_text(size=20),
  #                                  legend.key.size = unit(2,"line"))
  # colors of cost components
  cost.colors <- c("Investment Cost" = "azure4", "OMF Cost" = "darkcyan", "OMV Cost" = "cyan",
                   "Fuel Cost" = "orange3", "CO2 Tax Cost" = "indianred", "CO2 Provision Cost" = "slategray1",
                   "Second Fuel Cost" = "sandybrown", "VRE Storage Cost" = "mediumpurple3",
                   "Grid Cost" = "darkseagreen3", "CCS Tax Cost" = "chartreuse4", 
                   "CCS Cost" = "darkblue", "Flex Tax" = "yellow1")
  
  ### end plot settings
  
  # read in capacity additions from .mif, for second y axes of plot
  vm_deltaCap <- readGDX(gdx, "vm_deltaCap", field = "l", restore_zeros = F)
  
  
  # calculate 15-year moving average capacity additions
  df.dC <- as.quitte(dimSums(vm_deltaCap, dim=3.2)*1e3) %>%  
    rename(tech=all_te, vm_deltaCap = value) %>% 
    filter(period >= 2005) %>% 
    select(region, period, tech, vm_deltaCap) %>% 
    group_by(region, tech) %>% 
    mutate( vm_deltaCap = frollmean(vm_deltaCap, 3, align = "center", fill = NA)) %>% 
    ungroup() 
  
  
  # relabel outputs and only plot most important technologies per output
  relabel.outputs <- c("seliqfos" = "seliq", "seliqbio" = "seliq", "segafos" = "segas", "segabio" = "segas")
  
  plot.outputs <- c("seel","seliq","segas")
  plot.techs <- c("pc", "igcc", "ngt","ngcc", "ngccc","tnrs","hydro","spv","wind","csp", "biochp","bioigccc",
                  "refliq","biodiesel","bioftrec","bioftcrec", "MeOH",
                  "gastr", "biogas","h22ch4")
  
  # join capacity additions with LCOE
  df.LCOE.dC.join <- df.LCOE.in %>%
                      filter( type == "marginal") %>% 
                      rename(LCOE = value) %>%
                      left_join(df.dC) %>% 
                      gather(variable, value, LCOE, vm_deltaCap) %>%   
                      revalue.levels(output = relabel.outputs) %>% 
                      # do away with cost dimension for vm_deltaCap to save memory, filter for periods
                      filter( cost == "Investment Cost" | variable == "LCOE",
                              period %in% plot.period,
                              output %in% plot.outputs, tech %in% plot.techs)
  

  ### loop to plot LCOE and capacity additions per region and energy output
  
  # have to do plotting loop with lapply such that variable scale of second y axes works 
  # (normal loop cannot do this)
  LCOE.grouped <- split(df.LCOE.dC.join, list(df.LCOE.dC.join$output, df.LCOE.dC.join$region))
  make_plot <- function(df.LCOE.grouped) {

    # region and output
    plot.reg <- getRegs(df.LCOE.grouped)
    plot.output <- unique(df.LCOE.grouped$output)
    
    # remove LCOE that are higher than y axis limit
    df.LCOE.plot <- df.LCOE.grouped %>% 
                      select(period, tech, cost, variable, value) %>% 
                      spread(cost, value) %>%  
                      mutate( TooHigh = ifelse(variable == "LCOE", ifelse((`Total LCOE` - ifelse(`CO2 Tax Cost` < 0, `CO2 Tax Cost`, 0)
                                                            - ifelse(`Second Fuel Cost` < 0, `Second Fuel Cost`, 0)) > ylimit.up,T,F), F)) %>% 
                      gather(cost, value,  -period, -tech, -variable, -TooHigh) %>% 
                      filter(cost == "Investment Cost" | variable == "LCOE", TooHigh == FALSE)
                            
    
    
    # calculate lower y axis limit, limit of second y axes
    df.LCOE.min <- df.LCOE.plot %>% 
    # the two cost components that can be negative
                    filter(cost %in% c("CO2 Tax Cost","Second Fuel Cost"))
    
    ylimit.lo <- min(df.LCOE.min$value, na.rm = T)-50 # y axis min.
    
    # maximum capacity addition in plot
    df.dC.max <- df.LCOE.plot %>% 
                  filter(variable == "vm_deltaCap")
    
    sec.axis.limit <- max(df.dC.max$value, na.rm = T) + 1 # second axis max
    
    
    
    df.LCOE.plot.out <- df.LCOE.plot %>% 
                          # scale second axis variable s.t it fits into plot
                          mutate(value = ifelse(variable == "vm_deltaCap",
                            value / sec.axis.limit * ylimit.up,
                            value)) %>% 
                          # remove Total LCOE
                          filter( cost != "Total LCOE") %>% 
                          order.levels(cost = c("Flex Tax","CCS Cost","CCS Tax Cost","Grid Cost","VRE Storage Cost","Second Fuel Cost","CO2 Provision Cost",
                            "CO2 Tax Cost", "Fuel Cost", "OMV Cost", 
                            "OMF Cost", "Investment Cost"))
    
    df.LCOE.total <- df.LCOE.plot %>% 
                      filter(cost == "Total LCOE")
    
    
    p.LCOE <- ggplot() +
      geom_col(data=df.LCOE.plot.out %>% filter(variable == "LCOE"),
               aes(tech, value, fill=cost), alpha=0.7) +
      geom_point(data=df.LCOE.plot.out %>% filter(variable == "vm_deltaCap"),
                 aes(tech, value), 
                 alpha=1, size=7, shape=1, color="black",stroke = 3) +
      geom_point(data=df.LCOE.plot.out %>% filter(variable == "vm_deltaCap"),
                 aes(tech, value), 
                 alpha=0.9, size=5, color="white") +
      geom_errorbar(data=df.LCOE.total, aes(x=tech, ymin=value, ymax=value, linetype="solid"), size=2) +
      scale_linetype_identity(name = '', guide = 'legend',labels = c('Total LCOE')) +
      facet_wrap(~period) +
      plot_theme +
      scale_fill_manual(values=cost.colors, name="LCOE components") +
      scale_x_discrete("Technology") +
      scale_y_continuous("LCOE (USD2015/MWh)",
                         limits = c(ylimit.lo,ylimit.up),
                         sec.axis = sec_axis(~ . * sec.axis.limit / ylimit.up,
                                             name = paste0("Capacity Additions in GW/yr\n(15-year average)")))+  
      ggtitle(paste0(plot.scen,", ",plot.reg,":\nMarginal LCOE and capacity additions of ", plot.output," technologies"))
    
    swfigure(sw,print,p.LCOE, sw_option="height=20,width=35", dpi=1800)
    
  }
 
  sw <- swopen(fileName,template = template)
  fig_list <- lapply(LCOE.grouped, make_plot)
  swclose(sw)

}

