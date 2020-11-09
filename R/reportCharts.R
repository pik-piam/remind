#' @title Create REMIND reporting charts
#' @description Create REMIND reporting plots
#'
#' @param gdx GDX file path
#' @param regionMapping regionMapping file name
#' @param hist historic file path
#' @param reportfile REMIND mif report file path
#' @param chartType plot type to include in output object. Either "plotly", "ggplot" or both c("plotly","ggplot")
#' 
#' @author Renato Rodrigues
#' 
#' @examples
#'
#'   \dontrun{
#'     reportCharts(gdx="fulldata.gdx",regionMapping="./config/regionmappingH12.csv")
#'   }
#'
#' @importFrom madrat toolGetMapping
#' @importFrom highcharter highchart hc_plotOptions hc_add_series_list hc_tooltip hc_mapNavigation list_parse
#' @importFrom mip plotstyle mipArea shorten_legend mipLineHistorical
#' @importFrom tidyr %>%
#' @importFrom gdx readGDX
#' @importFrom dplyr group_by_	do_
#' @importFrom ggplot2 ggplot theme theme_minimal expand_limits labs element_blank ylab labeller geom_vline scale_color_manual geom_area  
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats as.formula
#' @importFrom scales pretty_breaks
#'
#' @export

reportCharts <- function(gdx=NULL, regionMapping=NULL, hist=NULL, reportfile=NULL, chartType="ggplot") {
  
  # error checking
  if(!(!(is.null(gdx)) && file.exists(gdx)) && !(!(is.null(reportfile)) && file.exists(reportfile))){
    warning("reportCharts function require either a valid gdx or a mif report file path.")
    return()
  }
  if(is.null(regionMapping)){
    warning("REMIND dashboard require a regionMapping file.")
    return()
  }
  
  # if no reporting-mif-file is provided create reporting
  if(is.null(reportfile)){ 
    reportfile <- convGDX2MIF(gdx)
  }
  
  data <- read.report(reportfile,as.list=FALSE)
  data <- collapseNames(data)
  data <- data[,getYears(data)<="y2100",]
  
  ### Aestethics Options 
  aestethics <- list("alpha"=0.6,
                     "line" = list("size"= 2/3.78),
                     "y-axis" = list("color"="#878787","size"= 1/3.78)
                     )
  
  # variable labels renaming for facet grids
  variable_label <- c(
    "Emi|CO2" = "Carbon Dioxide (Mt CO2)",
    "Emi|CH4" = "Methane (Mt CH4)",
    "Emi|N2O" = "Nitrous Oxide (kt N2O)",
    
    "Emi|PFC" = "Perfluorocarbons (kt CF4-equiv)",
    "Emi|HFC" = "Hydrofluorocarbons (kt HFC134a-equiv)",
    "Emi|SF6" = "Sulphur hexafluoride (kt SF6)",
    
    "Emi|NH3" = "NH3 (Mt)",
    "Emi|NOX" = "NOX (Mt)",
    "Emi|OC" = "OC (Mt)",
    "Emi|SO2" = "SO2 (Mt)",
    "Emi|VOC" = "VOC (Mt)",
    "Emi|BC" = "BC (Mt)",
    "Emi|C2F6" = "C2F6 (kt)",
    "Emi|C6F14" = "C6F14 (kt C6F14)",
    
    "Price|Final Energy|Buildings" = "Buildings",
    "Price|Final Energy|Industry" = "Industry",
    "Price|Final Energy|Transport" = "Transport",
    
    "Price|Final Energy|Electricity|Transport" = "Electricity",
    "Price|Final Energy|Liquids|Transport" = "Liquids",
    "Price|Final Energy|Hydrogen|Transport" = "Hydrogen",
    
    "Price|Final Energy|Electricity|Industry" = "Electricity",
    "Price|Final Energy|Heating Oil|Industry" = "Heating Oil",
    "Price|Final Energy|Solids|Industry" = "Solids",
    "Price|Final Energy|Gases|Industry" = "Gases",
    "Price|Final Energy|Heat|Industry" = "Heat",
    "Price|Final Energy|Hydrogen|Industry" = "Hydrogen",
    
    "Price|Final Energy|Electricity|Buildings" = "Electricity",
    "Price|Final Energy|Heat|Buildings" = "Heat",
    "Price|Final Energy|Gases|Buildings" = "Gases",
    "Price|Final Energy|Hydrogen|Buildings" = "Hydrogen",
    "Price|Final Energy|Heating Oil|Buildings" = "Heating Oil",
    "Price|Final Energy|Solids|Buildings" = "Solids"
  )
  
  missingColors <- c(
    "DEU"=brewer.pal(9,"Oranges")[9],
    "EUW"=brewer.pal(9,"YlOrRd")[6], "EWN"=brewer.pal(9,"YlOrRd")[6], "FRA"=brewer.pal(9,"YlOrRd")[7],                 
    "EUS"=brewer.pal(9,"YlOrRd")[2], "ESW"=brewer.pal(9,"YlOrRd")[2], "ESC"=brewer.pal(9,"YlOrRd")[3],  
    "EUC"=brewer.pal(9,"Greys")[5],  "ECS"=brewer.pal(9,"Greys")[3],  "ECE"=brewer.pal(9,"Greys")[5],
    "EUN"=brewer.pal(9,"Blues")[6],  "ENC"=brewer.pal(9,"Blues")[5],  "UKI"=brewer.pal(9,"Blues")[6],
    "NEU"=brewer.pal(9,"YlGn")[5],   "NEN"=brewer.pal(9,"YlGn")[5],   "NES"=brewer.pal(9,"YlGn")[3],
    "CHE"="#78C679",   "ENN"="#78C679",   "ESE"="#D9F0A3","EUI"="#78C679",   "ROE"="#D9F0A3", #older EU
    "SSA"="#00BAFF", "REF"="#D900BC", "CAZ"="#007362", "CHA"="#F24200", #maps
    "F-Gases"="#ff9977", "N2O|Waste"="#bcbc6d", "N2O|Industry"="#666666", #Kyoto Gases
    "Energy|Supply|Non-Elec"="#661a00", "Energy|Supply|Electricity|Gross"="#993a44", "FFaI|Industry|Process"="#aa5a00", "Energy|Demand|Industry|Gross"="#7777ff", "Buildings|Direct"="#4444bb", "Transport|Demand"="#222288", "Carbon Capture and Storage|Biomass|Neg"="#00aa00", "Land-Use Change"="#116611", # CO2 emissions per sector
    "Emi|CO2" = "#333333", "Emi|CH4" = "#3333aa", "Emi|N2O" = "#aa3333", # Main Greenhouse Gases Emissions
    "Emi|PFC" = "#000000", "Emi|HFC" = "#000000", "Emi|SF6" = "#000000", # F-Gases
    "Emi|NH3" = "#000000", "Emi|NOX" = "#000000", "Emi|OC" = "#000000", "Emi|SO2" = "#000000", "Emi|VOC" = "#000000", "Emi|BC" = "#000000", "Emi|C2F6" = "#000000", "Emi|C6F14" = "#000000", # Other Gases
    "Biomass|World Market" = "#005900", "Coal|Primary Level" = "#0c0c0c", "Natural Gas|Primary Level" = "#999959", "Crude Oil|Primary Level" = "#cc7500", # PE 
    "Uranium"="EF7676", #Uranium extraction
    "Lignocellulosic"="#005900",
    "1st Generation"="#008c00",  
    "Traditional Biomass" = "#000000", #SE Solids
    "Industry" = "#000000", "Heating Oil" = "#E41A1C", #FE prices
    "Liquids|Oil" = "#cc7500", "Liquids|Coal" = "#0c0c0c", "Liquids|Biomass" = "#005900", "Solids|Coal" = "#0c0c0c", "Solids|Biomass" = "#005900" #FE 
  )
  missingColorsdf <- data.frame(row.names=names(missingColors), color=missingColors)
  
  plotlyButtonsToHide <- list('sendDataToCloud', 'zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'zoom3d', 'pan3d', 'orbitRotation', 'tableRotation', 'resetCameraDefault3d', 'resetCameraLastSave3d', 'hoverClosest3d', 'zoomInGeo', 'zoomOutGeo', 'resetGeo', 'hoverClosestGeo', 'hoverClosestGl2d', 'hoverClosestPie', 'resetSankeyGroup', 'toggleHover', 'resetViews', 'toggleSpikelines', 'resetViewMapbox')
  
  ### auxiliar function to format specific text
  unitSubscript <- function(unit){
    unit <- gsub("CO2", "CO\\<sub\\>2\\<\\/sub\\>", unit)
    unit <- gsub("CO2 equivalent", "CO\\<sub\\>2 equivalent\\<\\/sub\\>", unit)
    unit <- gsub("CH4", "CH\\<sub\\>4\\<\\/sub\\>", unit)
    unit <- gsub("N2O", "N\\<sub\\>2\\<\\/sub\\>O", unit)
    unit <- gsub("CF4", "CF\\<sub\\>4\\<\\/sub\\>", unit)
    unit <- gsub("SF6", "SF\\<sub\\>6\\<\\/sub\\>", unit)
    unit <- gsub("NH3", "NH\\<sub\\>3\\<\\/sub\\>", unit)
    unit <- gsub("NOX", "NO\\<sub\\>x\\<\\/sub\\>", unit)
    unit <- gsub("SO2", "SO\\<sub\\>2\\<\\/sub\\>", unit)
    unit <- gsub("C2F6", "C\\<sub\\>2\\<\\/sub\\>F\\<sub\\>6\\<\\/sub\\>", unit)
    unit <- gsub("C6F14", "C\\<sub\\>6\\<\\/sub\\>F\\<sub\\>14\\<\\/sub\\>", unit)
    return(unit)
  }
  
  ### Create output object
  out <- list()
  
  ### Define which region plots to create
  gdxRegions <- unique(readGDX(gdx, name = "regi2iso")[[1]])
  regionSubsetList <- toolRegionSubsets(gdx)
  mainRegions <-  c(gdxRegions[!gdxRegions %in% as.vector(unlist(sapply(regionSubsetList,function(x)x)))],names(regionSubsetList))
  
  if (is.null(regionSubsetList)){
    regionsList <- c(list("world"="GLO"),
                     list("regions"=mainRegions)) # main regions
  } else {
    regionsList <- c(list("world"="GLO"),
                     list("regions"=mainRegions), # main regions
                     regionSubsetList, # aggregated regions
                     list("DEU"="DEU")) #extra country
  }
  out$data$regions <- regionsList
  
  ## loading population info
  pop <- readGDX(gdx, name = "pm_pop")[,getYears(data),] # pop in billions
  pop <- mbind(pop,dimSums(pop,dim=1)) # adding GLO
  if (!is.null(regionSubsetList))
    pop <- mbind(pop, calc_regionSubset_sums(pop, regionSubsetList))
  pop <- as.quitte(pop)
  pop <- pop[c("region","period","value")]
  colnames(pop) <- c("region","period","population")
  pop$population <- pop$population*1e9
  
  ### Creating maps
  
  # #Creating map files (only needed if you include a new map plot using highcharter)
  # library(highcharter)
  # worldMap <- download_map_data("custom/world-palestine-highres")
  # #adding missing Kosovo iso3 label
  # for (i in 1:length(worldMap$features)){
  #   if (worldMap$features[i][[1]]$id == "KV"){
  #     worldMap$features[i][[1]]$properties$`iso-a3` <- "KOS"
  #   }
  # }
  # save(worldMap, file="WorldMap.RData")
  # europeMap <- download_map_data("custom/europe")
  # #adding missing Kosovo iso3 label
  # for (i in 1:length(europeMap$features)){
  #   if (europeMap$features[i][[1]]$id == "KV"){
  #     europeMap$features[i][[1]]$properties$`iso-a3` <- "KOS"
  #   }
  # }
  # save(europeMap, file="EuropeMap.RData")
  
  # loading previosuly created map files
  worldMap <- system.file("extdata","WorldMap.RData",package = "remind")
  load(worldMap) #updates worldMap object with world map file data
  #europeMap <- system.file("extdata","EuropeMap.RData",package = "remind")
  #load(europeMap) #updates europeMap object with europe map file data
  
  # loading region mapping
  regionMapping <- toolGetMapping(basename(regionMapping), type = "regional")
  
  # adding kosovo
  regionMapping <- rbind(regionMapping,data.frame(CountryCode = "KOS", X = "Kosovo", RegionCode = regionMapping[which(regionMapping$CountryCode=="SRB"),]$RegionCode))
  
  colors <- plotstyle(unique(regionMapping$RegionCode),unknown=missingColorsdf) # region colors
  
  series <- regionMapping %>% 
    group_by_(name = ~RegionCode) %>% 
    do_(data = ~list_parse(select(., CountryCode))) %>%
    ungroup() 
  series$color <- colors[series$name]
  
  out$maps$world <- highchart(type = "map") %>% 
    hc_plotOptions(map = list(allAreas = FALSE, joinBy = c("iso-a3", "CountryCode"), mapData = worldMap, borderColor = "#FAFAFA", borderWidth = 0.03)) %>% 
    hc_add_series_list(series) %>% 
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "Region: {point.series.name}<br>Country: {point.name}") %>% 
    hc_mapNavigation(enabled = TRUE) 
  
  # out$maps$europe <- highchart(type = "map") %>% 
  #   hc_plotOptions(map = list(allAreas = FALSE, joinBy = c("iso-a3", "CountryCode"), mapData = europeMap, borderColor = "#FAFAFA", borderWidth = 0.03)) %>% 
  #   hc_add_series_list(series) %>% 
  #   hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "Region: {point.series.name}<br>Country: {point.name}") %>% 
  #   hc_mapNavigation(enabled = TRUE)

  ### Creating Charts
  
  ##### Emissions
  
  # CO2 prices
  vars <- c("Price|Carbon (US$2005/t CO2)")
  
  color <- c("Price|Carbon"="#000000") #overwritting default mip color value for carbon price 
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," US$<sub>2005</sub>/t CO<sub>2</sub><br>CO<sub>2</sub> ",ifelse(reg=="GLO","World", as.character(df$region))," carbon price","<br>year: ",df$period)
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=~variable,text=~details, group = ~variable)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, scales="fixed") +
      theme_minimal() +
      expand_limits(y=c(0,100)) +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$emissions$'Carbon Price' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$emissions$'Carbon Price' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$emissions$'Carbon Price') <- names(regionsList)
  }
  
  out$legend$'Carbon Price'$description <- "<p>CO2 emissions price</p>"
  out$legend$'Carbon Price'$contents <- list("CO<sub>2</sub> price" =list("fill"=color["Price|Carbon"],"linetype"="solid"))
  out$legend$'Carbon Price'$units <- "US$<sub>2005</sub>/t CO<sub>2</sub> per year"
  
  # Kyoto Gases
  vars <- c("F-Gases" = "Emi|F-Gases (Mt CO2-equiv/yr)",
            "CO2 - Gross Fossil Fuels and Industry" = "Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)",
            "CH4 - Energy Supply and Demand" = "Emi|CH4|Energy Supply and Demand (Mt CH4/yr)", 
            "N2O - Energy Supply and Demand" = "Emi|N2O|Energy Supply and Demand (kt N2O/yr)",
            "N2O - Industry" = "Emi|N2O|Industry (kt N2O/yr)",
            "CH4 - Waste" = "Emi|CH4|Waste (Mt CH4/yr)",
            "N2O - Waste" = "Emi|N2O|Waste (kt N2O/yr)",
            "CH4 - Other" = "Emi|CH4|Other (Mt CH4/yr)",
            "CH4 - Land-Use Change" = "Emi|CH4|Land Use (Mt CH4/yr)",
            "N2O - Land-Use Change" = "Emi|N2O|Land Use (kt N2O/yr)",
            "CO2 - CCS Biomass" = "Emi|CO2|Carbon Capture and Storage|Biomass (Mt CO2/yr)",
            "CO2 - Land-Use Change" = "Emi|CO2|Land-Use Change (Mt CO2/yr)"
  )
  tmpData <- data[,,vars] # converting emissions to CO2 equivalent
  GWP <- c("CO2"=1,"CH4"=28,"N2O"=265)
  tmpData[,,c("Emi|CO2|Land-Use Change (Mt CO2/yr)","Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)","Emi|CO2|Carbon Capture and Storage|Biomass (Mt CO2/yr)")] <- tmpData[,,c("Emi|CO2|Land-Use Change (Mt CO2/yr)","Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)","Emi|CO2|Carbon Capture and Storage|Biomass (Mt CO2/yr)")] * GWP["CO2"]
  tmpData[,,c("Emi|CH4|Energy Supply and Demand (Mt CH4/yr)","Emi|CH4|Land Use (Mt CH4/yr)","Emi|CH4|Other (Mt CH4/yr)","Emi|CH4|Waste (Mt CH4/yr)")] <- tmpData[,,c("Emi|CH4|Energy Supply and Demand (Mt CH4/yr)","Emi|CH4|Land Use (Mt CH4/yr)","Emi|CH4|Other (Mt CH4/yr)","Emi|CH4|Waste (Mt CH4/yr)")] * GWP["CH4"]
  tmpData[,,c("Emi|N2O|Land Use (kt N2O/yr)","Emi|N2O|Energy Supply and Demand (kt N2O/yr)","Emi|N2O|Waste (kt N2O/yr)","Emi|N2O|Industry (kt N2O/yr)")] <- tmpData[,,c("Emi|N2O|Land Use (kt N2O/yr)","Emi|N2O|Energy Supply and Demand (kt N2O/yr)","Emi|N2O|Waste (kt N2O/yr)","Emi|N2O|Industry (kt N2O/yr)")] * GWP["N2O"]/1000
  tmpData[,,c("Emi|CO2|Carbon Capture and Storage|Biomass (Mt CO2/yr)")] <- -tmpData[,,c("Emi|CO2|Carbon Capture and Storage|Biomass (Mt CO2/yr)")] 
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(tmpData[reg,,vars])  
    df$details <- unitSubscript(paste0(round(df$value,2)," Mt CO<sub>2 equivalent</sub><br>CO<sub>2 equivalent</sub> ",ifelse(reg=="GLO","World", as.character(df$region))," ", gsub("\\|"," emissions by ",gsub("Emi\\|","",df$variable)),"<br>year: ",df$period))
    g <- ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
      geom_area(alpha=aestethics$alpha) +
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$emissions$'Kyoto Gases Emissions' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$emissions$'Kyoto Gases Emissions' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$emissions$'Kyoto Gases Emissions') <- names(regionsList)
  }
  
  out$legend$'Kyoto Gases Emissions'$description <- "<p>Kyoto Gases Emissions</p>"
  out$legend$'Kyoto Gases Emissions'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Kyoto Gases Emissions'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Kyoto Gases Emissions'$units <- "US$<sub>2005</sub>/t CO<sub>2</sub> per year"
  
  # CO2 emissions per sector
  vars <- c("Energy - Non-electricity" = "Emi|CO2|Energy|Supply|Non-Elec (Mt CO2/yr)",
            "Energy - Electricity" = "Emi|CO2|Energy|Supply|Electricity|Gross (Mt CO2/yr)",
            "Industrial Processes" = "Emi|CO2|FFaI|Industry|Process (Mt CO2/yr)",
            "Demand - Industry" = "Emi|CO2|Energy|Demand|Industry|Gross (Mt CO2/yr)",
             #          "Emi|CO2|Industrial Processes (Mt CO2/yr)",
            "Demand - Buildings" = "Emi|CO2|Buildings|Direct (Mt CO2/yr)",
            "Demand - Transport" = "Emi|CO2|Transport|Demand (Mt CO2/yr)",
            "Land use change" = "Emi|CO2|Carbon Capture and Storage|Biomass|Neg (Mt CO2/yr)",
            "Carbon Capture and Storage - Biomass" = "Emi|CO2|Land-Use Change (Mt CO2/yr)")
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- unitSubscript(paste0(round(df$value,2)," Mt CO<sub>2</sub><br>CO<sub>2</sub> ",ifelse(reg=="GLO","World", as.character(df$region))," emissions by ",names(vars[df$variable]),"<br>year: ",df$period))
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ unitSubscript(paste0(round(value,2)," Mt CO<sub>2</sub><br>","Total carbon emissions in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period))) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$emissions$'CO2 Emissions per Sector' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$emissions$'CO2 Emissions per Sector' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$emissions$'CO2 Emissions per Sector') <- names(regionsList)
  }
  
  out$legend$'CO2 Emissions per Sector'$description <- "<p>Annual CO2 emissions per sector</p>"
  out$legend$'CO2 Emissions per Sector'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'CO2 Emissions per Sector'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'CO2 Emissions per Sector'$contents <- c(list("Total CO<sub>2</sub> emissions" =list("fill"="#000","linetype"="dashed")),out$legend$'CO2 Emissions per Sector'$contents)
  out$legend$'CO2 Emissions per Sector'$units <- "Mt CO<sub>2</sub> per year"
  
  # Main Greenhouse Gases Emissions
  vars <- c("Emi|CO2 (Mt CO2/yr)",
            "Emi|CH4 (Mt CH4/yr)",
            "Emi|N2O (kt N2O/yr)")
  
  g <- lapply(regionsList, function(reg) {
    if (length(reg) == 1) # color as variable
      color <- plotstyle(as.character(gsub(" \\(.*","",vars)),unknown=missingColorsdf)
    else # color as region
      color <- plotstyle(unique(reg),unknown=missingColorsdf)
    df <- as.quitte(data[reg,,vars])  
    df$details <- unitSubscript(paste0(round(df$value,2), " ", gsub("/yr","",df$unit),"<br>",gsub(".* ","",gsub("/yr","",df$unit))," ",ifelse(reg=="GLO","World", as.character(df$region))," emissions", "<br>","year: ",df$period))  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=as.formula(paste("~",ifelse(length(reg) == 1,"variable","region"))),text=~details, group = ~region)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~variable, ncol=1, scales="free_y",labeller=labeller(variable = unitSubscript(variable_label))) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$emissions$'Main Greenhouse Gases Emissions' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$emissions$'Main Greenhouse Gases Emissions' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$emissions$'Main Greenhouse Gases Emissions') <- names(regionsList)
  }
  
  #out$legend$'Main Greenhouse Gases Emissions'$description <- "<p>Annual emissions for main greenhouse gases</p>"
  #out$legend$'Main Greenhouse Gases Emissions'$contents <- "placeholder - not needed"
  
  # F-Gases
  vars <- c("Emi|PFC (kt CF4-equiv/yr)",
            "Emi|HFC (kt HFC134a-equiv/yr)",
            "Emi|SF6 (kt SF6/yr)")

  g <- lapply(regionsList, function(reg) {
    if (length(reg) == 1) # color as variable
      color <- plotstyle(as.character(gsub(" \\(.*","",vars)),unknown=missingColorsdf)
    else # color as region
      color <- plotstyle(unique(reg),unknown=missingColorsdf)
    df <- as.quitte(data[reg,,vars])  
    df$details <- unitSubscript(paste0(round(df$value,2), " ", gsub("/yr","",df$unit),"<br>",gsub(".* ","",gsub("/yr","",df$unit))," ",ifelse(reg=="GLO","World", as.character(df$region))," emissions", "<br>","year: ",df$period))  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=as.formula(paste("~",ifelse(length(reg) == 1,"variable","region"))),text=~details, group = ~region)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~variable, ncol=1, scales="free_y",labeller=labeller(variable = unitSubscript(variable_label))) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$emissions$'F-Gases' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$emissions$'F-Gases' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$emissions$'F-Gases') <- names(regionsList)
  }
  
  #out$legend$'F-Gases'$description <- "<p>placeholder - not needed</p>"
  #out$legend$'F-Gases'$contents <- "placeholder - not needed"
  
  # Other Gases
  vars <- c("Emi|NH3 (Mt NH3/yr)",
            "Emi|NOX (Mt NOX/yr)",
            "Emi|OC (Mt OC/yr)",
            "Emi|SO2 (Mt SO2/yr)",
            "Emi|VOC (Mt VOC/yr)",
            "Emi|BC (Mt BC/yr)",
            "Emi|C2F6 (kt C2F6/yr)",
            "Emi|C6F14 (kt C6F14/yr)")
  
  g <- lapply(regionsList, function(reg) {
    if (length(reg) == 1) # color as variable
      color <- plotstyle(as.character(gsub(" \\(.*","",vars)),unknown=missingColorsdf)
    else # color as region
      color <- plotstyle(unique(reg),unknown=missingColorsdf)
    df <- as.quitte(data[reg,,vars])  
    df$details <- unitSubscript(paste0(round(df$value,2), " ", gsub("/yr","",df$unit),"<br>",gsub(".* ","",gsub("/yr","",df$unit))," ",ifelse(reg=="GLO","World", as.character(df$region))," emissions", "<br>","year: ",df$period))  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=as.formula(paste("~",ifelse(length(reg) == 1,"variable","region"))),text=~details, group = ~region)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      scale_y_continuous(breaks = pretty_breaks(2), limits = c(0, NA)) +
      facet_wrap(~variable, ncol=1, scales="free_y",labeller=labeller(variable = unitSubscript(variable_label))) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$emissions$'Other Gases' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$emissions$'Other Gases' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$emissions$'Other Gases') <- names(regionsList)
  }
  
  #out$legend$'Other Gases'$description <- "<p>placeholder - not needed</p>"
  #out$legend$'Other Gases'$contents <- "placeholder - not needed"

  
  ##### PRIMARY ENERGY
  
  #var.tot <-"PE (EJ/yr)"
  vars <- c("Coal with CCS" = "PE|Coal|w/ CCS (EJ/yr)", 
            "Coal without CCS" = "PE|Coal|w/o CCS (EJ/yr)",
            "Oil" = "PE|+|Oil (EJ/yr)",
            "Gas with CCS" = "PE|Gas|w/ CCS (EJ/yr)", 
            "Gas without CCS" = "PE|Gas|w/o CCS (EJ/yr)",
            "Biomass with CCS" = "PE|Biomass|w/ CCS (EJ/yr)", 
            "Biomass without CCS" = "PE|Biomass|w/o CCS (EJ/yr)",
            "Nuclear" = "PE|+|Nuclear (EJ/yr)", 
            "Hydro" = "PE|+|Hydro (EJ/yr)",
            "Geothermal" = "PE|+|Geothermal (EJ/yr)",
            "Solar" = "PE|+|Solar (EJ/yr)",  
            "Wind" = "PE|+|Wind (EJ/yr)")
  
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",gsub("\\|"," ",gsub("\\+\\|","",gsub("PE\\|","",as.character(df$variable)))), " primary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- list()
    # absolute chart
    g$abs <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
       geom_area(alpha=aestethics$alpha) +
       geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
       geom_line(data=df %>%
                   group_by_(~region,~period) %>%
                   summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                   mutate_(position= ~ value+max(value)/1000) %>%
                   mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total  primary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                   ungroup(),
                 aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
       facet_wrap(~region, scales="fixed") +
       theme_minimal() +
       labs(x = NULL, y = NULL) +
       scale_fill_manual(values = color) )
    # relative chart
    df <- df %>% group_by_(~region,~period) %>% mutate_(percent = ~ value/sum(value)*100) #creating percentage column
    df$percDetails <- paste0(round(df$percent,2)," %<br>",gsub("\\|"," ",gsub("\\+\\|","",gsub("PE\\|","",as.character(df$variable)))), " primary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g$perc <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~percDetails,group = ~variable)) +
        geom_bar(position = "fill",stat = "identity",alpha=aestethics$alpha) +
        facet_wrap(~region, scales="fixed") +
        theme_minimal() +
        labs(x = NULL, y = NULL) +
        scale_fill_manual(values = color) +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(limits = c(2005, NA)) )
    #per capita chart
    df <- suppressWarnings(left_join(df, pop, by=c("region", "period")))
    df <- df %>% group_by_(~region,~period) %>% mutate_(percapita = ~ value/population) #creating per capita column
    df$percapitaDetails <- paste0(round(df$percapita,2)," EJ per capita<br>",gsub("\\|"," ",gsub("\\+\\|","",gsub("PE\\|","",as.character(df$variable)))), " primary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g$percapita <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~percapita,fill=~variable,text=~percapitaDetails,group = ~variable)) +
                                 geom_area(alpha=aestethics$alpha) +
                                 geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                                 geom_line(data=df %>%
                                             group_by_(~region,~period) %>%
                                             summarise_(percapita = ~sum(percapita,na.rm=TRUE)) %>%
                                             mutate_(position= ~ percapita+max(percapita)/1000) %>%
                                             mutate_(details= ~ paste0(round(percapita,2)," EJ per capita<br>","Total  primary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                             ungroup(),
                                           aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                                 facet_wrap(~region, scales="fixed") +
                                 theme_minimal() +
                                 labs(x = NULL, y = NULL) +
                                 scale_fill_manual(values = color) )
    if (length(reg) == 1){
      g$abs <- g$abs + theme(strip.text.x = element_blank())
      g$perc <- g$perc + theme(strip.text.x = element_blank())
      g$percapita <- g$percapita + theme(strip.text.x = element_blank())
    }
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$PE$'Total Primary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$PE$'Total Primary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- lapply(names(g[[reg]]), function(type) {
        gg <- ggplotly(g[[reg]][[type]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
        return(gg)
      })
      names(ggplotly) <- names(g[[reg]])
      return(ggplotly)
    })  
    names(out$plotly$PE$'Total Primary Energy') <- names(regionsList)
  }
  
  out$legend$'Total Primary Energy'$description <- "<p>Primary energy per carrier</p>"
  out$legend$'Total Primary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Total Primary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Total Primary Energy'$contents <- c(list("Total primary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Total Primary Energy'$contents)
  out$legend$'Total Primary Energy'$units <- c("EJ per year","Percentage share","EJ per capita")
  
  # PE prices
  vars <- c("Biomass - World Market" = "Price|Biomass|World Market (US$2005/GJ)",
            "Coal - Primary Level" = "Price|Coal|Primary Level (US$2005/GJ)",
            "Natural Gas - Primary Level" = "Price|Natural Gas|Primary Level (US$2005/GJ)",
            "Crude Oil - Primary Level"="Price|Crude Oil|Primary Level (US$2005/GJ)")
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2), " US$<sub>2005</sub>/GJ<br>",gsub("\\|","",regmatches(df$variable, gregexpr("\\|.*?\\|", df$variable))), " ", ifelse(reg=="GLO","World", as.character(df$region))," price","<br>year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=~variable,text=~details, group = ~variable)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color,alpha=aestethics$alpha) + # vertical line at initial year
      facet_wrap(~region, ncol=3, scales="free") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$PE$'Primary Energy Prices' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$PE$'Primary Energy Prices' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$PE$'Primary Energy Prices') <- names(regionsList)
  }
  
  out$legend$'Primary Energy Prices'$description <- "<p>Primary energy prices</p>"
  out$legend$'Primary Energy Prices'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"="solid")) })
  names(out$legend$'Primary Energy Prices'$contents) <- names(gsub(" \\(.*","",vars))
  out$legend$'Primary Energy Prices'$units <- "US$<sub>2005</sub>/GJ per year"
  
  # Coal production
  vars <- c("PE|Production|Net|Coal (EJ/yr)")
  
  color <- plotstyle("Coal")
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2),"<br>", ifelse(reg=="GLO","World", as.character(df$region)), " Coal production","<br>year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
      geom_area(alpha=aestethics$alpha) +
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = color)
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$PE$'Coal production' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$PE$'Coal production' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.02
      return(ggplotly)
    })
    names(out$plotly$PE$'Coal production') <- names(regionsList)
  }
  
  out$legend$'Coal production'$description <- "<p>Net Coal production.</p>"
  out$legend$'Coal production'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"=NULL)) })
  names(out$legend$'Coal production'$contents) <- gsub(".*\\|","",gsub(" \\(.*","",vars))
  out$legend$'Coal production'$units <- "EJ/yr"
  
  # Gas production
  vars <- c("PE|Production|Net|Gas (EJ/yr)")
  
  color <- "#999959"
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2),"<br>", ifelse(reg=="GLO","World", as.character(df$region)), " Gas production","<br>year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
      geom_area(alpha=aestethics$alpha) +
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = color)
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$PE$'Gas production' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$PE$'Gas production' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.02
      return(ggplotly)
    })
    names(out$plotly$PE$'Gas production') <- names(regionsList)
  } 
  
  out$legend$'Gas production'$description <- "<p>Net Gas production.</p>"
  out$legend$'Gas production'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"=NULL)) })
  names(out$legend$'Gas production'$contents) <- gsub(".*\\|","",gsub(" \\(.*","",vars))
  out$legend$'Gas production'$units <- "EJ/yr"
  
  # Oil production
  vars <- c("PE|Production|Net|Oil (EJ/yr)")
  
  color <- "#cc7500"
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2),"<br>", ifelse(reg=="GLO","World", as.character(df$region)), " Oil production","<br>year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
      geom_area(alpha=aestethics$alpha) +
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = color)
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$PE$'Oil production' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$PE$'Oil production' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.02
      return(ggplotly)
    })
    names(out$plotly$PE$'Oil production') <- names(regionsList)
  } 
  
  out$legend$'Oil production'$description <- "<p>Net Oil production.</p>"
  out$legend$'Oil production'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"=NULL)) })
  names(out$legend$'Oil production'$contents) <- gsub(".*\\|","",gsub(" \\(.*","",vars))
  out$legend$'Oil production'$units <- "EJ/yr"
  
  # Uranium production
  vars <- c("PE|Production|Net|Uranium [Energy] (EJ/yr)")
  
  color <- plotstyle("Uranium")
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2),"<br>", ifelse(reg=="GLO","World", as.character(df$region)), " Uranium production","<br>year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
      geom_area(alpha=aestethics$alpha) +
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = color)
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$PE$'Uranium production' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$PE$'Uranium production' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.02
      return(ggplotly)
    })
    names(out$plotly$PE$'Uranium production') <- names(regionsList)
  }  
  
  out$legend$'Uranium production'$description <- "<p>Net Uranium production.</p>"
  out$legend$'Uranium production'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"=NULL)) })
  names(out$legend$'Uranium production'$contents) <- gsub(".*\\|","",gsub(" \\(.*","",vars))
  out$legend$'Uranium production'$units <- "EJ/yr"
  
  # Biomass production
  vars <- c("PE|Production|Biomass|+|Lignocellulosic (EJ/yr)",
            "PE|Production|Biomass|+|1st Generation (EJ/yr)")
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ/yr<br>", ifelse(reg=="GLO","World", as.character(df$region)),gsub(".*\\|","",gsub(" \\(.*","",df$variable)), " Biomass production<br>", "year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
      geom_area(alpha=aestethics$alpha) +
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = color)
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$PE$'Biomass production' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$PE$'Biomass production' <- lapply(names(regionsList), function(reg) {
       ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
       for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
         if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
           ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.02
         return(ggplotly)
    })
    names(out$plotly$PE$'Biomass production') <- names(regionsList)
  }  
  
  out$legend$'Biomass production'$description <- "<p>Energy crops Biomass production.</p>"
  out$legend$'Biomass production'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"=NULL)) })
  names(out$legend$'Biomass production'$contents) <- gsub(".*\\|","",gsub(" \\(.*","",vars))
  out$legend$'Biomass production'$units <- "EJ/yr"
  
  ##### SECONDARY ENERGY
  
  # SE price
  vars <- c("Price|Secondary Energy|Biomass (US$2005/GJ)",
            "Price|Secondary Energy|Electricity (US$2005/GJ)",
            "Price|Secondary Energy|Gases (US$2005/GJ)",
            "Price|Secondary Energy|Heat (US$2005/GJ)",
            "Price|Secondary Energy|Hydrogen (US$2005/GJ)",
            "Price|Secondary Energy|Liquids (US$2005/GJ)",
            "Price|Secondary Energy|Solids (US$2005/GJ)")
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2), " US$<sub>2005</sub>/GJ<br>",ifelse(reg=="GLO","World", as.character(df$region))," ",gsub("Price\\|Secondary Energy\\|","",df$variable), " price<br>", "year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=~variable,text=~details, group = ~variable)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, ncol=3, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Secondary Energy Prices' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Secondary Energy Prices' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$SE$'Secondary Energy Prices') <- names(regionsList)
  }    
    
  out$legend$'Secondary Energy Prices'$description <- "<p>Secondary energy prices</p>"
  out$legend$'Secondary Energy Prices'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"="solid")) })
  names(out$legend$'Secondary Energy Prices'$contents) <- gsub(".*\\|","",gsub(" \\(.*","",vars))
  out$legend$'Secondary Energy Prices'$units <- "US$<sub>2005</sub>/GJ per year" 
    
  #SE quantity
  vars <- c("SE|Electricity (EJ/yr)",
            "SE|Gases (EJ/yr)",
            "SE|Heat (EJ/yr)",
            "SE|Hydrogen (EJ/yr)",
            "SE|Liquids (EJ/yr)",
            "SE|Solids (EJ/yr)")
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",gsub("SE\\|","",as.character(df$variable)), " secondary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total secondary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Total Secondary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Total Secondary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      return(ggplotly)
    })
    names(out$plotly$SE$'Total Secondary Energy') <- names(regionsList)
  }    
  
  out$legend$'Total Secondary Energy'$description <- "<p>Secondary energy per carrier</p>"
  out$legend$'Total Secondary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Total Secondary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Total Secondary Energy'$contents <- c(list("Total secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Total Secondary Energy'$contents)
  out$legend$'Total Secondary Energy'$units <- "EJ per year"
  
  ### SE Electricity
  #var.tot <-"SE|Electricity (EJ/yr)"
  vars <- c("Coal with CCS"    = "SE|Electricity|Coal|w/ CCS (EJ/yr)", 
            "Coal without CCS" = "SE|Electricity|Coal|w/o CCS (EJ/yr)",
            "Oil"              = "SE|Electricity|Oil (EJ/yr)",
            "Gas with CCS"     = "SE|Electricity|Gas|w/ CCS (EJ/yr)", 
            "Gas without CCS"  = "SE|Electricity|Gas|w/o CCS (EJ/yr)",
            "Biomass with CCS" = "SE|Electricity|Biomass|w/ CCS (EJ/yr)",  
            "Biomass without CCS" = "SE|Electricity|Biomass|w/o CCS (EJ/yr)",
            "Nuclear"          = "SE|Electricity|Nuclear (EJ/yr)", 
            "Hydrogen"         = "SE|Electricity|Hydrogen (EJ/yr)",
            "Solar"            = "SE|Electricity|Solar (EJ/yr)", 
            "Wind"             = "SE|Electricity|Wind (EJ/yr)",
            "Geothermal"       = "SE|Electricity|Geothermal (EJ/yr)", 
            "Hydro"            = "SE|Electricity|Hydro (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars]) 
    df$value <- 1/0.0036 * df$value # converting from EJ to TWh
    df$unit <- "TWh/yr"
    df$details <- paste0(round(df$value,2)," TWh<br>",names(vars[df$variable]), " electricity secondary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," TWh<br>","Total electricity secondary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) 
                             #+ annotate("text", x=2015,y=Inf, label = unique(df$region), hjust = 0, vjust = 2, size=3.5)
                             )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Electricity Secondary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Electricity Secondary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$SE$'Electricity Secondary Energy') <- names(regionsList)
  }  
  
  out$legend$'Electricity Secondary Energy'$description <- "<p>Electricity secondary energy per carrier</p>"
  out$legend$'Electricity Secondary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Electricity Secondary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Electricity Secondary Energy'$contents <- c(list("Total electricity secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Electricity Secondary Energy'$contents)
  out$legend$'Electricity Secondary Energy'$units <- "Terawatt hour per year"
  
  
  ### SE Biomass
  
  ### SE Gases
  #var.tot <-"SE|Gases (EJ/yr)"
  vars <- c("Biomass with CCS" = "SE|Gases|Biomass|w/ CCS (EJ/yr)", 
            "Biomass without CCS" = "SE|Gases|Biomass|w/o CCS (EJ/yr)",
            "Coal with CCS"    = "SE|Gases|Coal|w/ CCS (EJ/yr)", 
            "Coal without CCS" = "SE|Gases|Coal|w/o CCS (EJ/yr)", 
            "Natural Gas"      = "SE|Gases|Natural Gas (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " gases secondary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total gases secondary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Gases Secondary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Gases Secondary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$SE$'Gases Secondary Energy') <- names(regionsList)
  } 
  
  out$legend$'Gases Secondary Energy'$description <- "<p>Gases secondary energy per carrier</p>"
  out$legend$'Gases Secondary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Gases Secondary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Gases Secondary Energy'$contents <- c(list("Total gases secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Gases Secondary Energy'$contents)
  out$legend$'Gases Secondary Energy'$units <- "EJ per year"
    
  ### SE Heat
  #var.tot <-"SE|Heat (EJ/yr)"
  vars <- c("Biomass"    = "SE|Heat|Biomass (EJ/yr)", 
            "Coal"       = "SE|Heat|Coal (EJ/yr)", 
            "Gas"        = "SE|Heat|Gas (EJ/yr)",
            "Geothermal" = "SE|Heat|Geothermal (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " heat secondary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total heat secondary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Heat Secondary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Heat Secondary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$SE$'Heat Secondary Energy') <- names(regionsList)
  } 
  
  out$legend$'Heat Secondary Energy'$description <- "<p>Heat secondary energy per carrier</p>"
  out$legend$'Heat Secondary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Heat Secondary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Heat Secondary Energy'$contents <- c(list("Total heat secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Heat Secondary Energy'$contents)
  out$legend$'Heat Secondary Energy'$units <- "EJ per year"
  
  ### SE Liquids
  #var.tot <-"SE|Liquids (EJ/yr)"
  vars <- c("Biomass"         = "SE|Liquids|Biomass (EJ/yr)", 
            "Coal with CCS"   = "SE|Liquids|Coal|w/ CCS (EJ/yr)",
            "Coal without CCS"= "SE|Liquids|Coal|w/o CCS (EJ/yr)", 
            "Gas with CCS"    = "SE|Liquids|Gas|w/ CCS (EJ/yr)",
            "Gas without CCS" = "SE|Liquids|Gas|w/o CCS (EJ/yr)", 
            "Oil"             = "SE|Liquids|Oil (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " liquids secondary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total liquids secondary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Liquids Secondary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Liquids Secondary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$SE$'Liquids Secondary Energy') <- names(regionsList)
  }  
    
  out$legend$'Liquids Secondary Energy'$description <- "<p>Liquids secondary energy per carrier</p>"
  out$legend$'Liquids Secondary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Liquids Secondary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Liquids Secondary Energy'$contents <- c(list("Total liquids secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Liquids Secondary Energy'$contents)
  out$legend$'Liquids Secondary Energy'$units <- "EJ per year" 
  
  ### SE Solids
  #var.tot <-"SE|Solids (EJ/yr)"
  vars <- c("Biomass"   = "SE|Solids|Biomass (EJ/yr)",
            "Traditional Biomass" = "SE|Solids|Traditional Biomass (EJ/yr)",
            "Coal"     = "SE|Solids|Coal (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " solids secondary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total solids secondary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Solids Secondary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Solids Secondary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$SE$'Solids Secondary Energy') <- names(regionsList)
  }  
  
  out$legend$'Solids Secondary Energy'$description <- "<p>Solids secondary energy per carrier</p>"
  out$legend$'Solids Secondary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Solids Secondary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Solids Secondary Energy'$contents <- c(list("Total solids secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Solids Secondary Energy'$contents)
  out$legend$'Solids Secondary Energy'$units <- "EJ per year"
  
  ### SE Hydrogen
  #var.tot <-"SE|Hydrogen (EJ/yr)"
  vars <- c("Biomass with CCS"    = "SE|Hydrogen|Biomass|w/ CCS (EJ/yr)", 
            "Biomass without CCS" = "SE|Hydrogen|Biomass|w/o CCS (EJ/yr)", 
            "Coal with CCS"       = "SE|Hydrogen|Coal|w/ CCS (EJ/yr)", 
            "Coal without CCS"    = "SE|Hydrogen|Coal|w/o CCS (EJ/yr)",
            "Gas with CCS"        = "SE|Hydrogen|Gas|w/ CCS (EJ/yr)", 
            "Gas without CCS"     = "SE|Hydrogen|Gas|w/o CCS (EJ/yr)",
            "Electricity"         = "SE|Hydrogen|Electricity (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " hydrogen secondary energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total hydrogen secondary energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$SE$'Hydrogen Secondary Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$SE$'Hydrogen Secondary Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$SE$'Hydrogen Secondary Energy') <- names(regionsList)
  }  
  
  out$legend$'Hydrogen Secondary Energy'$description <- "<p>Hydrogen secondary energy per carrier</p>"
  out$legend$'Hydrogen Secondary Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Hydrogen Secondary Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Hydrogen Secondary Energy'$contents <- c(list("Total hydrogen secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Hydrogen Secondary Energy'$contents)
  out$legend$'Hydrogen Secondary Energy'$units <- "EJ per year"
  
  ##### FINAL ENERGY
  
  #var.tot <-"FE (EJ/yr)"
  vars <- c("Electricity" = "FE|+|Electricity (EJ/yr)",
            "Solids"      = "FE|+|Solids (EJ/yr)",
            "Liquids"     = "FE|+|Liquids (EJ/yr)",
            "Gases"       = "FE|+|Gases (EJ/yr)",
            "Heat"        = "FE|+|Heat (EJ/yr)",
            "Hydrogen"    = "FE|+|Hydrogen (EJ/yr)"#,
            #"FE|Solar (EJ/yr)",
            #"FE|Other (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",gsub(".*\\|","",as.character(df$variable)), " final energy consumption in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total final energy in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Total Final Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Total Final Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Total Final Energy') <- names(regionsList)
  }  
  
  out$legend$'Total Final Energy'$description <- "<p>Final energy per carrier</p>"
  out$legend$'Total Final Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Total Final Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Total Final Energy'$contents <- c(list("Total secondary energy" =list("fill"="#000","linetype"="dashed")),out$legend$'Total Final Energy'$contents)
  out$legend$'Total Final Energy'$units <- "EJ per year"
  
  #price
  vars <- c("Price|Final Energy|Buildings (US$2005/GJ)",
            #"Price|Final Energy|Transport (US$2005/GJ)",
            "Price|Final Energy|Industry (US$2005/GJ)"
            )
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2), " US$<sub>2005</sub>/GJ<br>", ifelse(reg=="GLO","World", as.character(df$region)), " final energy price for ",gsub("Price\\|Final Energy\\|","",df$variable), "<br>", "year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=~variable,text=~details, group = ~variable)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, ncol=3, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Final Energy Prices' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Final Energy Prices' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      if(length(ggplotly$x$layout$annotations)>4)
        for (i in 4:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Final Energy Prices') <- names(regionsList)
  }   
  
  out$legend$'Final Energy Prices'$description <- "<p>Final energy prices</p>"
  out$legend$'Final Energy Prices'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"="solid")) })
  names(out$legend$'Final Energy Prices'$contents) <- gsub(".*\\|","",gsub(" \\(.*","",vars))
  out$legend$'Final Energy Prices'$units <- "US$<sub>2005</sub>/GJ per year"
  
  ### FE Transport
  
  #var.tot <-"FE|Transport (EJ/yr)"
  vars <- c("Electricity"    = "FE|Transport|Electricity (EJ/yr)",
            "Liquids - Oil"  = "FE|Transport|Liquids|Oil (EJ/yr)",
            "Liquids - Coal" = "FE|Transport|Liquids|Coal (EJ/yr)",
            "Liquids - Biomass" = "FE|Transport|Liquids|Biomass (EJ/yr)",
            #"FE|Transport|Gases (EJ/yr)",
            "Hydrogen"       = "FE|Transport|Hydrogen (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " use by transport in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total final energy use by transportat in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Transport Final Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Transport Final Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Transport Final Energy') <- names(regionsList)
  }   
  
  out$legend$'Transport Final Energy'$description <- "<p>Transport final energy use per carrier</p>"
  out$legend$'Transport Final Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Transport Final Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Transport Final Energy'$contents <- c(list("Total final energy use by transport" =list("fill"="#000","linetype"="dashed")),out$legend$'Transport Final Energy'$contents)
  out$legend$'Transport Final Energy'$units <- "EJ per year" 
  
  #price
  vars <- c("Price|Final Energy|Electricity|Transport (US$2005/GJ)",
            "Price|Final Energy|Liquids|Transport (US$2005/GJ)",
            "Price|Final Energy|Hydrogen|Transport (US$2005/GJ)")
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2), " US$<sub>2005</sub>/GJ<br>", ifelse(reg=="GLO","World", as.character(df$region)), " ",as.character(variable_label[as.character(df$variable)]), " final energy price for transport<br>", "year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=~variable,text=~details, group = ~variable)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, ncol=3, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Transport Final Energy Prices' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Transport Final Energy Prices' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      if(length(ggplotly$x$layout$annotations)>4)
        for (i in 4:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Transport Final Energy Prices') <- names(regionsList)
  }     
    
  out$legend$'Transport Final Energy Prices'$description <- "<p>Transport final energy Prices prices</p>"
  out$legend$'Transport Final Energy Prices'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"="solid")) })
  names(out$legend$'Transport Final Energy Prices'$contents) <- as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE)))
  out$legend$'Transport Final Energy Prices'$units <- "US$<sub>2005</sub>/GJ per year"
  
  ### FE Industry
  #var.tot <-"FE|Industry (EJ/yr)"
  vars <- c("Electricity"      = "FE|Industry|Electricity (EJ/yr)",
            "Liquids"          = "FE|Industry|Liquids (EJ/yr)",
            "Gases"            = "FE|Industry|Gases (EJ/yr)",
            "Heat"             = "FE|Industry|Heat (EJ/yr)",
            "Solids - Coal"    = "FE|Industry|Solids|Coal (EJ/yr)",
            "Solids - Biomass" = "FE|Industry|Solids|Biomass (EJ/yr)",
            "Hydrogen"         = "FE|Industry|Hydrogen (EJ/yr)"#,
            #"FE|Industry|Other (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " use by industry in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total final energy use by industry in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Industry Final Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Industry Final Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Industry Final Energy') <- names(regionsList)
  }      
    
  out$legend$'Industry Final Energy'$description <- "<p>Industry final energy use per carrier</p>"
  out$legend$'Industry Final Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Industry Final Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Industry Final Energy'$contents <- c(list("Total final energy use by industry" =list("fill"="#000","linetype"="dashed")),out$legend$'Industry Final Energy'$contents)
  out$legend$'Industry Final Energy'$units <- "EJ per year"
  
  #price  
  vars <- c("Price|Final Energy|Electricity|Industry (US$2005/GJ)",
            "Price|Final Energy|Heating Oil|Industry (US$2005/GJ)",
            "Price|Final Energy|Gases|Industry (US$2005/GJ)",
            "Price|Final Energy|Heat|Industry (US$2005/GJ)",
            "Price|Final Energy|Solids|Industry (US$2005/GJ)",
            "Price|Final Energy|Hydrogen|Industry (US$2005/GJ)")
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2), " US$<sub>2005</sub>/GJ<br>", ifelse(reg=="GLO","World", as.character(df$region)), " ",as.character(variable_label[as.character(df$variable)]), " final energy price for industry<br>", "year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=~variable,text=~details, group = ~variable)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color,alpha=aestethics$alpha) + # vertical line at initial year
      facet_wrap(~region, ncol=3, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Industry Final Energy Prices' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Industry Final Energy Prices' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      if(length(ggplotly$x$layout$annotations)>4)
        for (i in 4:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Industry Final Energy Prices') <- names(regionsList)
  }  

  out$legend$'Industry Final Energy Prices'$description <- "<p>Industry final energy Prices prices</p>"
  out$legend$'Industry Final Energy Prices'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"="solid")) })
  names(out$legend$'Industry Final Energy Prices'$contents) <- as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE)))
  out$legend$'Industry Final Energy Prices'$units <- "US$<sub>2005</sub>/GJ per year"
  
  ### FE Buildings
  #var.tot <-"FE|Buildings (EJ/yr)"
  vars <- c("Electricity"      = "FE|Buildings|Electricity (EJ/yr)",
            "Liquids"          = "FE|Buildings|Liquids (EJ/yr)",
            "Gases"            = "FE|Buildings|Gases (EJ/yr)",
            "Heat"             = "FE|Buildings|Heat (EJ/yr)",
            "Solids - Coal"    = "FE|Buildings|Solids|Coal (EJ/yr)",
            "Solids - Biomass" = "FE|Buildings|Solids|Biomass (EJ/yr)",
            "Hydrogen"         = "FE|Buildings|Hydrogen (EJ/yr)"#,
            #"FE|Buildings|Other (EJ/yr)"
  )
  
  color <- plotstyle(as.character(gsub("\\+\\|","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2)," EJ<br>",names(vars[df$variable]), " use by buildings in ", ifelse(reg=="GLO","the World", as.character(df$region)),"<br>year: ",df$period)
    g <- suppressWarnings( ggplot(data=df,aes_(x=~period,y=~value,fill=~variable,text=~details,group = ~variable)) +
                             geom_area(alpha=aestethics$alpha) +
                             geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
                             geom_line(data=df %>%
                                         group_by_(~region,~period) %>%
                                         summarise_(value = ~sum(value,na.rm=TRUE)) %>%
                                         mutate_(position= ~ value+max(value)/1000) %>%
                                         mutate_(details= ~ paste0(round(value,2)," EJ<br>","Total final energy use by buildings in ", ifelse(reg=="GLO","the World", as.character(region)),"<br>year: ",period)) %>%
                                         ungroup(),
                                       aes_(~period,~position,text=~details,group = ~region),color="#000000",size=aestethics$line$size,inherit.aes = FALSE, linetype="dashed",alpha=aestethics$alpha) +
                             facet_wrap(~region, scales="fixed") +
                             theme_minimal() +
                             labs(x = NULL, y = NULL) +
                             scale_fill_manual(values = color) )
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Buildings Final Energy' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Buildings Final Energy' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      for (i in 1:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
        if(ggplotly$x$layout$annotations[[i]]$y < 0.9)
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Buildings Final Energy') <- names(regionsList)
  }  
  
  out$legend$'Buildings Final Energy'$description <- "<p>Buildings final energy use per carrier</p>"
  out$legend$'Buildings Final Energy'$contents <- lapply(names(gsub(" \\(.*","",vars)), function(var) { return(list("fill"=color[gsub(" \\(.*","",vars[var])],"linetype"=NULL)) })
  names(out$legend$'Buildings Final Energy'$contents) <- names(gsub(" \\(.*","",vars)) 
  out$legend$'Buildings Final Energy'$contents <- c(list("Total final energy use by buildings" =list("fill"="#000","linetype"="dashed")),out$legend$'Buildings Final Energy'$contents)
  out$legend$'Buildings Final Energy'$units <- "EJ per year"
  
  #price
  vars <- c("Price|Final Energy|Electricity|Buildings (US$2005/GJ)",
            "Price|Final Energy|Heating Oil|Buildings (US$2005/GJ)",
            "Price|Final Energy|Gases|Buildings (US$2005/GJ)",
            "Price|Final Energy|Heat|Buildings (US$2005/GJ)",
            "Price|Final Energy|Solids|Buildings (US$2005/GJ)",
            "Price|Final Energy|Hydrogen|Buildings (US$2005/GJ)"
            )
  
  color <- plotstyle(as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE))),unknown=missingColorsdf)
  names(color) <- gsub(" \\(.*","",vars)
  
  g <- lapply(regionsList, function(reg) {
    df <- as.quitte(data[reg,,vars])  
    df$details <- paste0(round(df$value,2), " US$<sub>2005</sub>/GJ<br>", ifelse(reg=="GLO","World", as.character(df$region)), " ",as.character(variable_label[as.character(df$variable)]), " final energy price for buildings<br>", "year: ",df$period)  
    g <- ggplot(data=df,aes_(x=~period,y=~value,color=~variable,text=~details, group = ~variable)) + 
      geom_line(size=aestethics$line$size,alpha=aestethics$alpha) + # line plot
      geom_vline(xintercept=as.numeric(min(df$period)),linetype=2, size=aestethics$`y-axis`$size, color=aestethics$`y-axis`$color) + # vertical line at initial year
      facet_wrap(~region, ncol=3, scales="fixed") +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color) 
    if (length(reg) == 1)
      g <- g + theme(strip.text.x = element_blank())
    else 
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(g)
  })
  
  if ("ggplot" %in% chartType){
    out$ggplot$FE$'Buildings Final Energy Prices' <- g
  }
  
  if ("plotly" %in% chartType){
    out$plotly$FE$'Buildings Final Energy Prices' <- lapply(names(regionsList), function(reg) {
      ggplotly <- ggplotly(g[[reg]], tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
      if(length(ggplotly$x$layout$annotations)>4)
        for (i in 4:length(ggplotly$x$layout$annotations)) #displacing facet titles a little down
          ggplotly$x$layout$annotations[[i]]$y <- ggplotly$x$layout$annotations[[i]]$y - 0.04
      return(ggplotly)
    })
    names(out$plotly$FE$'Buildings Final Energy Prices') <- names(regionsList)
  }  
  
  out$legend$'Buildings Final Energy Prices'$description <- "<p>Buildings final energy prices</p>"
  out$legend$'Buildings Final Energy Prices'$contents <- lapply(gsub(" \\(.*","",vars), function(var) { return(list("fill"=color[var],"linetype"="solid")) })
  names(out$legend$'Buildings Final Energy Prices'$contents) <- as.character(gsub(" \\(.*","",shorten_legend(vars,identical_only=TRUE)))
  out$legend$'Buildings Final Energy Prices'$units <- "US$<sub>2005</sub>/GJ per year"
  
  return(out)
}
