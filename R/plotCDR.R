
#' Read in GDX and write *.mif reporting
#' 
#' Read in all information from GDX file and create
#' the *.mif reporting
#' 
#' 
#' @param mif a path to one or more mif-files (might be created by confGDX2MIF)
#' @param hist a path to one mif-file containing historical data
#' @param y time span for the data in line plots, default: y=c(seq(2005,2060,5),seq(2070,2100,10)) 
#' @param y_hist time span for the historical data in the line plots, default: c(seq(1960,2014,1))
#' @param y_bar time slides for bar plots, default: y_bar=c(2010,2030,2050,2100)
#' @param reg region(s) in focus, reg ="all_regi" shows all regions if the mifs contain different regions
#' 
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{compareScenarios(mif_path)}
#'
#'
#' @export
#' @importFrom magclass read.report mbind getRegions new.magpie getYears add_dimension setNames getNames<-
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom mip mipLineHistorical mipBarYearData
#' @importFrom luplot magpie2ggplot2
#' @importFrom mip plotstyle

plotCDR <- function(mif,hist,y=c(seq(2005,2060,5),seq(2070,2100,10)),y_hist=c(seq(1960,2015,1)),y_bar=c(2010,2030,2050,2100),reg=NULL) {

############### read data ####################################################
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
  if (!reg=="all_reg") {
     data <- data[reg,y,]
  } else {
     data <- data[,y,]
  }  
} else {
  data <- data[,y,]
}
# delete "+" and "++" from variable names
data <- deletePlus(data)

# read historical data
hist <- read.report(hist,as.list=FALSE)
if(all(getRegions(data) %in% getRegions(hist))) {
  hist <- hist[getRegions(data),y_hist,]
} else {
  if(reg=="all_reg"){
    # fill up historical data for additional regions with 0
    dataReg    <- getRegions(data)[-which(getRegions(data) %in% getRegions(hist))]
    dummy_hist <- new.magpie(dataReg,getYears(hist),getNames(hist),fill=NA)
    hist       <- mbind(hist,dummy_hist)
    hist       <- hist[getRegions(data),y_hist,]
  } else {
    stop("historical data do not contain the choosen region")
  }
}

###############################################################################

################## open output-pdf ############################################
sw <- swopen("plotCDR.pdf")
swlatex(sw,"\\tableofcontents")
###############################################################################

################## plot data ##################################################
# Emissions
swlatex(sw,"\\subsection{Emissions}")
swfigure(sw,mipLineHistorical,data[,,"Emi|CO2 (Mt CO2/yr)"],x_hist=NULL,
         ylab='CO2 total [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|Gross Fossil Fuels and Industry (Mt CO2/yr)"],x_hist=NULL,
         ylab='Emi|CO2|Gross Fossil Fuels and Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"],x_hist=NULL,
         ylab='Emi|CO2|Fossil Fuels and Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|Land-Use Change (Mt CO2/yr)"],x_hist=NULL,
         ylab='Emi|CO2|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|Carbon Capture and Storage|Fossil|Pe2Se (Mt CO2/yr)"],x_hist=NULL,
         ylab='Emi|CO2|CCS|Fossil [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|Carbon Capture and Storage (Mt CO2/yr)"],x_hist=NULL,
         ylab='Emi|CO2|CCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|Cumulated (Mt CO2/yr)"],x_hist=NULL,
         ylab='CO2 total cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

# CDR emissions
swlatex(sw,"\\subsubsection{CDR Emissions}")
swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|Land-Use Change (Mt CO2/yr)"],x_hist=NULL,
                              ylab='Afforestation [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|BECCS (Mt CO2/yr)"],x_hist=NULL,
         ylab='BECCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")


if("Emi|CO2|CDR|EW (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|EW (Mt CO2/yr)"],x_hist=NULL,
           ylab='EW [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("Emi|CO2|CDR|DAC (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|DAC (Mt CO2/yr)"],x_hist=NULL,
           ylab='DAC [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR (Mt CO2/yr)"],x_hist=NULL,
         ylab='CDR total [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

if("Emi|CO2|CDR|Land-Use Change|Cumulated (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|Land-Use Change|Cumulated (Mt CO2/yr)"],x_hist=NULL,
           ylab='CDR Land-Use Change Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("Emi|CO2|CDR|BECCS|Cumulated (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|BECCS|Cumulated (Mt CO2/yr)"],x_hist=NULL,
           ylab='BECCS Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("Emi|CO2|CDR|EW|Cumulated (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|EW|Cumulated (Mt CO2/yr)"],x_hist=NULL,
           ylab='EW Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("Emi|CO2|CDR|DAC|Cumulated (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|DAC|Cumulated (Mt CO2/yr)"],x_hist=NULL,
           ylab='DAC Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("Emi|CO2|CDR|Cumulated (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"Emi|CO2|CDR|Cumulated (Mt CO2/yr)"],x_hist=NULL,
           ylab='CDR total Cumulated [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

# Emissions per sector
if("Emi|CO2|Carbon Capture and Storage|Fossil|Cumulated (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
var <- NULL
var <- mbind(var,data[,y_bar,"Emi|CO2|Gross Fossil Fuels and Industry|Cumulated (Mt CO2/yr)"])
var <- mbind(var,data[,y_bar,"Emi|CO2|Carbon Capture and Storage|Fossil|Cumulated (Mt CO2/yr)"])
var <- mbind(var,data[,y_bar,"Emi|CO2|CDR|Land-Use Change|Cumulated (Mt CO2/yr)"])
var <- mbind(var,data[,y_bar,"Emi|CO2|CDR|BECCS|Cumulated (Mt CO2/yr)"])
var <- mbind(var,data[,y_bar,"Emi|CO2|CDR|DAC|Cumulated (Mt CO2/yr)"])
var <- mbind(var,data[,y_bar,"Emi|CO2|CDR|EW|Cumulated (Mt CO2/yr)"])
var <- setNames(var,gsub("Emi\\|","",magclass::getNames(var)))
var <- setNames(var,gsub(" \\(.*\\)","",magclass::getNames(var)))
p <- mipBarYearData(var,ylab="Emi (Mt CO2eq/yr)",colour=plotstyle(magclass::getNames(var,dim=3)))
print(p)
swfigure(sw,print,p,sw_option="height=10,width=9")
}

#Prices
swlatex(sw,"\\subsection{Prices}")
swfigure(sw,mipLineHistorical,data[,,"Price|Carbon (US$2005/t CO2)"],x_hist=NULL,
                              ylab='Price|Carbon [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

if("GLO" %in% getRegions(data)) {
swfigure(sw,mipLineHistorical,data["GLO",,"Price|Carbon (US$2005/t CO2)"],x_hist=NULL,
                              ylab='Price|Carbon_log [US$2005/t CO2]',ybreaks=c(20,30,40,50,60,75,100,200,500,1000,2000,3000), 
                              ylim=c(20,3000),ylog=TRUE,sw_option="height=10,width=9")
}

swfigure(sw,mipLineHistorical,data[,,"Price|Biomass|Primary Level (US$2005/GJ)"],x_hist=NULL,
         ylab='Price|Biomass|Primary Level [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
swfigure(sw,mipLineHistorical,data[,,"Price|Biomass|MAgPIE (US$2005/GJ)"],x_hist=NULL,
         ylab='Price|Biomass|MAgPIE [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

# Energy
swlatex(sw,"\\subsection{Energy}")

swlatex(sw,"\\subsubsection{Primary Energy Mixes}")
items <-c("PE|Coal (EJ/yr)",
          "PE|Oil (EJ/yr)",
          "PE|Gas (EJ/yr)",
          "PE|Biomass (EJ/yr)",
          "PE|Nuclear (EJ/yr)",
          "PE|Solar (EJ/yr)",
          "PE|Wind (EJ/yr)",
          "PE|Hydro (EJ/yr)",
          "PE|Geothermal (EJ/yr)")
PE <- data[,,items]
magclass::getNames(PE) <- gsub("PE\\|","",magclass::getNames(PE))
magclass::getNames(PE) <- gsub(" \\(EJ/yr\\)","",magclass::getNames(PE))
PE <- PE[,y_bar,]
p <- mipBarYearData(PE,ylab="PE (EJ/yr)",colour=plotstyle(magclass::getNames(PE,dim=3))) 
print(p)
swfigure(sw,print,p,sw_option="height=10,width=9")

# PE
swlatex(sw,"\\subsubsection{Primary Energy line plots}")
swfigure(sw,mipLineHistorical,data[,,"PE|Coal (EJ/yr)"],x_hist=hist[,,"PE|Coal (EJ/yr)"],
        ylab='PE|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"PE|Gas (EJ/yr)"],x_hist=hist[,,"PE|Gas (EJ/yr)"],
         ylab='PE|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"PE|Oil (EJ/yr)"],x_hist=hist[,,"PE|Oil (EJ/yr)"],
         ylab='PE|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"PE|Biomass (EJ/yr)"],x_hist=hist[,,"PE|Biomass (EJ/yr)"],
         ylab='PE|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"PE|Biomass|Modern (EJ/yr)"],x_hist=hist[,,"PE|Biomass|Modern (EJ/yr)"],
         ylab='PE|Biomass|Modern [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

#FE
swlatex(sw,"\\subsubsection{Final Energy line plots}")
swfigure(sw,mipLineHistorical,data[,,"FE|Electricity (EJ/yr)"],x_hist=hist[,,"FE|Electricity (EJ/yr)"],
         ylab='FE|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

if("FE|CDR|Electricity (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|Electricity (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("FE|CDR|DAC|Electricity (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|DAC|Electricity (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|DAC|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("FE|CDR|EW|Electricity (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|EW|Electricity (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|EW|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

swfigure(sw,mipLineHistorical,data[,,"FE|Liquids (EJ/yr)"],x_hist=hist[,,"FE|Liquids (EJ/yr)"],
         ylab='FE|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

if("FE|CDR|Liquids (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|Liquids (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("FE|CDR|EW|Liquids (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|EW|Liquids (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|EW|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

swfigure(sw,mipLineHistorical,data[,,"FE|Hydrogen (EJ/yr)"],x_hist=hist[,,"FE|Hydrogen (EJ/yr)"],
         ylab='FE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"FE|Heat (EJ/yr)"],x_hist=hist[,,"FE|Heat (EJ/yr)"],
         ylab='FE|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

if("FE|CDR|DAC|Heat (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|DAC|Heat (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|DAC|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("FE|CDR|DAC|Heat|H2 (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|DAC|Heat|H2 (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|DAC|Heat|H2 [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}

if("FE|CDR|DAC|Heat|Gas (EJ/yr)" %in% magclass::getNames(data,dim=3)) {
  swfigure(sw,mipLineHistorical,data[,,"FE|CDR|DAC|Heat|Gas (EJ/yr)"],x_hist=NULL,
           ylab='FE|CDR|DAC|Heat|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
}


#Climate
swlatex(sw,"\\subsubsection{Climate}")
swfigure(sw,mipLineHistorical,data[,,"Forcing (W/m2)"],x_hist=NULL,
         ylab='Forcing [W/m2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

swfigure(sw,mipLineHistorical,data[,,"Temperature|Global Mean (K)"],x_hist=NULL,
         ylab='Temperature|Global Mean [K]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")

###############################################################################

################## close output-pdf ###########################################
swclose(sw)
###############################################################################

}  
  

