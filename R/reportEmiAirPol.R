#' Read in GDX and calculate air pollution emissions, used in convGDX2MIF.R for 
#' the reporting
#' 
#' Read in air pollution emission information from GDX file, information used in 
#' convGDX2MIF.R for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @return MAgPIE object - contains the emission variables
#' @author Antoine Levesque, Jerome Hilaire
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{reportEmiAirPol(gdx)}
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass collapseNames dimSums getNames<- mbind setNames new.magpie getRegions getYears mbind dimSums setYears getRegions<-
reportEmiAirPol <- function(gdx,regionSubsetList=NULL){

  # Get realisation name 
  realisation <- readGDX(gdx, "module2realisation")
  realisation <- realisation[which(realisation[,1] == "aerosols"),2]
  if(is.null(realisation)) realisation <- "postIIASA" # default value 
  
  ######### initialisation  ###########
  tmp <- NULL
  out <- NULL
  
  #===================================================================
  # If realisation is postIIASA...
  #===================================================================
  if (realisation == "postIIASA") {

  ######### initialisation  ###########
  airpollutants <- c("SO2","BC","OC","CO","VOC","NOx","NH3")
  airpollutants_low <- c("so2","bc","oc","CO","VOC","NOx","NH3")

  generateReportingEmiAP <- function(pollutant){
    
    pollutant_low <- tolower(pollutant)

    # Remove unecessary technological dimensions
    dataEndo <- collapseNames(dimSums(p11_emi_postrun[,,pollutant_low], dim=c("all_enty","all_enty1","all_te"),na.rm=TRUE))
    dataExog <- collapseNames(pm_emiAP[,,pollutant_low])

    # Replace REMIND sector names by reporting ones

    mapping = data.frame(
      remind = c("power", "indst", "res", "trans", "indprocess", "solvents", "extraction"),
      reporting = c(paste0("Emi|", pollutant, "|Energy Supply|Electricity (Mt ", pollutant, "/yr)"),
                    paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)"), 
                    paste0("Emi|", pollutant, "|Energy Demand|ResCom (Mt ", pollutant, "/yr)"),
                    paste0("Emi|", pollutant, "|Energy Demand|Transport|Ground Trans (Mt ", pollutant, "/yr)"),
                    paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)"), 
                    paste0("Emi|", pollutant, "|Solvents (Mt ", pollutant, "/yr)"), 
                    paste0("Emi|", pollutant, "|Energy Supply|Extraction (Mt ", pollutant, "/yr)")))

    magclass::getNames(dataEndo) <- paste(sapply(magclass::getNames(dataEndo), function(x) mapping$reporting[mapping$remind == x]))
    magclass::getNames(dataExog) <- paste(sapply(magclass::getNames(dataExog), function(x) mapping$reporting[mapping$remind == x]))

    dataAll <- mbind(dataEndo[,,paste(mapping$reporting[which(!mapping$remind %in% c("indst","indprocess", "solvents","extraction"))])],
                     dataExog[,,paste(mapping$reporting[which(!mapping$remind %in% c("power","indst","res","trans","indprocess"))])],
                     dataEndo[,,paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)")] + 
                     dataExog[,,paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)")])

    if (is.null(pm_limits_wp4_rcp_world)) {
      tmp1 <-
        mbind(dataAll,
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"international shipping",sep= ".")],
                     paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"aviation",sep= ".")],
                     paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"ag waste burning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Agricultural Waste Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"agriculture",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Agriculture (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"forest burning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Forest Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"grassland burning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Savannah Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"waste",sep= ".")],
                     paste0("Emi|",pollutant,"|Waste (Mt ",pollutant,"/yr)")))
    } else {
      tmp1 <- 
        mbind(dataAll,
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"internationalshipping",sep= ".")],
                     paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"aviation",sep= ".")],
                     paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"agwasteburning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Agricultural Waste Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"agriculture",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Agriculture (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"forestburning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Forest Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"grasslandburning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Savannah Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"waste",sep= ".")],
                     paste0("Emi|",pollutant,"|Waste (Mt ",pollutant,"/yr)")))
    }

    # Aggregation: Transport and Energy Supply
    tmp2 <- mbind(tmp1,
                  setNames(dimSums(tmp1[,,
                                        c(paste0("Emi|",pollutant,"|Energy Demand|Transport|Ground Trans (Mt ",pollutant,"/yr)"),
                                          paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)"),
                                          paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)")),
                  setNames(dimSums(tmp1[,,
                                        c(paste0("Emi|",pollutant,"|Energy Supply|Electricity (Mt ",pollutant,"/yr)"),
                                          paste0("Emi|",pollutant,"|Energy Supply|Extraction (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Energy Supply (Mt ",pollutant,"/yr)")))

    # Aggregation: Energy Demand + Energy Supply, Land Use
    tmp3 <- mbind(tmp2, 
                  setNames(dimSums(tmp2[,,c(paste0("Emi|",pollutant,"|Energy Demand|Industry (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Energy Demand|ResCom (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Energy Supply (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)")),
                  setNames(dimSums(tmp2[,,c(paste0("Emi|",pollutant,"|Land Use|Savannah Burning (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use|Forest Burning (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use|Agriculture (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use|Agricultural Waste Burning (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Land Use (Mt ",pollutant,"/yr)")))

    # Compute total
    tmp4 <- mbind(tmp3,
                  setNames(dimSums(tmp3[,,c(paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Solvents (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Waste (Mt ",pollutant,"/yr)"))],dim=3),
                           paste0("Emi|",pollutant," (Mt ",pollutant,"/yr)")))


    # Set NAs to 0
    tmp4[is.na(tmp4)] <- 0

    return(tmp4)
  }

  ####### conversion factors ##########
  pm_conv_TWa_EJ    <- 31.536
  conv_MtSO2_to_MtS <- 1/2     # 32/(32+2*16)
    
  ####### read in needed data #########

  ## parameter
  p11_emi_postrun      <- readGDX(gdx, name=c("pm_emi_postrun","p11_emi_postrun"), field="l", format="first_found",restore_zeros = FALSE)
  pm_limits_wp4_rcp    <- readGDX(gdx, name=c("pm_emiAPexo","pm_limits_wp4_rcp"), field="l", format="first_found")[,,airpollutants_low]
  pm_limits_wp4_rcp_world    <- readGDX(gdx, name=c("pm_emiAPexoGlob","pm_limits_wp4_rcp_world"), field="l", format="first_found")[,,airpollutants_low]
  pm_emiAP             <- readGDX(gdx, name="pm_emiAP", field="l", format="first_found")
  p11_emi_nh3_agwaste  <- readGDX(gdx, name="p11_emi_nh3_agwaste", field="l")
  p11_emi_nh3_ag       <- readGDX(gdx, name="p11_emi_nh3_ag", field="l")
  p11_emi_nh3_forest   <- readGDX(gdx, name="p11_emi_nh3_forest", field="l")
  p11_emi_nh3_savannah <- readGDX(gdx, name="p11_emi_nh3_savannah", field="l")

  
  if (!is.null(pm_emiAP)) {

  ####### process data #########
  magclass::getNames(p11_emi_postrun)   <- tolower(magclass::getNames(p11_emi_postrun))
  magclass::getNames(pm_emiAP)          <- tolower(magclass::getNames(pm_emiAP))

  if (is.null(pm_limits_wp4_rcp_world)) {
    pm_limits_wp4_rcp <- do.call("mbind",
                                 lapply(setdiff(magclass::getNames(pm_limits_wp4_rcp,dim = 1),"WORLD"),
                                        function(reg){
                                          mreg <- collapseNames(pm_limits_wp4_rcp[,,reg])
                                          getRegions(mreg) <- reg
                                          return(mreg)
                                        }))

    magclass::getNames(pm_limits_wp4_rcp) <- tolower(magclass::getNames(pm_limits_wp4_rcp))
  } else {
    magclass::getNames(pm_limits_wp4_rcp)       <- tolower(magclass::getNames(pm_limits_wp4_rcp))
    magclass::getNames(pm_limits_wp4_rcp_world) <- tolower(magclass::getNames(pm_limits_wp4_rcp_world))
  }


  p11_emi_postrun[,,"so2"]   <- 1/conv_MtSO2_to_MtS * p11_emi_postrun[,,"so2"]
  pm_limits_wp4_rcp[,,"so2"] <- 1/conv_MtSO2_to_MtS * pm_limits_wp4_rcp[,,"so2"]
  pm_emiAP[,,"so2"]          <- 1/conv_MtSO2_to_MtS * pm_emiAP[,,"so2"]


  if (is.null(pm_limits_wp4_rcp_world)) {
    pm_limits_wp4_rcp_world[,,"so2"] <- 1/conv_MtSO2_to_MtS * pm_limits_wp4_rcp_world[,,"so2"]
  }

  if (is.null(pm_limits_wp4_rcp_world)) {
  # Merge NH3 data with other air pollutants data
    magclass::getNames(p11_emi_nh3_agwaste)  <- "nh3.ag waste burning"
    magclass::getNames(p11_emi_nh3_ag)       <- "nh3.agriculture"
    magclass::getNames(p11_emi_nh3_forest)   <- "nh3.forest burning"
    magclass::getNames(p11_emi_nh3_savannah) <- "nh3.grassland burning"
    p_emi_nh3_others = new.magpie(cells_and_regions = getRegions(p11_emi_nh3_savannah), years = getYears(p11_emi_nh3_savannah), names = c("nh3.waste", "nh3.international shipping", "nh3.aviation"), fill=0.0)
    p_emi_nh3 = mbind(p11_emi_nh3_agwaste, p11_emi_nh3_ag, p11_emi_nh3_forest, p11_emi_nh3_savannah, p_emi_nh3_others)
  } else {
    magclass::getNames(p11_emi_nh3_agwaste)  <- "nh3.agwasteburning"
    magclass::getNames(p11_emi_nh3_ag)       <- "nh3.agriculture"
    magclass::getNames(p11_emi_nh3_forest)   <- "nh3.forestburning"
    magclass::getNames(p11_emi_nh3_savannah) <- "nh3.grasslandburning"
    p_emi_nh3_others = new.magpie(cells_and_regions = getRegions(p11_emi_nh3_savannah), years = getYears(p11_emi_nh3_savannah), names = c("nh3.waste", "nh3.internationalshipping", "nh3.aviation"), fill=0.0)
    p_emi_nh3 = mbind(p11_emi_nh3_agwaste, p11_emi_nh3_ag, p11_emi_nh3_forest, p11_emi_nh3_savannah, p_emi_nh3_others)
  }
  ####### calculate minimal temporal and spatial resolutions #####
  getRegions(pm_limits_wp4_rcp) <- toupper(getRegions(pm_limits_wp4_rcp))
  y <- Reduce(intersect,list(getYears(p11_emi_postrun),  getYears(pm_limits_wp4_rcp),   getYears(pm_emiAP)))
  r <- Reduce(intersect,list(getRegions(p11_emi_postrun),getRegions(pm_limits_wp4_rcp), getRegions(pm_emiAP)))
  p11_emi_postrun    <- p11_emi_postrun[r,y,]
  pm_limits_wp4_rcp  <- pm_limits_wp4_rcp[r,y,]
  if (!is.null(pm_limits_wp4_rcp_world)) {
    pm_limits_wp4_rcp_world  <- pm_limits_wp4_rcp_world[,y,]
  }
  pm_emiAP           <- pm_emiAP[r,y,]
  p_emi_nh3          <- p_emi_nh3[r,y,]
  pm_limits_wp4_rcp  <- pm_limits_wp4_rcp[,,magclass::getNames(p_emi_nh3),invert=TRUE]
  pm_limits_wp4_rcp = mbind(pm_limits_wp4_rcp, p_emi_nh3)


  ####### calculate reporting parameters ############
  # Loop over air pollutants and call reporting generating function
  out <- do.call("mbind", lapply(airpollutants, generateReportingEmiAP))

  # Add global values
  out              <- mbind(out, dimSums(out,dim=1))

  # Add Aviation and Int. Shipping emissions
  if (!is.null(pm_limits_wp4_rcp_world)) {
    for (pollutant in airpollutants) {
      pollutant_low <- tolower(pollutant)

      out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] <- pm_limits_wp4_rcp_world["GLO",,paste(pollutant_low,"internationalshipping",sep= ".")]
      out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]               <- pm_limits_wp4_rcp_world["GLO",,paste(pollutant_low,"aviation",sep= ".")]

      out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)")] <- out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]

      out["GLO",,paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)")] <- out["GLO",,paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)")]  +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]

      out["GLO",,paste0("Emi|",pollutant," (Mt ",pollutant,"/yr)")] <- out["GLO",,paste0("Emi|",pollutant," (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]
    }
  }


  }

  #===================================================================
  # if realisation is postECLIPSE
  #===================================================================
  } else if (realisation == "postECLIPSE") {

  # DEBUG
  #gdx        = "path_to_gdx_directory/fulldata.gdx"
  #mainfolder = "path_to_rd3_inputdata/"
  
  #require(moinput,quietly = TRUE)
  #setConfig(mainfolder = mainfolder)
  
  ######### initialisation  ###########
  airpollutants <- c("SO2","BC","OC","CO","VOC","NOx","NH3")
  
  generateReportingEmiAP <- function(pollutant){
    
    pollutant_low <- tolower(pollutant)
    
    # Remove unecessary technological dimensions
    dataEndo <- collapseNames(dimSums(p11_emi_postrun[,,pollutant_low], dim=c("all_enty","all_enty1","all_te"),na.rm=TRUE))

    if (!is.null(p11_emiAP_endu) & pollutant_low != "nh3") { 
      dataEndo <- dataEndo[,,"indst", pmatch=TRUE, invert=TRUE]
      dataEndo <- dataEndo[,,"res", pmatch=TRUE, invert=TRUE]

      dataEndo <- mbind(dataEndo, collapseNames(p11_emiAP_endu[,,pollutant_low])[,,"indst"])
      dataEndo <- mbind(dataEndo, collapseNames(p11_emiAP_endu[,,pollutant_low])[,,"res"])
    }

    dataExog <- collapseNames(pm_emiAP[,,pollutant_low])
    
    # Apply emission base year calibration (if required)
    if (!is.null(pm_emi_calib)) {
      if (pollutant_low %in% c("nox", "co", "voc")) {
        # Industry
        dataEndo[,,"indst"] = dataEndo[,,"indst"] * setYears(collapseNames(pm_emi_calib[,2005,paste0(pollutant_low, ".indst")],1) / (dataEndo[,2005,"indst"] + setNames(dataExog[,2005,"indprocess"], NULL)), NULL)
        dataExog[,,"indprocess"] = dataExog[,,"indprocess"] * setYears(collapseNames(pm_emi_calib[,2005,paste0(pollutant_low, ".indst")],1) / (dataEndo[,2005,"indst"] + setNames(dataExog[,2005,"indprocess"], NULL)), NULL)
        # Residential and commercial
        dataEndo[,,"res"]   = dataEndo[,,"res"]   * setYears(collapseNames(pm_emi_calib[,2005,paste0(pollutant_low, ".res")],1)   / (dataEndo[,2005,"res"]), NULL)
        # Power
        dataEndo[,,"power"] = dataEndo[,,"power"] * setYears(collapseNames(pm_emi_calib[,2005,paste0(pollutant_low, ".power")],1) / (dataEndo[,2005,"power"] + setNames(dataExog[,2005,"extraction"], NULL)), NULL)
        dataExog[,,"extraction"] = dataExog[,,"extraction"] * setYears(collapseNames(pm_emi_calib[,2005,paste0(pollutant_low, ".power")],1) / (dataEndo[,2005,"power"] + setNames(dataExog[,2005,"extraction"], NULL)), NULL)
        # Transport
        dataEndo[,,"trans"] = dataEndo[,,"trans"] * setYears(collapseNames(pm_emi_calib[,2005,paste0(pollutant_low, ".trans")],1) / (dataEndo[,2005,"trans"]), NULL)
      }
    }
    if (!is.null(pm_emi_calib)) {
      if (pollutant_low %in% c("so2", "bc", "oc")) {
        # Cross-sectoral scaling factor
        ship = pm_limits_wp4_rcp[,2005,paste0(pollutant_low,".internationalshipping")]
        air  = pm_limits_wp4_rcp[,2005,paste0(pollutant_low,".aviation")]
        scalfac <- setNames(setYears((dimSums(pm_emi_calib[,2005,pollutant_low]) + ship + air )/(dimSums(dataEndo[,2005,]) + dimSums(dataExog[,2005,]) + ship + air), NULL), NULL)
        # Multiply factor 
        dataEndo[,,] = dataEndo[,,] * scalfac
        dataExog[,,] = dataExog[,,] * scalfac
        pm_limits_wp4_rcp[,,paste0(pollutant_low,".internationalshipping")] = pm_limits_wp4_rcp[,,paste0(pollutant_low,".internationalshipping")] * scalfac
        pm_limits_wp4_rcp[,,paste0(pollutant_low,".aviation")] = pm_limits_wp4_rcp[,,paste0(pollutant_low,".aviation")] * scalfac
      }
    }

    # Replace REMIND sector names by reporting ones
    mapping = data.frame(
      remind = c("power", "indst", "res", "trans", "indprocess", "solvents", "extraction"),
      reporting = c(paste0("Emi|", pollutant, "|Energy Supply|Electricity (Mt ", pollutant, "/yr)"),
                    paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)"), 
                    paste0("Emi|", pollutant, "|Energy Demand|ResCom (Mt ", pollutant, "/yr)"),
                    paste0("Emi|", pollutant, "|Energy Demand|Transport|Ground Trans (Mt ", pollutant, "/yr)"),
                    paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)"), 
                    paste0("Emi|", pollutant, "|Solvents (Mt ", pollutant, "/yr)"), 
                    paste0("Emi|", pollutant, "|Energy Supply|Extraction (Mt ", pollutant, "/yr)")))
    
    magclass::getNames(dataEndo) <- paste(sapply(magclass::getNames(dataEndo), function(x) mapping$reporting[mapping$remind == x]))
    magclass::getNames(dataExog) <- paste(sapply(magclass::getNames(dataExog), function(x) mapping$reporting[mapping$remind == x]))
    
    dataAll <- mbind(dataEndo[,,paste(mapping$reporting[which(!mapping$remind %in% c("indst","indprocess", "solvents","extraction"))])],
                     dataExog[,,paste(mapping$reporting[which(!mapping$remind %in% c("power","indst","res","trans","indprocess"))])],
                     dataEndo[,,paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)")] + 
                     dataExog[,,paste0("Emi|", pollutant, "|Energy Demand|Industry (Mt ", pollutant, "/yr)")])
    
    tmp1 <- 
      mbind(dataAll,
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"internationalshipping",sep= ".")],
                     paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"aviation",sep= ".")],
                     paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"agwasteburning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Agricultural Waste Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"agriculture",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Agriculture (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"forestburning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Forest Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"grasslandburning",sep= ".")],
                     paste0("Emi|",pollutant,"|Land Use|Savannah Burning (Mt ",pollutant,"/yr)")),
            setNames(pm_limits_wp4_rcp[,,paste(pollutant_low,"waste",sep= ".")],
                     paste0("Emi|",pollutant,"|Waste (Mt ",pollutant,"/yr)")))
    
    # Aggregation: Transport and Energy Supply
    tmp2 <- mbind(tmp1,
                  setNames(dimSums(tmp1[,,
                                        c(paste0("Emi|",pollutant,"|Energy Demand|Transport|Ground Trans (Mt ",pollutant,"/yr)"),
                                          paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)"),
                                          paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)")),
                  setNames(dimSums(tmp1[,,
                                        c(paste0("Emi|",pollutant,"|Energy Supply|Electricity (Mt ",pollutant,"/yr)"),
                                          paste0("Emi|",pollutant,"|Energy Supply|Extraction (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Energy Supply (Mt ",pollutant,"/yr)"))
                  )
    
    # Aggregation: Energy Demand + Energy Supply, Land Use
    tmp3 <- mbind(tmp2, 
                  setNames(dimSums(tmp2[,,c(paste0("Emi|",pollutant,"|Energy Demand|Industry (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Energy Demand|ResCom (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Energy Supply (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)")),
                  setNames(dimSums(tmp2[,,c(paste0("Emi|",pollutant,"|Land Use|Savannah Burning (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use|Forest Burning (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use|Agriculture (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use|Agricultural Waste Burning (Mt ",pollutant,"/yr)"))],dim = 3),
                           paste0("Emi|",pollutant,"|Land Use (Mt ",pollutant,"/yr)")))
    
    # Compute total
    tmp4 <- mbind(tmp3,
                  setNames(dimSums(tmp3[,,c(paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Solvents (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Land Use (Mt ",pollutant,"/yr)"),
                                            paste0("Emi|",pollutant,"|Waste (Mt ",pollutant,"/yr)"))],dim=3),
                           paste0("Emi|",pollutant," (Mt ",pollutant,"/yr)")))
    
    # Set NAs to 0
    tmp4[is.na(tmp4)] <- 0
    
    return(tmp4)
  } 
 
  ####### conversion factors ##########
  pm_conv_TWa_EJ    <- 31.536
  conv_MtSO2_to_MtS <- 1/2     # 32/(32+2*16)
  
  
  ####### read in needed data #########
  ## parameter
  p11_emi_postrun         <- readGDX(gdx, name=c("pm_emi_postrun","p11_emi_postrun"), field="l", format="first_found",restore_zeros = FALSE)
  pm_limits_wp4_rcp       <- readGDX(gdx, name=c("pm_emiAPexo","pm_limits_wp4_rcp"), field="l", format="first_found")
  pm_limits_wp4_rcp_world <- readGDX(gdx, name=c("pm_emiAPexoGlob","pm_limits_wp4_rcp_world"), field="l", format="first_found")
  pm_emiAP                <- readGDX(gdx, name="pm_emiAP", field="l", format="first_found")
  p11_emiAP_endu          <- readGDX(gdx, name=c("pm_emiAP_endu","p11_emiAP_endu"), format="first_found")
  p11_emi_nh3_agwaste     <- readGDX(gdx, name="p11_emi_nh3_agwaste", field="l")
  p11_emi_nh3_ag          <- readGDX(gdx, name="p11_emi_nh3_ag", field="l")
  p11_emi_nh3_forest      <- readGDX(gdx, name="p11_emi_nh3_forest", field="l")
  p11_emi_nh3_savannah    <- readGDX(gdx, name="p11_emi_nh3_savannah", field="l")
  pm_emi_calib            <- readGDX(gdx, name=c("pm_APemi_calib","pm_emi_calib_ceds"), format="first_found")
 
  if (!is.null(pm_emiAP)) {
  ####### process data #########
  magclass::getNames(p11_emi_postrun)         <- tolower(magclass::getNames(p11_emi_postrun))
  magclass::getNames(pm_limits_wp4_rcp)       <- tolower(magclass::getNames(pm_limits_wp4_rcp))
  magclass::getNames(pm_limits_wp4_rcp_world) <- tolower(magclass::getNames(pm_limits_wp4_rcp_world))
  magclass::getNames(pm_emiAP)                <- tolower(magclass::getNames(pm_emiAP))
  magclass::getNames(p11_emiAP_endu)          <- tolower(magclass::getNames(p11_emiAP_endu))
  magclass::getNames(pm_emi_calib)            <- tolower(magclass::getNames(pm_emi_calib))  

  p11_emi_postrun[,,"so2"]         <- 1/conv_MtSO2_to_MtS * p11_emi_postrun[,,"so2"]
  pm_limits_wp4_rcp[,,"so2"]       <- 1/conv_MtSO2_to_MtS * pm_limits_wp4_rcp[,,"so2"]
  pm_limits_wp4_rcp_world[,,"so2"] <- 1/conv_MtSO2_to_MtS * pm_limits_wp4_rcp_world[,,"so2"]
  pm_emiAP[,,"so2"]                <- 1/conv_MtSO2_to_MtS * pm_emiAP[,,"so2"]
  p11_emiAP_endu[,,"so2"]          <- 1/conv_MtSO2_to_MtS * p11_emiAP_endu[,,"so2"]  
  pm_emi_calib[,,"so2"]            <- 1/conv_MtSO2_to_MtS * pm_emi_calib[,,"so2"]

  if (is.null(pm_limits_wp4_rcp_world)) {
    pm_limits_wp4_rcp <- do.call("mbind",
                                 lapply(setdiff(magclass::getNames(pm_limits_wp4_rcp,dim = 1),"WORLD"),
                                        function(reg){
                                          mreg <- collapseNames(pm_limits_wp4_rcp[,,reg])
                                          getRegions(mreg) <- reg
                                          return(mreg)
                                        }))

    magclass::getNames(pm_limits_wp4_rcp) <- tolower(magclass::getNames(pm_limits_wp4_rcp))
  } else {
    select_species.sector <- paste(sapply(tolower(airpollutants), function(x) paste(x,c("agriculture","agwasteburning","forestburning","grasslandburning","aviation","internationalshipping","waste"), sep=".")))
    pm_limits_wp4_rcp <- pm_limits_wp4_rcp[,,select_species.sector][,,"nh3", invert=TRUE]
  }

  # Merge NH3 data with other air pollutants data
  magclass::getNames(p11_emi_nh3_agwaste)  <- "nh3.agwasteburning"
  magclass::getNames(p11_emi_nh3_ag)       <- "nh3.agriculture"
  magclass::getNames(p11_emi_nh3_forest)   <- "nh3.forestburning"
  magclass::getNames(p11_emi_nh3_savannah) <- "nh3.grasslandburning"
  p_emi_nh3_others = new.magpie(cells_and_regions = getRegions(p11_emi_nh3_savannah), years = getYears(p11_emi_nh3_savannah), names = c("nh3.waste", "nh3.internationalshipping", "nh3.aviation"), fill=0.0)
  p_emi_nh3 = mbind(p11_emi_nh3_agwaste, p11_emi_nh3_ag, p11_emi_nh3_forest, p11_emi_nh3_savannah, p_emi_nh3_others)
  
  ####### calculate minimal temporal and spatial resolutions #####
  getRegions(pm_limits_wp4_rcp) <- toupper(getRegions(pm_limits_wp4_rcp))
  y <- Reduce(intersect,list(getYears(p11_emi_postrun),  getYears(pm_limits_wp4_rcp),   getYears(pm_emiAP), getYears(p11_emiAP_endu)))
  r <- Reduce(intersect,list(getRegions(p11_emi_postrun),getRegions(pm_limits_wp4_rcp), getRegions(pm_emiAP), getRegions(p11_emiAP_endu)))
  p11_emi_postrun          <- p11_emi_postrun[r,y,]
  pm_limits_wp4_rcp        <- pm_limits_wp4_rcp[r,y,]
  pm_limits_wp4_rcp_world  <- pm_limits_wp4_rcp_world[,y,] 
  pm_emiAP                 <- pm_emiAP[r,y,]
  p11_emiAP_endu           <- p11_emiAP_endu[r,y,]
  p_emi_nh3                <- p_emi_nh3[r,y,]

  pm_limits_wp4_rcp = mbind(pm_limits_wp4_rcp, p_emi_nh3)
  
  
  ####### calculate reporting parameters ############
  # Loop over air pollutants and call reporting generating function
  out <- do.call("mbind", lapply(airpollutants, generateReportingEmiAP))

  
  # Add global values
  out <- mbind(out, dimSums(out,dim=1))

  # Add Aviation and Int. Shipping emissions
  if (!is.null(pm_limits_wp4_rcp_world)) {
    for (pollutant in airpollutants) {
      pollutant_low <- tolower(pollutant)

      out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] <- pm_limits_wp4_rcp_world["GLO",,paste(pollutant_low,"internationalshipping",sep= ".")]
      out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]               <- pm_limits_wp4_rcp_world["GLO",,paste(pollutant_low,"aviation",sep= ".")]

      out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)")] <- out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] +  
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]

      out["GLO",,paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)")] <- out["GLO",,paste0("Emi|",pollutant,"|Energy Supply and Demand (Mt ",pollutant,"/yr)")]  +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]

      out["GLO",,paste0("Emi|",pollutant," (Mt ",pollutant,"/yr)")] <- out["GLO",,paste0("Emi|",pollutant," (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|International Shipping (Mt ",pollutant,"/yr)")] +
        out["GLO",,paste0("Emi|",pollutant,"|Energy Demand|Transport|Aviation (Mt ",pollutant,"/yr)")]
    }
  }

  }

  #===================================================================
  # if realisation is exoGAINS
  #===================================================================
  } else if (realisation == "exoGAINS") {
    ######### initialisation  ###########
    airpollutants <- c("so2","bc","oc","CO","VOC","NOx","NH3")
    
    ######### internal function  ###########
    generateReportingEmiAirPol <- function(pollutant,i_emiAPexsolve=pm_emiAPexsolve,i_emiAPexo=pm_emiAPexo){
      poll_rep <- toupper(pollutant)
      tmp <- NULL
      
      # reduce to the pollutant
      emiAPexsolve <- collapseNames(i_emiAPexsolve[,,pollutant])
      emiAPexo     <- collapseNames(i_emiAPexo[,,pollutant])
      getSets(emiAPexo) <- getSets(emiAPexsolve)
      
      # add indprocess to indst
      emiAPexsolve[,,"indst"] <- emiAPexsolve[,,"indst"] + emiAPexsolve[,,"indprocess"]
      
      # Replace REMIND sector names by reporting ones
      mapping = data.frame(
        remind = c("power", "indst", "res", "trans", "solvents", "extraction"),
        reporting = c(paste0("Emi|", poll_rep, "|Energy Supply|Electricity (Mt ", poll_rep, "/yr)"),
                      paste0("Emi|", poll_rep, "|Energy Demand|Industry (Mt ", poll_rep, "/yr)"), 
                      paste0("Emi|", poll_rep, "|Energy Demand|ResCom (Mt ", poll_rep, "/yr)"),
                      paste0("Emi|", poll_rep, "|Energy Demand|Transport|Ground Trans (Mt ", poll_rep, "/yr)"),
                      paste0("Emi|", poll_rep, "|Solvents (Mt ", poll_rep, "/yr)"), 
                      paste0("Emi|", poll_rep, "|Energy Supply|Extraction (Mt ", poll_rep, "/yr)")))
      
      emiAPexsolve <- setNames(emiAPexsolve[,,mapping$remind],as.character(mapping$reporting)) 
     
      tmp <- 
          mbind(emiAPexsolve,
                setNames(emiAPexo[,,"AgWasteBurning"],  paste0("Emi|",poll_rep,"|Land Use|Agricultural Waste Burning (Mt ",poll_rep,"/yr)")),
                setNames(emiAPexo[,,"Agriculture"],     paste0("Emi|",poll_rep,"|Land Use|Agriculture (Mt ",poll_rep,"/yr)")),
                setNames(emiAPexo[,,"ForestBurning"],   paste0("Emi|",poll_rep,"|Land Use|Forest Burning (Mt ",poll_rep,"/yr)")),
                setNames(emiAPexo[,,"GrasslandBurning"],paste0("Emi|",poll_rep,"|Land Use|Savannah Burning (Mt ",poll_rep,"/yr)")),
                setNames(emiAPexo[,,"Waste"],           paste0("Emi|",poll_rep,"|Waste (Mt ",poll_rep,"/yr)")))
    
      
      # Set NAs to 0
      tmp[is.na(tmp)] <- 0
      
      return(tmp)
    }
    
    ####### conversion factors ##########
    pm_conv_TWa_EJ    <- 31.536
    conv_MtSO2_to_MtS <- 1/2     # 32/(32+2*16)
    
    ####### read in needed data #########
    ## sets
    ttot  <-  as.numeric(readGDX(gdx, name=c("ttot"), format="first_found"))
    ## parameter
    pm_emiAPexsolve   <- readGDX(gdx, name=c("pm_emiAPexsolve"), field="l", format="first_found")[,ttot,]
    pm_emiAPexo       <- readGDX(gdx, name=c("pm_emiAPexo"), field="l", format="first_found")[,ttot,airpollutants]
    pm_emiAPexoGlob   <- readGDX(gdx, name=c("pm_emiAPexoGlob"), field="l", format="first_found")[,ttot,airpollutants]
    
    ####### prepare parameter ########################
    magclass::getNames(pm_emiAPexsolve) <- gsub("SOx","so2",magclass::getNames(pm_emiAPexsolve))
    magclass::getNames(pm_emiAPexsolve) <- gsub("NMVOC","VOC",magclass::getNames(pm_emiAPexsolve))
    
    ####### calculate reporting parameters ############
    # Loop over air pollutants and call reporting generating function
    out <- do.call("mbind", lapply(airpollutants, generateReportingEmiAirPol))
      
    # Add global values
    out   <- mbind(out, dimSums(out,dim=1))
    # add other region aggregations
    if (!is.null(regionSubsetList))
      out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))
    
    # Loop over air pollutants and add some variables
    for (pollutant in airpollutants) {
        poll_rep <- toupper(pollutant)
        tmp <- NULL
        # Add Aviation and Int. Shipping emissions
        tmp <- mbind(tmp,setNames(pm_emiAPexoGlob["GLO",,"InternationalShipping"][,,pollutant],paste0("Emi|",poll_rep,"|Energy Demand|Transport|International Shipping (Mt ",poll_rep,"/yr)")),
                         setNames(pm_emiAPexoGlob["GLO",,"Aviation"][,,pollutant],             paste0("Emi|",poll_rep,"|Energy Demand|Transport|Aviation (Mt ",poll_rep,"/yr)"))
                      )
        tmp1 <- new.magpie(getRegions(out),getYears(out),magclass::getNames(tmp),fill=0)
        tmp1["GLO",,] <- tmp["GLO",,]
        out  <- mbind(out,tmp1)
        # Aggregation: Transport and Energy Supply
        out <- mbind(out,
                      setNames(dimSums(out[,,
                                            c(paste0("Emi|",poll_rep,"|Energy Demand|Transport|Ground Trans (Mt ",poll_rep,"/yr)"),
                                              paste0("Emi|",poll_rep,"|Energy Demand|Transport|International Shipping (Mt ",poll_rep,"/yr)"),
                                              paste0("Emi|",poll_rep,"|Energy Demand|Transport|Aviation (Mt ",poll_rep,"/yr)"))],dim = 3),
                               paste0("Emi|",poll_rep,"|Energy Demand|Transport (Mt ",poll_rep,"/yr)")),
                      setNames(dimSums(out[,,
                                            c(paste0("Emi|",poll_rep,"|Energy Supply|Electricity (Mt ",poll_rep,"/yr)"),
                                              paste0("Emi|",poll_rep,"|Energy Supply|Extraction (Mt ",poll_rep,"/yr)"))],dim = 3),
                               paste0("Emi|",poll_rep,"|Energy Supply (Mt ",poll_rep,"/yr)"))
                      )
        # Aggregation: Energy Demand + Energy Supply, Land Use
        out <- mbind(out, 
                      setNames(dimSums(out[,,c(paste0("Emi|",poll_rep,"|Energy Demand|Industry (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Energy Demand|ResCom (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Energy Demand|Transport (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Energy Supply (Mt ",poll_rep,"/yr)"))],dim = 3),
                               paste0("Emi|",poll_rep,"|Energy Supply and Demand (Mt ",poll_rep,"/yr)")),
                      setNames(dimSums(out[,,c(paste0("Emi|",poll_rep,"|Land Use|Savannah Burning (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Land Use|Forest Burning (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Land Use|Agriculture (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Land Use|Agricultural Waste Burning (Mt ",poll_rep,"/yr)"))],dim = 3),
                               paste0("Emi|",poll_rep,"|Land Use (Mt ",poll_rep,"/yr)"))
                     )
        # Compute total
        out <- mbind(out,
                      setNames(dimSums(out[,,c(paste0("Emi|",poll_rep,"|Energy Supply and Demand (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Solvents (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Land Use (Mt ",poll_rep,"/yr)"),
                                               paste0("Emi|",poll_rep,"|Waste (Mt ",poll_rep,"/yr)"))],dim=3),
                               paste0("Emi|",poll_rep," (Mt ",poll_rep,"/yr)"))
                     )
    }
    

  } else {stop("not allowed AP-realization")}  

  return(out)
}

