#' Reporting for the coupled EDGE-T Transport Sector Model (REMIND Module edge_esm)
#'
#' Data is loaded from the EDGE-T subfolder in the output folder.
#' The input files can be (re-) generated calling
#' `Rscript EDGETransport.R --reporting`
#' from the output folder.
#'
#' *Warning* The function modifies the "REMIND_generic_<scenario>.mif" file by appending the
#' additional reporting variables and replaces the "_withoutPlus" version.
#'
#' Region subsets are obtained from fulldata.gdx
#'
#' @param output_folder path to the output folder, default is current folder.
#' @param remind_root path to the REMIND root directory, defaults to two levels up from output_folder.
#' @author Alois Dirnaichner Marianna Rottoli
#'
#' @importFrom rmndt approx_dt
#' @importFrom gdxdt readgdx
#' @importFrom data.table fread fwrite rbindlist
#' @export

reportEDGETransport <- function(output_folder=".",
                                remind_root=NULL) {

  if(is.null(remind_root)){
    remind_root <- file.path(output_folder, "../..")
  }
  gdx <- file.path(output_folder, "fulldata.gdx")

  regionSubsetList <- toolRegionSubsets(gdx)
  sub_folder = "EDGE-T/"

  ## NULL Definitons for codeCheck compliance
  RegionCode <- CountryCode <- cfg <- `.` <- sector <- subsector_L3 <- region <- year <- NULL
  subsector_L2 <- subsector_L1 <- aggr_mode <- vehicle_type <- det_veh <- aggr_nonmot <- NULL
  demand_F <- demand_EJ <- remind_rep <- V25 <- aggr_veh <- technology <- NULL
  variable <- value <- demand_VKM <- loadFactor <- NULL
  all_enty <- ef <- variable_agg <- model <- scenario <- period <- NULL
  Region <- Variable <- co2 <- co2val <- elh2 <- fe  <- NULL
  int <- se <- sec  <- sharesec <- te  <- tech <-  val <- share <- NULL
  eff <- sharebio <- sharesyn <- totseliq <- type <- NULL

  load(file.path(output_folder, "config.Rdata"))

  datapath <- function(fname){
    file.path(output_folder, sub_folder, fname)
  }

  ## load input data from last EDGE run
  demand_km <- readRDS(datapath(fname = "demandF_plot_pkm.RDS"))[
    , demand_F := demand_F * 1e-3] ## million -> billion pkm
  load_factor <- readRDS(datapath(fname = "loadFactor.RDS"))
  demand_vkm <- merge(demand_km, load_factor, by=c("year", "region", "vehicle_type"))
  demand_vkm[, demand_VKM := demand_F/loadFactor] ## billion vkm

  demand_ej <- readRDS(datapath(fname = "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ

  name_mif = list.files(output_folder, pattern = "REMIND_generic", full.names = F)
  name_mif = file.path(output_folder, name_mif[!grepl("withoutPlu", name_mif)])

  stopifnot(typeof(name_mif) == "character")
  miffile <- fread(name_mif, sep=";", header=T)

  ## ES and FE Demand

  reportingESandFE <- function(datatable, mode){

    datatable[, sector := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "Pass", "Freight")]

    ## attribute aggregated mode and vehicle names for plotting purposes, and aggregate
    ## Large Categories (as used by REMIND)
    datatable[subsector_L2 == "trn_pass_road_LDV",
              aggr_mode := "Pass|Road|LDV"]
    datatable[subsector_L2 != "trn_pass_road_LDV" & sector == "Pass",
              aggr_mode := "Pass|non-LDV"]
    ## Freight is already in the "normal" reporting
    ## datatable[is.na(aggr_mode), aggr_mode := "Freight"]

    ## A little more detail: Vehicle Aggregates
    datatable[grepl("^Truck", vehicle_type), aggr_veh := "Freight|Road"]
    datatable["Freight Rail_tmp_vehicletype" == vehicle_type, aggr_veh := "Freight|Rail"]
    ## there seem to be no passenger ships in EDGE-T!
    datatable[subsector_L3 == "International Ship", aggr_veh := "Freight|International Shipping"]
    datatable[subsector_L3 == "Domestic Ship", aggr_veh := "Freight|Navigation"]

    datatable[grepl("Bus", vehicle_type), aggr_veh := "Pass|Road|Bus"]
    if(mode == "ES")
      datatable[grepl("Cycle|Walk", subsector_L3), aggr_nonmot := "Pass|Road|Non-Motorized"]

    ## Rail & Aviation
    datatable[grepl("Passenger Rail|HSR", vehicle_type), aggr_veh := "Pass|Rail"]
    datatable[subsector_L3 == "Domestic Aviation", aggr_veh := "Pass|Aviation|Domestic"]
    datatable[subsector_L3 == "International Aviation", aggr_veh := "Pass|Aviation|International"]

    ## High Detail: Ecoinvent-Compatible Output
    datatable[grepl("Subcompact", vehicle_type),
              det_veh := "Pass|Road|LDV|Small"]
    datatable[grepl("Mini", vehicle_type),
              det_veh := "Pass|Road|LDV|Mini"]
    datatable[vehicle_type == "Compact", det_veh := "Pass|Road|LDV|Medium"]
    datatable[vehicle_type == "Large Car|Midsize Car", det_veh := "Pass|Road|LDV|Large"]
    datatable[grepl("SUV", vehicle_type),
              det_veh := "Pass|Road|LDV|SUV"]
    datatable[grepl("Van|Multipurpose", vehicle_type),
              det_veh := "Pass|Road|LDV|Van"]

    datatable[grepl("Motorcycle|Scooter|Moped", vehicle_type),
              det_veh := "Pass|Road|LDV|Two-Wheelers"]

    prepare4MIF <- function(dt, remind_unit, valcol, varcol){
      ## REMIND years, loading from MIF File would take too long
      yrs <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
      remind_scenario <- cfg$title

      prefix <- switch(mode,
                       "FE" = "FE|Transport|",
                       "ES" = "ES|Transport|",
                       "VKM" = "ES|Transport|VKM|")

      ## we only care for non-NA variables (NA is basically *all others*)
      toadd = dt[!is.na(get(varcol)), .(model="REMIND", scenario=remind_scenario, region,
                                        variable=paste0(prefix, get(varcol)),
                                        unit=remind_unit, period=year,
                                        value=get(valcol))]

      toadd <- approx_dt(toadd, yrs, xcol="period", ycol = "value",
                         idxcols=colnames(toadd)[1:5],
                         extrapolate=T)
      return(toadd)
    }

    if(mode == "ES"){
      report <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "bn pkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn tkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn pkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(aggr_nonmot), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_nonmot")],
          "bn pkm/yr", "V1", "aggr_nonmot"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "det_veh")], "bn pkm/yr", "V1", "det_veh")))
    }else if(mode == "VKM"){
      report <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "bn vkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "det_veh")], "bn vkm/yr", "V1", "det_veh")))
    }else{
      report <- rbindlist(list(
        prepare4MIF(
          datatable[!is.na(aggr_mode), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "EJ/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[!is.na(aggr_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "EJ/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "det_veh")],
          "EJ/yr", "V1", "det_veh")))
    }

    ## with energy carrier

    ## remove cycling and walking placeholder techs for ESs
    techmap <- data.table(technology=unique(datatable$technology),
                          key="technology")[!c("Cycle_tmp_technology", "Walk_tmp_technology")]

    if(mode == "ES"){
      ## for energy services, it is better to refer to the actual technologies
      ## and not the fuel types (-> LCA)
      techmap[, remind_rep := technology]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
          "bn pkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
          "bn tkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
          "bn pkm/yr", "V1", "aggr_veh"),
        prepare4MIF(datatable[!is.na(det_veh), sum(demand_F, na.rm=T),
                              by = c("region", "year", "det_veh", "remind_rep")
                              ][, det_veh := paste0(det_veh, "|", remind_rep)],
                    "bn pkm/yr", "V1", "det_veh")))
    }else if(mode == "VKM"){
      ## for energy services, it is better to refer to the actual technologies
      ## and not the fuel types (-> LCA)
      techmap[, remind_rep := technology]
      techmap["LA-BEV", remind_rep := "BEV"]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
          "bn vkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(datatable[!is.na(det_veh), sum(demand_VKM, na.rm=T),
                              by = c("region", "year", "det_veh", "remind_rep")
                              ][, det_veh := paste0(det_veh, "|", remind_rep)],
                    "bn vkm/yr", "V1", "det_veh")))
    }else{
      techmap["BEV", remind_rep := "Electricity"]
      techmap["Electric", remind_rep := "Electricity"]
      techmap["FCEV", remind_rep := "Hydrogen"]
      techmap["Hydrogen", remind_rep := "Hydrogen"]
      techmap["Liquids", remind_rep := "Liquids"]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[!is.na(aggr_mode), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
          "EJ/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[!is.na(aggr_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
          "EJ/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "det_veh", "remind_rep")
                    ][, det_veh := paste0(det_veh, "|", remind_rep)],
          "EJ/yr", "V1", "det_veh")))

    }
    ## add World
    report_w = report[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    report = rbind(report, report_w)

    report_tech_w = report_tech[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    report_tech = rbind(report_tech, report_tech_w)
    return(rbindlist(list(report, report_tech)))
  }

  ## Demand emissions
  reportingEmi <- function(repFE, gdx, miffile){

    ## load emission factors for fossil fuels
    p_ef_dem <- readGDX(gdx, "p_ef_dem")  ## MtCO2/EJ
    p_ef_dem <- as.data.table(p_ef_dem)[all_enty %in% c("fepet", "fegas", "feelt", "feh2t")]  ## emissions factor for Electricity and Hydrogen are 0, as we are calculating tailpipe emissions
    setnames(p_ef_dem, old = "value", new = "ef")
    ## attribute explicitly fuel used to the FE values
    emidem = repFE[grepl("Liquids|Gases|Hydrogen|Electricity", variable) & region != "World"]   ## EJ
    emidem[, all_enty := ifelse(grepl("Liquids", variable), "fepet", NA)]
    emidem[, all_enty := ifelse(grepl("Gases", variable), "fegas", all_enty)]
    emidem[, all_enty := ifelse(grepl("Electricity", variable), "feelt", all_enty)]
    emidem[, all_enty := ifelse(grepl("Hydrogen", variable), "feh2t", all_enty)]
    ## merge with emission factors
    emidem = emidem[p_ef_dem, on = "all_enty"]
    ## calculate emissions and attribute variable and unit names
    emidem[, value := value*ef][, c("variable", "unit") := list(gsub("FE", "Emi\\|CO2", variable), "Mt CO2/yr")]

    emidem = rbind(emidem[region %in% unique(emidem$region)][,type := "tailpipe"], emidem[region %in% unique(emidem$region)][,type := "demand"])

    ## create corresponding entries with share of biofuels+synfuels in total liquids, for passenger SM, LO, freight SM, LO
    TWa_2_EJ <- 31.536
    ## demSe: secondary energy demand, secondary energy carrier units
    demSe <- readgdx(gdx, "vm_demSe")[, value := value*TWa_2_EJ]
    setnames(demSe, c("year", "region", "se", "fe", "te", "value"))
    ## prodSe: secondary energy production, secondary energy carrier units
    prodSe <- readgdx(gdx, "vm_prodSe")[, value := value*TWa_2_EJ]
    setnames(prodSe, c("year", "region", "pe", "se", "te", "value"))
    ## energy conversion for the different technologies
    etaconv = readgdx(gdx, "pm_eta_conv")
    setnames(etaconv, c("year", "region", "te", "eff"))
    ## separately see MeOH conversion
    convMeOH = unique(etaconv[te == "MeOH", eff])

    ## calculate the shares of seliqfos for the two sectors, transport and stationary (in seliqfos units)
    shareLiqSec = demSe[se == "seliqfos"]
    shareLiqSec[, sec := ifelse(fe %in% c("fepet", "fedie"), "trsp", "st")]
    shareLiqSec = shareLiqSec[, .(value = sum(value)), by = .(region, year, sec)]
    shareLiqSec[, share := value/sum(value), by =.(region, year)]
    shareLiqSec[, c("value") := NULL]

    ## create pathway in demSe for synfuels to transport and synfuels to stationary
    demSeSyn = merge(demSe[fe == "seliqfos" & te == "MeOH", .(year, region, value)], shareLiqSec, all = TRUE, by = c("region", "year"))
    demSeSyn[is.na(value), value := 0]
    demSeSyn[, value := share*value*convMeOH]  ## convert in seliqfos values
    demSeSyn[, share := NULL]
    demSeSyn[, fe := ifelse(sec == "trsp", "fesynt", "fesyns")]
    demSeSyn[, se := "seliqfos"]


    ## calculate the seliqfos from synfuels as a share of the total seliqfos
    demSeLiq = demSe[se =="seliqfos"]
    demSeLiq = demSeLiq[, sec := ifelse(fe %in% c("fepet", "fedie"), "trsp", "st")]
    demSeLiq = demSeLiq[,.(totseliq = sum(value)), by = c("year", "region", "sec")]
    demSeLiq = merge(demSeLiq, demSeSyn, by = c("region", "year", "sec"))
    demSeLiq[, sharesyn := value/(totseliq)]
    demSeLiq = demSeLiq[, .(sharesyn = sum(sharesyn)), .(region, year, sec)]
    setnames(demSeLiq, old = "year", new = "period")
    demSeLiq[, period := as.numeric(period)]

    ## calculate share of biofuels on total seliqfos+seliqbio
    shareBioSec = demSe[se %in% c("seliqfos", "seliqbio")]
    shareBioSec[, sec := ifelse(fe %in% c("fepet", "fedie"), "trsp", "st")]
    shareBioSec = shareBioSec[sec == "trsp"]
    shareBioSec = shareBioSec[, sharebio := value/sum(value), by = .(region, year)]
    shareBioSec = shareBioSec[,.(sharebio = sum(sharebio)), .(region, year, se)]
    shareBioSec = shareBioSec[se == "seliqbio"]
    shareBioSec[, c("se") := NULL]
    setnames(shareBioSec, old = "year", new = "period")
    shareBioSec[, period := as.numeric(period)]

    ## decrease the "demand" entries of the amount of biofuels
    emidem = merge(emidem, shareBioSec, by = c("region", "period"))
    emidem[type == "demand" & all_enty %in% c("fedie", "fepet"), value := value*(1-sharebio)]

    ## merge demand and shares of synfuels+biofuels
    emidem = merge(emidem, demSeLiq[sec == "trsp"][, sec := NULL], by = c("region", "period"))
    emidem[type == "demand", value := value*(1-sharesyn)]

    ## the taipipe emissions are to be labeled as "Tailpipe"
    emidem[type == "tailpipe", variable := paste0(variable, "|Tailpipe")]
    ## the demand emissions are to be labeled as "Demand"
    emidem[type == "demand", variable := paste0(variable, "|Demand")]

    emidem[, c("sharesyn", "sharebio", "type", "ef", "V3", "V2", "all_enty") := NULL]

    ## aggregate removing the fuel dependency
    emidem[, variable_agg := gsub("\\|Liquids|\\|Electricity|\\|Hydrogen|\\|Gases", "", variable)]
    emidem = emidem[, .(value = sum(value)), by = c("model", "scenario", "region", "unit", "period", "variable_agg")]
    setnames(emidem, old = "variable_agg", new = "variable")
    emidem = emidem[, .(model, scenario, region, variable, unit, period, value)]

    return(emidem)
  }

  repFE <- reportingESandFE(
      demand_ej,
    mode ="FE")

  toMIF <- rbindlist(list(
    repFE,
    reportingESandFE(
      datatable=demand_km,
      mode="ES"),
    reportingESandFE(
      datatable=demand_vkm,
      mode="VKM"),
    reportingEmi(repFE = repFE, gdx = gdx, miffile = miffile)
  ))

  ## add Road Totals
  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("ES\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="ES|Transport|Pass|Road",
            unit="bn pkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Pass|Road|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Pass|Road|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("ES\\|Transport\\|VKM\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="ES|Transport|VKM|Pass|Road",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("FE\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="FE|Transport|Pass|Road",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]), use.names = TRUE)

  ## VKM totals, Road and Rail
  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("ES\\|Transport\\|VKM\\|(Pass|Freight)\\|Road$", variable),
          .(variable="ES|Transport|VKM|Road",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("ES\\|Transport\\|VKM\\|(Pass|Freight)\\|Rail$", variable),
          .(variable="ES|Transport|VKM|Rail",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]
    ), use.names = TRUE)

  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("FE\\|Transport\\|(Pass|Freight)\\|Road$", variable),
          .(variable="FE|Transport|Road",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Road\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Road|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Rail\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Rail|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Road\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Road|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Rail\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Rail|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("FE\\|Transport\\|(Pass|Freight)\\|Rail$", variable),
          .(variable="FE|Transport|Rail",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]), use.names = TRUE)



  if(!is.null(regionSubsetList)){
    toMIF <- toMIF[region %in% regionSubsetList]
  }

  ## Make sure there are no duplicates!
  idx <- anyDuplicated(toMIF, by = c("region", "variable", "period"))
  if(idx){
    warning(paste0("Duplicates found in EDGE-T reporting output:",
                   capture.output(toMIF[idx]), collapse="\n"))
  }

  toMIF <- data.table::dcast(toMIF, ... ~ period, value.var="value")

  EOL <- if (.Platform$OS.type=="windows") ";\r\n" else ";\n"

  fwrite(toMIF, name_mif, append=T, sep=";", eol=EOL)
  deletePlus(name_mif, writemif=T)
}
