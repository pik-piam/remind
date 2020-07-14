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
  variable <- value <- NULL
  all_enty <- ef <- variable_agg <- model <- scenario <- period <- NULL
  Region <- Variable <- allEl <- co2 <- co2val <- elh2 <- elh2Dir <- elh2Syn <- fe <- fosh2  <- NULL
  fosh2Dir <- fosh2Syn <- int <- pe <- se <- sec  <- sharesec  <- syn <- te  <- tech <-  val <- el <- share <- NULL

  load(file.path(output_folder, "config.Rdata"))

  datapath <- function(fname){
    file.path(output_folder, sub_folder, fname)
  }

  REMIND2ISO_MAPPING <- fread(file.path(remind_root, cfg$regionmapping))[, .(iso = CountryCode, region = RegionCode)]

  ## load input data from last EDGE run
  demand_km <- readRDS(datapath(fname = "demandF_plot_pkm.RDS"))[
    , demand_F := demand_F * 1e-3] ## million -> billion pkm

  demand_ej <- readRDS(datapath(fname = "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ

  name_mif = list.files(output_folder, pattern = "REMIND_generic", full.names = F)
  name_mif = file.path(output_folder, name_mif[!grepl("withoutPlu", name_mif)])

  stopifnot(typeof(name_mif) == "character")
  miffile <- fread(name_mif, sep=";", header=T)

  ## ES and FE Demand

  reportingESandFE <- function(datatable, mode){

    ## datatable <- datatable[, c("sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "iso", "year", "demand_F")]
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

    datatable[grepl("bus|Bus", vehicle_type), aggr_veh := "Pass|Road|Bus"]
    if(mode == "ES")
      datatable[grepl("Cycle|Walk", subsector_L3), aggr_nonmot := "Pass|Road|Non-Motorized"]

    ## Rail & Aviation
    datatable[grepl("Passenger Rail|HSR", vehicle_type), aggr_veh := "Pass|Rail"]
    datatable[subsector_L3 == "Domestic Aviation", aggr_veh := "Pass|Aviation|Domestic"]
    datatable[subsector_L3 == "International Aviation", aggr_veh := "Pass|Aviation|International"]

    ## High Detail: Ecoinvent-Compatible Output
    datatable[grepl("Compact|Subcompact", vehicle_type),
              det_veh := "Pass|Road|LDV|Small"]
    datatable[grepl("Mini|Three-Wheeler", vehicle_type),
              det_veh := "Pass|Road|LDV|Mini"]
    datatable[vehicle_type == "Midsize Car", det_veh := "Pass|Road|LDV|Medium"]
    datatable[vehicle_type == "Large Car", det_veh := "Pass|Road|LDV|Large"]
    datatable[grepl("SUV", vehicle_type),
              det_veh := "Pass|Road|LDV|SUV"]
    datatable[grepl("Van|Multipurpose", vehicle_type),
              det_veh := "Pass|Road|LDV|Van"]

    datatable[grepl("Motorcycle|Scooter|Moped", vehicle_type),
              det_veh := "Pass|Road|LDV|Two-Wheelers"]

    datatable = datatable[REMIND2ISO_MAPPING, on = "iso"]

    prepare4MIF <- function(dt, remind_unit, valcol, varcol){
      ## REMIND years, loading from MIF File would take too long
      yrs <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
      remind_scenario <- cfg$title

      prefix <- ifelse(mode == "ES", "ES|Transport|", "FE|Transport|")

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
      techmap["LA-BEV", remind_rep := "BEV"]
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
    }else{
      techmap["BEV", remind_rep := "Electricity"]
      techmap["Electric", remind_rep := "Electricity"]
      techmap["LA-BEV", remind_rep := "Electricity"]
      techmap["FCEV", remind_rep := "Hydrogen"]
      techmap["Liquids", remind_rep := "Liquids"]
      techmap["Hybrid Liquids", remind_rep := "Liquids"]
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

    #### Tailpipe emissions ####
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
    ## the emissions are to be labeled as "Demand"
    emidem[, variable := paste0(variable, "|Demand")]

    emidem[, c("ef", "V3", "V2", "all_enty") := NULL]
    ## aggregate removing the fuel dependency
    emidem[, variable_agg := gsub("\\|Liquids|\\|Electricity|\\|Hydrogen|\\|Gases", "", variable)]
    emidem = emidem[, .(value = sum(value)), by = c("model", "scenario", "region", "unit", "period", "variable_agg")]
    setnames(emidem, old = "variable_agg", new = "variable")
    emidem = emidem[, .(model, scenario, region, variable, unit, period, value)]

    ####  calculate Tailpipe+Energy system emissions = Supply and Demand emissions ####
    TWa_2_EJ <- 31.536
    GtC_2_MtCO2 <- 44 / 12 * 1000

    ## prodSe: secondary energy production, secondary energy carrier units
    prodSe <- readgdx(gdx, "vm_prodSe")[, value := value*TWa_2_EJ]
    setnames(prodSe, c("year", "region", "pe", "se", "te", "value"))
    ## demSe: secondary energy demand, secondary energy carrier units
    demSe <- readgdx(gdx, "vm_demSe")[, value := value*TWa_2_EJ]
    setnames(demSe, c("year", "region", "se", "fe", "te", "value"))

    ## create a variable substitute of seliqfos that is divided in two components: fesynt and fesyns (transport and stationary)
    shareSynSec = demSe[se == "seliqfos"]
    shareSynSec[, sec := ifelse(fe %in% c("fepet", "fedie"), "trsp", "st")]
    shareSynSec = shareSynSec[, .(value = sum(value)), by = .(region, year, sec)]
    shareSynSec[, share := value/sum(value), by =.(region, year)]
    shareSynSec[, fe := ifelse(sec == "trsp", "fesynt", "fesyns")]
    shareSynSec[, c("se", "te") := list("seh2", "MeOH")]
    shareSynSec[, c("value", "sec") := NULL]

    ## create pathway in demSe for synfuels to transport and synfuels to stationary
    demSeSyn = merge(demSe[fe == "seliqfos" & te == "MeOH", .(year, region, value, te)], shareSynSec, all = TRUE, by = c("region", "year", "te"))
    demSeSyn[is.na(value), value := 0]
    demSeSyn[, value := share*value]
    demSeSyn[, share := NULL]

    ## substitute the non-separated parts of demSe with the newly calculated fesyn for transport and stationary
    demSe = rbind(demSe[te != "MeOH"], demSeSyn)

    ## final energies that are directly going into transport and stationary are calculated
    demSeSec = demSe[, sec := ifelse(fe %in% c("fepet", "fedie", "feelt", "fegat", "feh2t", "fesynt"), "trsp", NA)]
    demSeSec[, sec := ifelse(fe %in% c("fesos", "fehos", "feels", "feh2s", "fegas", "fehes", "fesyns"), "st", sec)]

    ## Only se->fe transformations
    demSeFe = demSeSec[fe != "seh2",] ## remove electricity used to produce hydrogen (no secondary to secondary path)
    demSeFe = demSeFe[,.(value = sum(value)), by = c("year", "region", "se", "sec")]
    demSeFe[, share := value/sum(value), by = .(year, region, se)]

    ## share of hydrogen used in transport (both directly and to produce synfuels, accounted for separately)
    shareH2Trsp = demSe[se == "seh2" & fe %in% c("feh2s", "feh2t", "fesyns", "fesynt"), c("year", "region", "value", "fe")]
    shareH2Trsp[, share := value/sum(value), by = c("region", "year")]

    ## Electricity consumption for H2 production (in electricity units)
    prodElH2 <- demSeSec[se == "seel" & fe == "seh2" & !te %in% c("elh2VRE")]  ## the "dummy" variable needs to be removed
    prodElH2[, elh2 := sum(value), by = .(year, region)]
    prodElH2[, c("te", "value", "sec", "se", "fe") := NULL]
    prodElH2 <- unique(prodElH2)

    ## ... of which only a share is used in transport, directly and in synfuels (accounted for separately)
    prodElH2trp <- merge(prodElH2, shareH2Trsp[fe %in% c("feh2t", "fesynt")], all.x = TRUE, by = c("year", "region"))
    prodElH2trp[, elh2 := elh2*share]
    prodElH2trp[, c("share") := NULL]

    ## Emissions
    vemi <- readgdx(gdx, "vm_emiTeDetail")
    setnames(vemi, c("year", "region", "pe", "se", "te", "emi", "val"))

    ## CO2 emission for electricity
    emiElco2 <- vemi[se == "seel" & emi == "co2"][
      , co2val := sum(val) * GtC_2_MtCO2, by=.(year, region)][
        , c("pe", "se", "te", "emi", "val") := NULL
        ]
    emiElco2 <- unique(emiElco2)

    ## electricity production secondary energy
    prodEl <- prodSe[se == "seel"]
    prodEl[, allEl := sum(value), by=.(year, region)][
      , c("pe", "se", "te", "value") := NULL
      ]
    prodEl <- unique(prodEl)

    ## emissions intensity of electricity
    emiElco2 <- merge(emiElco2, prodEl, by=c("year", "region"))
    emiElco2[, int := co2val/allEl] # MtCO2/EJ -> tCO2/MJ
    emiElco2[, c("allEl", "co2val") := NULL]

    ## Emissions from electricity used to produce hydrogen (used directly and to produce synfuels)
    emih2El <-  merge(prodElH2trp, emiElco2, by = c("region", "year"))
    emih2El[, co2 := elh2*int]
    emih2El[, c("elh2", "value", "int") := NULL]

    ## Emissions to produce hydrogen from fossil fuels
    emiH2Fosco2 <- vemi[se == "seh2" & emi == "co2"][
      , co2val := sum(val) * GtC_2_MtCO2, by=.(year, region)][
        , c("pe", "se", "te", "emi", "val") := NULL
        ]
    emiH2Fosco2 <- unique(emiH2Fosco2)


    ## Fossils/biomass secondary energy for H2 production
    prodFosH2 <- prodSe[pe != "seel" & se == "seh2"]
    prodFosH2[, fosh2 := sum(value), by=.(year, region)]
    prodFosH2[, c("pe", "te", "value") := NULL]
    prodFosH2 <- unique(prodFosH2)

    ## emissions intensity of fossil-based hydrogen
    emiH2Fosco2 <- merge(emiH2Fosco2, prodFosH2, by=c("year", "region"))
    emiH2Fosco2[, int := co2val/fosh2] # MtCO2/EJ -> tCO2/MJ
    emiH2Fosco2[, c("fosh2", "co2val") := NULL]

    ## Fossil/biomass for H2 in transport (both directly used and for synfuels, accounted for separately)
    prodFosH2trp <- merge(prodFosH2, shareH2Trsp[fe %in% c("feh2t", "fesynt")], all.x = TRUE, by = c("year", "region"))
    prodFosH2trp[, fosh2 := fosh2*share]
    prodFosH2trp[, c("share", "value") := NULL]


    ## Emissions from fossils used to produce hydrogen (both directly used and for synfuels, accounted for separately)
    emih2Fos <-  merge(prodFosH2trp, emiH2Fosco2, by = c("region", "year", "se"))
    emih2Fos[, co2 := fosh2*int]
    emih2Fos[, c("fosh2", "int", "se") := NULL]

    ## Emissions from hydrogen (from electricity and from fossil fuels, for both h2 used directly and synfuels, acocunted for separately)
    emih2 = rbind(emih2Fos, emih2El)
    emih2 = emih2[,.(co2 = sum(co2)), by = c("region", "year", "fe")]

    ## fossil emissions for the whole transport sector (the loaded values contain: tailpipe emissions, refineries, losses. NOT synfuels, which will be added in the following lines)
    fosemi = miffile[
      Variable %in% c("Emi|CO2|Transport|Pass|Short-Medium Distance|Liquids",
                      "Emi|CO2|Transport|Pass|Long Distance|Liquids",
                      "Emi|CO2|Transport|Freight|Short-Medium Distance|Liquids",
                      "Emi|CO2|Transport|Freight|Long Distance|Liquids")][, V25 := NULL]

    fosemi = data.table::melt(fosemi, id.vars = c("Model", "Scenario", "Region", "Unit", "Variable"))
    fosemi[, value :=as.numeric(value)]
    fosemi = fosemi[,.(Variable = "Emi|CO2|Transport|Liquids", value = sum(value)), by = c("Region", "variable", "Model", "Unit")]
    fosemi = fosemi[, .(year = variable, region = Region, se="fos", co2=as.numeric(value))]

    ## calculate fosemi+synfuels
    fosemi = rbind(fosemi, emih2[fe == "fesynt"][, c("se", "fe") := list("fos", NULL)])
    fosemi = fosemi[, .(co2 = sum(co2), se = "fos"), by = c("year", "region")]

    ## of the total electrity, only a share is used in transport (directly)
    prodEltrp <- merge(prodEl, demSeFe[sec == "trsp" & se == "seel", c("year", "region", "share")], all.x = TRUE, by = c("year", "region"))
    prodEltrp[, el := allEl*share]
    prodEltrp[, c("share", "allEl") := NULL]

    ## Emissions from electricity used directly
    emiel <-  merge(prodEltrp, emiElco2, by = c("region", "year"))
    emiel[, co2 := el*int]
    emiel[, c("el", "int") := NULL]
    emiel[, se := "el"]

    ## natural gas emissions
    emigas = miffile[
      Variable %in% c("Emi|CO2|Transport|Pass|Short-Medium Distance|Gases",
                      "Emi|CO2|Transport|Freight|Short-Medium Distance|Gases")][, V25 := NULL]

    emigas = data.table::melt(emigas, id.vars = c("Model", "Scenario", "Region", "Unit", "Variable"))
    emigas[, value :=as.numeric(value)]
    emigas = emigas[,.(Variable = "Emi|CO2|Transport|Gases", value = sum(value)), by = c("Region", "variable", "Model", "Unit")]
    emigas = emigas[, .(year = variable, region = Region, se="ng", co2=as.numeric(value))]

    ## rename h2 emissions
    emih2[, se := ifelse(fe == "feh2t", "h2", "syn") ]
    emih2[, fe := NULL]
    ## combine all
    emi_all <- rbindlist(list(
      emih2,
      fosemi,
      emiel,
      emigas),
      use.names=TRUE)

    ## year and co2 columns should be numeric
    emi_all[, c("year", "co2") := list(as.numeric(as.character(year)), as.numeric(co2))]

    ## convert to wide format
    emi_wide <- data.table(dcast(emi_all, year+region ~ se, value.var = "co2"))
    emi_wide[, year := as.numeric(as.character(year))]
    ## attribute a 0 to the regions where there are no h2 emissions before 2020 (the conversion to wide format creates NAs as they have no corresponding value)
    emi_wide = emi_wide[is.na(syn), syn := 0]

    remind_scenario <- cfg$title
    emi_wide[, c("model", "scenario", "unit") := list("REMIND", remind_scenario, "Mt CO2/yr")]

    emi_result <- data.table::melt(emi_wide,
                                   value.name="co2",
                                   id.vars = c("year", "region", "model", "unit", "scenario"),
                                   variable.name = "tech")

    emi_result[, variable := ifelse(tech == "fos", "Emi|CO2|Transport|Liquids|WithSynfuels", NA)]
    emi_result[, variable := ifelse(tech == "syn", "Emi|CO2|Transport|Synfuels", variable)]
    emi_result[, variable := ifelse(tech == "h2", "Emi|CO2|Transport|Hydrogen", variable)]
    emi_result[, variable := ifelse(tech == "el", "Emi|CO2|Transport|Electricity", variable)]
    emi_result[, variable := ifelse(tech == "ng", "Emi|CO2|Transport|Gases", variable)]

    ## fill in the values with NAs (e.g. hydrogen or synfuels in historical years)
    emi_result[is.na(co2), co2 := 0]

    ## calculate World values
    emi_result[, co2 := ifelse(region == "World", sum(co2), co2), by = c("year", "variable")]

    emi_result[, c("tech") := NULL]
    names(emi_result) = c("period", "region", "model", "unit", "scenario", "value", "variable")

    emi = rbind(emidem, emi_result)

    return(emi)
  }

  repFE <- reportingESandFE(
    demand_ej,
    mode ="FE")

  toMIF <- rbindlist(list(
    reportingESandFE(
      datatable=demand_km,
      mode="ES"),
    repFE,
    reportingEmi(repFE = repFE, gdx = gdx, miffile = miffile)
  ))

  ## add Road Totals
  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("ES\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="ES|Transport|Pass|Road",
            unit="bn pkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Pass|Road|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("FE\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="FE|Transport|Pass|Road",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]), use.names = TRUE)

  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("FE\\|Transport\\|(Pass|Freight)\\|Road$", variable),
          .(variable="FE|Transport|Road",
            unit="EJ/yr", value=sum(value)),
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
  if(length(toMIF) < length(miffile)){
    toMIF[, V25 := ""]
  }

  fwrite(toMIF, name_mif, append=T, sep=";")
  deletePlus(name_mif, writemif=T)
}
