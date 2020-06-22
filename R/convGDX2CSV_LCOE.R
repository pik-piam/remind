#' Read in GDX and write LCOE .csv reporting
#' 
#' Read in all information from GDX file, calculate the LCOE for pe2se technologies and create
#' the LCOE .csv reporting
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param file name of the csv file which will be written
#' @param scen scenario name that is used in the *.mif reporting
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2030,2050)
#' @author Felix Schreyer
#' @examples
#' \dontrun{convGDX2CSV_LCOE(gdx,file="REMIND_LCOE_reporting.csv",scenario="default")}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% mutate

convGDX2CSV_LCOE <- function(gdx,file=NULL,scen="default",t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)) {
  
  # initialize variables for dplyr operations
  scenario <- NULL
  model <- NULL
  region <- NULL
  period <- NULL
  type <- NULL
  cost <- NULL
  value <- NULL
  unit <- NULL
  tech <- NULL
  
  # make the reporting
  output <- NULL
  output <- mbind(output,reportLCOE(gdx)[,t,]) 
  
  # convert to quitte, 
  # if reportLCOE only produced NA because gdx is from depreciated REMIND version -> return NA
  if (all(is.na(output[,,]))) {
    df.LCOE.report <- as.quitte(output)
  } else {
    df.LCOE.report <- as.quitte(output) %>% 
      mutate(scenario = scen, model = "REMIND") %>% 
      select(model, scenario, region, period, type, tech, output, unit, cost, value)
  }

  # write the LCOE.mif or give back the magpie opject output
  if(!is.null(file)) {
    write.table(df.LCOE.report, file=file, quote = F, row.names = F, col.names = T, sep = ";")
  } else {
    return(output)
  }
  
}