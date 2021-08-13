#' Read bioenergy supplycurve from GDX
#' 
#' Read coefficients for bioenergy prices from gdx and calculate bioenergy supplycurve. Also read the actual
#' demand for bioenergy from gdx. 
#' 
#' @param outputdirs Vector providing the folders that contain the gdx files
#' @param userfun Function that was used to fit the supplycurve. Is needed to calculate the supplycurve correctly.
#' User can provide a functional form using the following syntax: \code{function(param,x)return(param[[1]] + param[[2]] * x ^param[[3]])}. This function is the default.
#' @param mult_on Should the multiplication factor (read from the gdx) be applied on the entire forumla ("all"), or on the slope only ("slope"). Use "all" for REMIND 2.0 (this is the default) and "slope" for REMIND 1.7
#' @importFrom gdx readGDX
#' @importFrom magclass mbind collapseNames add_dimension getSets
#' @importFrom remulator calc_supplycurve
#' @author David Klein
#' @seealso \code{\link[remulator]{emulator}} \code{\link{calc_supplycurve}}
#' @export

readSupplycurveBio <- function(outputdirs, userfun = function(param,x) {return(param[[1]] + param[[2]] * x)}, mult_on = "all") {
  
  #########################################################################
  ####################### Internal functions ##############################
  #########################################################################
  # function to read parameter from gdx file
  .readpar <- function(gdx,name) {
    out <- readGDX(gdx, name, format="first_found")
    getNames(out) <- "dummy" # something has to be here, will be removed by collapseNames anyway
    out <- collapseNames(out)
    return(out)
  }
  
  # function to read variable from gdx file
  .readvar <- function(gdx,name,enty=NULL) {
    if (is.null(enty)) {
      out <- readGDX(gdx,name, format="first_found", field="l")
      getNames(out) <- "dummy" # something has to be here, will be removed by collapseNames anyway
    } else {
      out <- readGDX(gdx,name=name, format="first_found", field="l")[,,enty]
    }
    out <- collapseNames(out)
    return(out)
  }
  
  #########################################################################
  ########################## Settings #####################################
  #########################################################################
  TWa2EJ          <- 31.5576         # TWa to EJ (1 a = 365.25*24*3600 s = 31557600 s)
  sm_tdptwyr2dpgj <- 1/(TWa2EJ/1000) # Wa to GJ: TWa2EJ/1000
  
  gdx_path <- paste0(outputdirs,"/fulldata.gdx")
  
  # retrieve run titles
  for (i in 1:length(gdx_path)) {
    names(gdx_path)[i] <- readGDX(gdx_path[i],"c_expname", format="first_found")
  }
  
  #########################################################################
  ############ Read in fit coefficients ###################################
  #########################################################################
  
  a <- readAll(gdx_path, .readpar, name= c("i30_bioen_price_a","i30_bioen_price_at1"),asList=FALSE)
  a <- add_dimension(a,dim=3.2,add="coeff",nm="a")
  b <- readAll(gdx_path, .readpar, name= c("i30_bioen_price_b","i30_bioen_price_at2"),asList=FALSE)
  b <- add_dimension(b,dim=3.2,add="coeff",nm="b")
  
  fitcoef <- mbind(a,b)
  
  getSets(fitcoef) <- c("region","year","scenario","coeff")
  
  #########################################################################
  ###################### Read REMIND points ###############################
  #########################################################################
  
  # demand
  x <- readAll(gdx_path,.readvar,name="vm_fuExtr",enty="pebiolc",asList=FALSE)[,,"1"]
  # price
  y <- readAll(gdx_path,.readvar,name="vm_pebiolc_price",asList=FALSE)
  
  x <- add_dimension(x,dim=3.3,add="variable",nm="x")
  y <- add_dimension(y,dim=3.2,add="sample",nm="1")
  y <- add_dimension(y,dim=3.3,add="variable",nm="y")
  
  rem_point <- mbind(x,y)
  
  getSets(rem_point) <- c("region","year","scenario","sample","variable")
  
  #########################################################################
  ###################### Calculate supplycurve ############################
  #########################################################################
  
  supplycurve_unshifted <- calc_supplycurve(rem_point,fitcoef,userfun)
  
  mult <- readAll(gdx_path,.readvar,name="v30_pricemult",asList=FALSE)
  
  supplycurve_shifted <- supplycurve_unshifted
  supplycurve_shifted[,,"y"] <- supplycurve_unshifted[,,"y"] * mult
  
  if (mult_on == "all") {
    # do nothing, let the supplycurve_shifted untouched  
  
  } else if (mult_on == "slope") {
    # Correcting the price (y): line above applies mult on entire formula, but REMIND (before revision 8148 27.9.2018) 
    # applies it only on slope -> make appliccance on intercept rueckgaengig
    supplycurve_shifted[,,"y"] <- supplycurve_shifted[,,"y"] + collapseNames(fitcoef[,,"a"]-mult*fitcoef[,,"a"],collapsedim = "coeff")
    
  } else {
    
    stop("Unknown value for mult_on: ",mult_on)
  }

  supplycurve_unshifted <- add_dimension(supplycurve_unshifted,dim=3.4,add = "curve",nm = "unshifted")
  supplycurve_shifted   <- add_dimension(supplycurve_shifted,  dim=3.4,add = "curve",nm = "shifted")
  
  supplycurve <- mbind(supplycurve_unshifted,supplycurve_shifted)
  
  #########################################################################
  ###################### Read MAgPIE points ###############################
  #########################################################################
  
  # demand
  dem_mag <- readAll(gdx_path,.readpar,name="p30_pebiolc_demandmag",asList=FALSE)
  # price
  pri_mag <- readAll(gdx_path,.readvar,name="p30_pebiolc_pricemag",asList=FALSE)
  
  dem_mag <- add_dimension(dem_mag,dim=3.2,add="sample",nm="1")
  dem_mag <- add_dimension(dem_mag,dim=3.3,add="variable",nm="x")
  pri_mag <- add_dimension(pri_mag,dim=3.2,add="sample",nm="1")
  pri_mag <- add_dimension(pri_mag,dim=3.3,add="variable",nm="y")
  
  mag_point <- mbind(dem_mag,pri_mag)
  
  getSets(mag_point) <- c("region","year","scenario","sample","variable")
  
  #########################################################################
  ######################### Convert units #################################
  #########################################################################
  
  supplycurve[,,"x"] <- supplycurve[,,"x"] * TWa2EJ
  supplycurve[,,"y"] <- supplycurve[,,"y"] * sm_tdptwyr2dpgj

  rem_point[,,"x"] <- rem_point[,,"x"] * TWa2EJ
  rem_point[,,"y"] <- rem_point[,,"y"] * sm_tdptwyr2dpgj

  mag_point[,,"x"] <- mag_point[,,"x"] * TWa2EJ
  mag_point[,,"y"] <- mag_point[,,"y"] * sm_tdptwyr2dpgj
  
  return(list(supplycurve=supplycurve,rem_point=rem_point,mag_point=mag_point))
  
}
