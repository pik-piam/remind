#' Read in GDX and calculate capital stocks, used in convGDX2MIF.R for the reporting
#' 
#' Read in capital stock information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#' 
#' 
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @return MAgPIE object - contains the capital stock variables
#' @author Lavinia Baumstark; Michaja Pehl
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' 
#' \dontrun{reportCapitalStock(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getYears mbind setNames dimSums
#' @importFrom dplyr tribble
reportCapitalStock <- function(gdx,regionSubsetList=NULL) {

    module2realisation <- readGDX(gdx, "module2realisation", react = "silent")
    tran_mod = module2realisation[module2realisation$modules == "transport", 2]

    pm_conv_cap_2_MioLDV <- 650  # The world has ~715million cars in 2005 (IEA TECO2)
    
    # read sets
    teall2rlf    <- readGDX(gdx,name=c("te2rlf","teall2rlf"),format="first_found")  
    teue2rlf     <- readGDX(gdx,name=c("teue2rlf", "tees2rlf"),format="first_found")
    te           <- readGDX(gdx,name=c("te"),format="first_found")   
    # read variables
    vm_cap       <- readGDX(gdx,name=c("vm_cap"),field="l",format="first_found")
    vm_deltaCap  <- readGDX(gdx,name=c("vm_deltaCap"),field="l",format="first_found")
    v_investcost <- readGDX(gdx,name=c("vm_costTeCapital","v_costTeCapital","v_investcost"),field="l",format="first_found")
    vm_cesIO     <- readGDX(gdx, name = 'vm_cesIO', field = 'l')
    # read parameters
    ppfKap_Ind <- readGDX(gdx, name = 'ppfkap_industry_dyn37', react = 'silent')
    
    # calculate maximal temporal resolution
    y <- Reduce(intersect,list(getYears(vm_cap),getYears(v_investcost)))
    vm_cap       <- vm_cap[,y,]
    vm_deltaCap  <- vm_deltaCap[,y,]
    v_investcost <- v_investcost[,y,]
    
    # build reporting 
    tmp <- NULL

    if(tran_mod == "complex"){
        LDV35 <- readGDX(gdx,name=c("LDV35"),format="first_found")
        tmp <- mbind(tmp,setNames(dimSums( (vm_cap * v_investcost)[teue2rlf] 
                                         ,dim=c(3.1,3.2)) * 1000, "Est Capital Stock|ESM|Transp vehic (billion US$2005)"))
        tmp <- mbind(tmp,setNames(dimSums( (vm_cap * v_investcost)[teall2rlf][,,LDV35]                                    
                                         ,dim=c(3.1,3.2)) * 1000, "Est Capital Stock|ESM|Pet/EV LDV (billion US$2005)"))

        tmp <- mbind(tmp, 
                     setNames(
                         dimSums(mbind(vm_cap * v_investcost), 
                                 dim = 3) * 1000, 
                         "Estimated Capital Stock|ESM (billion US$2005)"
                     )
                     )
        
                                        # prepare variables
        vm_cap      <- vm_cap[teall2rlf]
        vm_deltaCap <- vm_deltaCap[teall2rlf]
        
                                        # build reporting
        tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,LDV35] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est LDV Stock (million vehicles)"))
        tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"apCarElT"] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est EV LDV Stock (million vehicles)"))
        tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"apCarH2T"] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est H2 LDV Stock (million vehicles)"))
        tmp <- mbind(tmp,setNames(dimSums(vm_cap[,,"apCarPeT"] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est ICE LDV Stock (million vehicles)"))
        
        tmp <- mbind(tmp,setNames(dimSums(vm_deltaCap[,,LDV35] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est LDV Sales (million vehicles)"))
        tmp <- mbind(tmp,setNames(dimSums(vm_deltaCap[,,"apCarElT"] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est EV LDV Sales (million vehicles)"))
        tmp <- mbind(tmp,setNames(dimSums(vm_deltaCap[,,"apCarH2T"] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est H2 LDV Sales (million vehicles)"))
        tmp <- mbind(tmp,setNames(dimSums(vm_deltaCap[,,"apCarPeT"] * pm_conv_cap_2_MioLDV,dim=c(3.1,3.2)),"Est ICE LDV Sales (million vehicles)"))


        ## add global values
        tmp <- mbind(tmp,dimSums(tmp,dim=1))
        ## add other region aggregations
        if (!is.null(regionSubsetList)){
            tmp <- mbind(tmp,do.call("mbind",lapply(names(regionSubsetList),
                                                    function(x) { result <- dimSums(tmp[regionSubsetList[[x]],,],dim=1)
                                                        getRegions(result) <- x
                                                        return(result) })))
    
        }
    }
    
    ## report industry energy efficiency capital stocks
    if (!is.null(ppfKap_Ind) & 0 < length(ppfKap_Ind)) {
      mixer <- tribble(
        ~pf,                     ~name,
        'kap_cement',            'Cement',
        'kap_chemicals',         'Chemicals',
        'kap_steel_primary',     'Primary Steel',
        'kap_steel_secondary',   'Secondary Steel',
        'kap_otherInd',          'other')
      
      if (0 != length(setdiff(ppfKap_Ind, mixer$pf))) {
        warning(paste('Unknown ppfKap_industry_dyn37 entity.',
                      'Adjust remind::reportCapitalStock()'))
      }
      
      if (0 != length(setdiff(mixer$pf, ppfKap_Ind))) {
        warning(paste('Missing ppfKap_industry_dyn37 entity.',
                      'Adjust remind::reportCapitalStock()'))
      } 
      
      eek_Ind <- setNames(vm_cesIO[,y,ppfKap_Ind],
                          paste0('Capital|Energy Efficiency|Industry|', 
                                 mixer[mixer$pf %in% ppfKap_Ind,][['name']],
                                 ' (billion US$2005)'))
      # add industry EEK and global totals
      tmp <- mbind(tmp, mbind(eek_Ind, dimSums(eek_Ind, dim = 1)))
    }
             
    return(tmp)
}
