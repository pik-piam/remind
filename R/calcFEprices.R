# calcFEprices
# Calculates FE prices from REMIND gdx
# input:  gdx path or object, as used with readGDX()
# output: magclass object with reporting variables



#' Read in GDX and calculate FE prices, used in convGDX2MIF.R for the
#' reporting.
#' 
#' Read in GDX and calculate FE prices, used in convGDX2MIF.R for the
#' reporting.
#' 
#' 
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @author Michaja Pehl
#' @examples
#' 
#' \dontrun{calcFEprices(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass getNames<- dimSums mbind
calcFEprices <- function(gdx) {

    # FIXME: replace with library-wide value/mechanism
    TWa_2_EJ     <- 31.536
    
    # Marginal of the budget equation to scale prices
    qm_budget.m  <- readGDX(gdx, "qm_budget",  field = "m", 
                            restore_zeros = FALSE)
    
    # simple terms in q_balfinen
    list(
        fesos = "Price|Final Energy|Solids; US$2005/GJ;",
        fehos = "Price|Final Energy|Heating Oil; US$2005/GJ;", 
        fegas = "Price|Final Energy|Gases; US$2005/GJ;", 
        feh2s = "Price|Final Energy|Hydrogen|Stationary; US$2005/GJ;", 
        fehes = "Price|Final Energy|Heat; US$2005/GJ;", 
        feels = "Price|Final Energy|Electricity|Stationary; US$2005/GJ;"
    ) -> simple.q_balfinen.terms
    
    # load q_balfinen.m
    q_balfinen.m <- readGDX(gdx, name=c("qm_balFeForCes","q_balfinen"), field = "m", restore_zeros = FALSE,format="first_found")
    
    # collapse q_balfinen.m dimension names from "x.x" to "x"
    sub(".*\\.", "", magclass::getNames(q_balfinen.m)) -> magclass::getNames(q_balfinen.m)
    
    # calculate prices
    abs(q_balfinen.m[,,names(simple.q_balfinen.terms)] / (qm_budget.m + 1e-10) * 
            1000 / TWa_2_EJ) -> q_balfinen.p
    
    # change item names
    unlist(simple.q_balfinen.terms, use.names = FALSE) -> magclass::getNames(q_balfinen.p)
    
    
    # simple terms in q_febal
    list(
        fepet = "Price|Final Energy|Petrol; US$2005/GJ;", 
        fedie = "Price|Final Energy|Diesel; US$2005/GJ;", 
        feh2t = "Price|Final Energy|Hydrogen|Transport; US$2005/GJ;", 
        feelt = "Price|Final Energy|Electricity|Transport; US$2005/GJ;"
    ) -> simple.q_febal.terms
    
    # load q_febal.m
    q_febal.m <- readGDX(gdx, c("q_balFe","q_febal"), field = "m", restore_zeros = FALSE ,format="first_found")
    
    # collapse q_febal.m dimension names from "x.x" to "x"
    sub(".*\\.", "", magclass::getNames(q_febal.m)) -> magclass::getNames(q_febal.m)
    
    # calculate prices
    abs(q_febal.m[,,names(simple.q_febal.terms)] / (qm_budget.m + 1e-10) * 
            1000 / TWa_2_EJ) -> q_febal.p
    
    # change item names 
    unlist(simple.q_febal.terms, use.names = FALSE) -> magclass::getNames(q_febal.p)
    
    
    # compounds
    
    # load vm_prodFe and filter items
    vm_prodFe.l <- readGDX(gdx, "vm_prodFe", field = "l", restore_zeros = FALSE)
    vm_prodFe.l[,,c("fepet", "fedie", "fehos")] -> vm_prodFe.l
    
    # change item names
    c("fepet", "fedie", "fehos") -> magclass::getNames(vm_prodFe.l)
    
    # calculate
    ( dimSums(
        abs(
            mbind(
                q_febal.m[,,c("fepet", "fedie")] * vm_prodFe.l[,,c("fepet", "fedie")],
                q_balfinen.m[,,"fehos"] * vm_prodFe.l[,,"fehos"])),
        dims = 3
    ) 
    / dimSums(vm_prodFe.l, dims = 3)
    / (abs(dimSums(qm_budget.m, dims = 3)) + 1e-10)
    * 1000
    / TWa_2_EJ
    ) -> liquids.p
    
    magclass::getNames(liquids.p) <- "Price|Final Energy|Liquids; US$2005/GJ;"
    
    mbind(
        q_balfinen.p,
        q_febal.p,
        liquids.p
    )
}
