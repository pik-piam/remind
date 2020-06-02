#' Get CES Total Efficiencies
#' 
#' Computes total efficiency 
#' \eqn{\alpha = \xi \left(\theta \delta \right)^\rho} from \code{pm_cesdata}.
#'
#' @param gdxName Path to \code{.gdx} file
#'
#' @return A \code{data.frame} with columns \code{t}, \code{regi}, \code{pf} 
#'     (production factor) and the \code{alpha} value.
#'     
#' @importFrom quitte read.gdx
#' @importFrom dplyr full_join inner_join sym filter select mutate
#' @importFrom tidyr spread
#' 
#' @author Michaja Pehl

#' @export
get_total_efficiencies <- function(gdxName) { 
  pm_cesdata <- read.gdx(gdxName, 'pm_cesdata', factors = FALSE, 
                         colNames = c('t', 'regi', 'pf', 'param', 'value'))
  
  pf_mapping <- read.gdx(gdxName, 'cesOut2cesIn', factors = FALSE, 
                         colNames = c('pf.out', 'pf.in'))
  
  full_join(
    inner_join(
      pf_mapping,
      
      pm_cesdata %>% 
        filter(!!sym('param') %in% c('xi', 'eff', 'effgr')) %>% 
        spread(!!sym('param'), !!sym('value')),
      
      c('pf.in' = 'pf')
    ),
    
    inner_join(
      pf_mapping,
      
      pm_cesdata %>% 
        filter('rho' == !!sym('param')) %>% 
        select(-!!sym('param'), rho = !!sym('value')),
      
      c('pf.out' = 'pf')
    ),
    
    c('pf.out', 'pf.in', 't', 'regi')
  ) %>% 
    mutate(!!sym('alpha') := !!sym('xi') * (!!sym('eff') * !!sym('effgr')) ^ !!sym('rho')) %>% 
    select('t', 'regi', pf = 'pf.in', 'alpha')
}
