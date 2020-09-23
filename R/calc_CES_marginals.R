#' Calculate CES Marginals
#' 
#' Calculate marginals on the REMIND CES function and combine them to prices.
#' 
#' Marginals are calculated analytically
#' \deqn{\frac{\partial V_i}{\partial V_o} = \xi_i (\theta_i \delta_i)^{\rho_o} 
#'       {V_o}^{1 - \rho_o} {V_i}^{\rho_o - 1}}
#' and prices by recursively applying the chain rule
#' \deqn{\pi_i = \frac{\partial V_i}{\partial V_o} \pi_o 
#'       \quad \forall (i,o) \in CES}
#'
#' @md
#' @param gdxName Vector of paths to `.gdx` files.
#' @param id If several `.gdx` files are read, an id column is appended to the 
#'        result; either `file`, with the paths of the originating `.gdx` files,
#'        or `scenario`, with the content of `c_expname`.
#'
#' @return A `data frame` with columns `pf` (production factor), `t`, `regi`,
#'         `marginal`, `price`, and `file` (path to originating `.gdx` file).
#'         
#' @importFrom quitte read.gdx
#' @importFrom dplyr %>% left_join filter sym select rename mutate pull 
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom gdxrrw gdxInfo
#' @importFrom rlang is_empty

#' @export
calc_CES_marginals <- function(gdxName, id = 'file') {
  
  if (!id %in% c('file', 'scenario')) {
    warning('id must be either "file" or "scenario".  Defaulting to "file".')
    id <- 'file'
  }
  
  .calc_CES_marginals <- function(gdxName, id) {
    # ---- read required items from gdx ----
    pm_cesdata <- read.gdx(gdxName, 'pm_cesdata', factors = FALSE,
                           colNames = c('t', 'regi', 'pf', 'param', 'value'))
    
    vm_cesIO <- read.gdx(gdxName, 'vm_cesIO', colNames = c('t', 'regi', 'pf', 'value'),
                         factors = FALSE)
    
    cesOut2cesIn <- read.gdx(gdxName, 'cesOut2cesIn', colNames = c('pf.out', 'pf.in'),
                             factors = FALSE)
    
    # ---- calculate marginals ----
    marginals <- cesOut2cesIn %>% 
      left_join(
        pm_cesdata %>% 
          filter(2100 >= !!sym('t'),
                 !!sym('param') %in% c('xi', 'eff', 'effgr')) %>% 
          pivot_wider(names_from = 'param') %>% 
          drop_na(),
        
        c('pf.in' = 'pf')
      ) %>% 
      left_join(
        pm_cesdata %>% 
          filter('rho' == !!sym('param')) %>% 
          select(-'param', 'rho' = 'value'),
        
        c('t', 'regi', 'pf.out' = 'pf')
      ) %>% 
      left_join(
        vm_cesIO %>% 
          rename('value.in' = 'value'), 
        
        c('t', 'regi', 'pf.in' = 'pf')
      ) %>% 
      left_join(
        vm_cesIO %>% 
          rename('value.out' = 'value'),
        
        c('t', 'regi', 'pf.out' = 'pf')
      ) %>% 
      mutate(
        # ^ !!sym() doesn't work, so use the explicit function call
        !!sym('marginal') := !!sym('xi') 
                           * (!!sym('eff') * !!sym('effgr')) ^ (!!sym('rho'))
                           * `^`(!!sym('value.out'), !!sym('rho'))
                           * `^`(!!sym('value.in'), !!sym('rho') - 1))
    
    # ---- calculate prices recursively using the chain rule ----
    CES_root <- setdiff(cesOut2cesIn$pf.out, cesOut2cesIn$pf.in)
    
    prices <- marginals %>% 
      filter(!!sym('pf.out') %in% CES_root) %>% 
      select('pf' = 'pf.in', 't', 'regi', 'price' = 'marginal')
    
    CES_root <- cesOut2cesIn %>% 
      filter(!!sym('pf.out') %in% CES_root) %>% 
      pull('pf.in')
    
    while (!is_empty(CES_root)) {
      prices <- bind_rows(
        prices,
        
        marginals %>% 
          filter(!!sym('pf.out') %in% CES_root) %>% 
          select('pf' = 'pf.in', 't', 'regi', 'price' = 'marginal') %>% 
          left_join(cesOut2cesIn, c('pf' = 'pf.in')) %>% 
          left_join(
            prices %>% 
              rename('price.out' = 'price'), 
            
            c('t', 'regi', 'pf.out' = 'pf')
          ) %>% 
          mutate(!!sym('price') := !!sym('price') * !!sym('price.out')) %>% 
          select('pf', 't', 'regi', 'price')
      )
      
      CES_root <- cesOut2cesIn %>% 
        filter(!!sym('pf.out') %in% CES_root) %>% 
        pull('pf.in')
    }
    
    # ---- bind marginals and prices together ----
    r <- bind_rows(
      marginals %>% 
        select('pf' = 'pf.in', 't', 'regi', 'value' = 'marginal') %>% 
        mutate(!!sym('name') := 'marginal'),
      
      prices %>% 
        rename('value' = 'price') %>% 
        mutate(!!sym('name') := 'price')
    ) %>% 
      pivot_wider()
    
    if (id) {
      r <- r %>% 
        mutate(!!sym('scenario') := read.gdx(gdxName, 'c_expname', 
                                             colNames = 'c_expname',
                                             factors = FALSE) %>% 
                 pull('c_expname'))
    }
    
    return(r)
  }

  # ---- turn gdxInfo errors on gdx files into warnings and remove paths ----
  for (p in gdxName) {
    gdxName <- tryCatch(
      expr = {
        gdxInfo(p, dump = FALSE)
        gdxName
      },
      error = function(e) {
        warning(e)
        return(setdiff(gdxName, p))
      }
    )
  }
  
  # ---- bind results for all valid input files together ----
  r <- bind_rows(
    lapply(gdxName, function(gdxName) {
      .calc_CES_marginals(gdxName, id = id == 'scenario') %>% 
        mutate(file = gdxName)
    })
  )
  
  if ('file' != id) {
    r <- r %>% 
      select(-file)
  }
  
  return(r)
}
