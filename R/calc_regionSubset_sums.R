#' Calculate Sums for Region Subsets
#' 
#' Sum up values in `data` for all sets of regions in `regionSubsetList`.
#'
#' @param data A [`MAgPIE`][magclass::magclass] object.
#' @param regionSubsetList A list of region subsets to calculate of the form 
#'   `list(subset_name = c(region_A, region_B, ...)`
#' @md
#' @return A [`MAgPIE`][magclass::magclass] object.
#' @author Michaja Pehl
#' 
#' @importFrom magclass mbind `getRegions<-` dimSums
#'
#' @examples
#' calc_regionSubset_sums(population_magpie, list(xAM = c('LAM', 'NAM')))

#' @export
calc_regionSubset_sums <- function(data, regionSubsetList) {
  mbind(
    lapply(
      names(regionSubsetList),
      
      function(subset_name) {
        `getRegions<-`(dimSums(data[regionSubsetList[[subset_name]]], dim = 1),
                       subset_name)
      }
    )
  )
}
