#' Read Current Account from GDX file
#' 
#' Read Current Account data from a GDX file into a magpie object.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx
#' file(file name is recommended as this speeds up the code)
#' @author Jonas Hoersch
#' @examples
#' 
#' \dontrun{readCurrentAccount(gdx)}
#' 
#' @export
#' @importFrom gdx readGDX
readCurrentAccount <- function(gdx) {
  tmp <- readGDX(gdx, name='p80_curracc', format="first_found")
  # getNames(tmp) <- attr(tmp, "description") # removed due to compatibility
  # issues between Negishi and Nash (no description for current accont given in
  # Nash prior to version XXXX)
  return(tmp)
}
