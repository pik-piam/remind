#' function for copying REMIND GDX files
#' 
#' allows to copy a GDX file from place A to place B using the name with the
#' *.gz ending, regardless if it's zipped or not
#' 
#' 
#' @param from path to a zipped or unzipped GDX
#' @param to where the unzipped GDX will go and what name it will have
#' @author Anastasis Giannousakis
#' @seealso \code{\link{file.copy}}
#' @examples
#' 
#' \dontrun{gdx.copy(path_gdx, "config/input.gdx", overwrite=TRUE)
#' }
#' 
#' @export
gdx.copy <- function(from,to) {
  
  if(!from==""){
    if(file.exists(from)) {
    system(paste0("gzip -d -f ",from))
    from<-strsplit(from,split=".gz")[[1]]
      file.copy(from,to,overwrite=TRUE,recursive=TRUE)
    } else if(file.exists(strsplit(from,split=".gz")[[1]])) {
      file.copy(strsplit(from,split=".gz")[[1]],strsplit(to,split=".gz")[[1]],overwrite=TRUE,recursive=TRUE)
    }
  }
}
