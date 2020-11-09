#' Read in information form gdx and write convergenceCheck.pdf
#' 
#' Read in all information from gdx file and create
#' the convergenceCheck.pdf to check tax convergence
#' 
#' 
#' @param gdx GDX file
#' @param y time span for the data in line plots, default: y=c(seq(2005,2060,5),seq(2070,2100,10)) 
#' @param file a file name the output pdf
#' 
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{convergenceCheck(gdx="fulldata.gdx")}
#'
#'
#' @export
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom gdx readGDX
#' @importFrom luplot magpie2ggplot2
#' @importFrom grDevices colorRampPalette


convergenceCheck <- function(gdx,y=c(seq(2005,2060,5),seq(2070,2100,10)),file="convergenceCheck.pdf") {

##################### read in data ############################################
iter      <- readGDX(gdx,"cm_iteration_max")   # maximal number of iterations
iter      <- c(as.character(1:iter))
deltarev  <- readGDX(gdx,name=c("p21_deltarev","pm_deltarev"),format="first_found")[,,iter]

# sub parts of taxes
p21_taxrevGHG_iter        <- readGDX(gdx,c("p21_taxrevGHG_iter"),format="first_found")[,y,iter]
p21_taxrevCCS_iter        <- readGDX(gdx,c("p21_taxrevCCS_iter"),format="first_found")[,y,iter]
p21_taxrevNetNegEmi_iter  <- readGDX(gdx,c("p21_taxrevNetNegEmi_iter"),format="first_found")[,y,iter]
p21_taxrevFEtrans_iter    <- readGDX(gdx,c("p21_taxrevFEtrans_iter"),format="first_found")[,y,iter]
p21_taxrevFEBuildInd_iter <- readGDX(gdx,c("p21_taxrevFEBuildInd_iter"),format="first_found")[,y,iter]
p21_taxrevFE_Es_iter      <- readGDX(gdx,c("p21_taxrevFE_Es_iter"),format="first_found")[,y,iter]
p21_taxrevResEx_iter      <- readGDX(gdx,c("p21_taxrevResEx_iter"),format="first_found")[,y,iter]
p21_taxrevPE2SE_iter      <- readGDX(gdx,c("p21_taxrevPE2SE_iter"),format="first_found")[,y,iter]
p21_taxrevXport_iter      <- readGDX(gdx,c("p21_taxrevXport_iter"),format="first_found")[,y,iter]
p21_taxrevSO2_iter        <- readGDX(gdx,c("p21_taxrevSO2_iter"),format="first_found")[,y,iter]
p21_taxrevBio_iter        <- readGDX(gdx,c("p21_taxrevBio_iter"),format="first_found")[,y,iter]
p21_implicitDiscRate_iter <- readGDX(gdx,c("p21_implicitDiscRate_iter"),format="first_found")[,y,iter]
p21_taxrevFlex_iter       <- readGDX(gdx,c("p21_taxrevFlex_iter"),format="first_found")[,y,iter]
###############################################################################

################## open output-pdf ############################################
sw <- swopen("ConvergenceCheck.pdf")
swlatex(sw,"\\tableofcontents")
###############################################################################

##################### make plots ##############################################
col   <- colorRampPalette(c("red", "green"))(length(iter))
col_y <- colorRampPalette(c("blue", "yellow"))(length(y))

swlatex(sw,"\\subsection{deltarev}")
p <- magpie2ggplot2(deltarev,xaxis="Data1",ncol=3,ylab='deltarev',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(deltarev,xaxis="Data1",ncol=3,ylim=c(0,0.1),ylab='deltarev',xlab="iteration",show_grid=TRUE) # ,ylim=c(0,0.1)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevGHG}")
p <- magpie2ggplot2(p21_taxrevGHG_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevGHG_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevGHG_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevGHG_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevCCS}")
p <- magpie2ggplot2(p21_taxrevCCS_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevCCS_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevCCS_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevCCS_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevNetNegEmi}")
p <- magpie2ggplot2(p21_taxrevNetNegEmi_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevNetNegEmi_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevNetNegEmi_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevNetNegEmi_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevFEtrans}")
p <- magpie2ggplot2(p21_taxrevFEtrans_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevFEtrans_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevFEtrans_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevFEtrans_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevFEBuildInd}")
p <- magpie2ggplot2(p21_taxrevFEBuildInd_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevFEBuildInd_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevFEBuildInd_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevFEBuildInd_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevFE_Es}")
p <- magpie2ggplot2(p21_taxrevFE_Es_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevFE_Es_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevFE_Es_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevFE_Es_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevResEx}")
p <- magpie2ggplot2(p21_taxrevResEx_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevResEx_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevResEx_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevResEx_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevPE2SE}")
p <- magpie2ggplot2(p21_taxrevPE2SE_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevPE2SE_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevPE2SE_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevPE2SE_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevXport}")
p <- magpie2ggplot2(p21_taxrevXport_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevXport_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevXport_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevXport_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevSO2}")
p <- magpie2ggplot2(p21_taxrevSO2_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevSO2_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevSO2_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevSO2_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevBio}")
p <- magpie2ggplot2(p21_taxrevBio_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevBio_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevBio_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevBio_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{implicitDiscRate}")
p <- magpie2ggplot2(p21_implicitDiscRate_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_implicitDiscRate_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_implicitDiscRate_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_implicitDiscRate_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

swlatex(sw,"\\subsection{taxrevFlex}")
p <- magpie2ggplot2(p21_taxrevFlex_iter,xaxis="Data1",ncol=3,color="Year",color_pal=col_y,asDate=FALSE,
                    ylab='p21_taxrevFlex_iter',xlab="iteration",show_grid=TRUE)
swfigure(sw,print,p,sw_option="height=10,width=9")

p <- magpie2ggplot2(p21_taxrevFlex_iter,color="Data1",color_pal=col,ncol=3,group=NULL,
                    ylab='p21_taxrevFlex_iter',show_grid=TRUE,legend_ncol=4)
swfigure(sw,print,p,sw_option="height=10,width=9")

###############################################################################

################## close output-pdf ###########################################
cat("Generating PDF using knitr\n")
swclose(sw,outfile=file, clean_output=TRUE,engine="knitr",save_stream=FALSE)
###############################################################################
}
