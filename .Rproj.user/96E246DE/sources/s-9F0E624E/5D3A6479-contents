#' @title mipBarYearData
#' @description Function for plotting (bar-plot) MAgPIE objects and compare different scenarios, 
#' on the x-axis for some time steps one bar for each scenario is generated
#' 
#' 
#' @param x Data to plot. Allowed data formats: magpie or quitte
#' @param ylab y-axis text
#' @param xlab x-axis text
#' @param title title appering at the top of the plot
#' @param colour Dimension to be colored, default: "Scenario"
#' @author Lavinia Baumstark
#' @section Example Plot:
#' \if{html}{\figure{mipBarYearData.png}{example plot}}
#' \if{html}{\figure{mipBarYearData_oneRegi.png}{example plot}}
#' \if{html}{\figure{mipBarYearData_oneScenario.png}{example plot}}
#' 
#' @examples
#' 
#' \dontrun{
#'  plotCompBarYearData(EnInv, ylab="Energy Investments|Elec (billion US$2005/yr)",
#'                      colour=plotstyle(getNames(EnInv,dim=2)))
#' }
#'   
#' @importFrom magclass is.magpie
#' @importFrom ggplot2 ggplot aes_ guides guide_legend scale_x_continuous ggtitle
#' @importFrom dplyr %>% select_ distinct_ mutate_
#' @export
#


mipBarYearData <- function(x,colour=NULL,ylab=NULL,xlab=NULL,title=NULL){
  
  x <- as.quitte(x)
  
  if(length(unique(x$model)) > 1) {
    stop("this plot can only deal with data that have only one model")
  }
  
  # calculate ylab
  
  x$variable <- shorten_legend(x$variable,identical_only=TRUE)
  
  if (is.null(ylab)) {   
     ylab <- paste0(sub(".$","",attr(x$variable,"front")),attr(x$variable,"back"))
     # add unit
     unit <- unique(as.character(x$unit))
     ylab <- paste0(ylab, " (",paste0(unit,collapse=" | "),")")
  }
     
  # add dummy-dimension for space between the time-steps
  tmp <- x %>%   
         select_(~-scenario, ~-value) %>%  
         distinct_() %>%                  
         mutate_(scenario = ~"dummy",      
                value    = 0)
  x <- rbind(x,tmp)
  
  # delete the name of the dummy-bar for space between the time-steps
  xlabs = gsub("dummy.*", "", levels(interaction(x$scenario, x$period)))  
  
  # split in positive and negative part
  x_pos <- x
  x_neg <- x
  x_pos$value[x_pos$value<0]  <- 0
  x_neg$value[x_neg$value>=0] <- 0
  
  # make plot
  p <- ggplot(x, 
              aes_(x = ~as.numeric(interaction(scenario, period)),
                  y = ~value, 
                  fill = ~variable)) 
  if (any(x_pos$value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=x_pos,stat = "identity")
  if (any(x_neg$value <  0,na.rm=TRUE)) p <- p + geom_bar(data=x_neg,stat = "identity")
  p <- p + scale_x_continuous(breaks = 1:length(levels(interaction(x$scenario, x$period))),labels = xlabs) 
  p <- p + facet_wrap(~region, ncol=4, scales="free_y") 
  p <- p + theme(
                 legend.position="bottom",
                 axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5)) 
  p <- p + guides(fill = guide_legend(reverse = TRUE)) +
           guides(fill = guide_legend(ncol=2))
  if(!is.null(xlab)) { 
    p <- p + xlab(xlab) 
  } else {
    p <- p + xlab("")
  }
  if(!is.null(ylab))   { p <- p + ylab(ylab) }
  if(!is.null(title))  { p <- p + ggtitle(title) }
  if(is.null(colour)){
    colour <- plotstyle(as.character(x$variable))
  }
  p <- p + scale_fill_manual(values=colour)
  return(p)  
} 