#' @title Create REMIND Diagnosis variables and plots
#' @description Create REMIND run diagnosis variables and plots
#'
#'
#' @param gdx GDX file
#' @param statsFile run statistics file
#' @param chartType plot type to include in output object. Either "plotly", "ggplot" or both c("plotly","ggplot")
#' @param includeData boolean to include diagnosis data in output (default: FALSE)
#' 
#' @author Renato Rodrigues
#' 
#' @examples
#'
#'   \dontrun{
#'     reportDiagnosis(gdx="fulldata.gdx",statsFile="runstatistics.rda")
#'   }
#'
#' @importFrom gdx readGDX
#' @importFrom lubridate seconds_to_period day hour minute second
#' @importFrom dplyr bind_rows summarise_ group_by_ mutate_ mutate filter_
#' @importFrom tidyr %>%
#' @importFrom quitte as.quitte
#' @importFrom ggplot2 ggplot geom_line scale_fill_manual scale_y_discrete geom_rect geom_hline scale_x_continuous coord_cartesian coord_flip geom_bar geom_text position_stack element_blank geom_boxplot aes_
#' @importFrom plotly ggplotly config hide_legend subplot layout style
#' @importFrom reshape2 dcast
#' @importFrom stats quantile IQR lag
#' @importFrom grDevices rgb
#' 
#' @export

reportDiagnosis <- function(gdx=NULL,statsFile=NULL,chartType="ggplot",includeData=FALSE) {

  # error checking
  if(!(!(is.null(gdx)) && file.exists(gdx))){
    warning("reportDiagnosis function require a valid gdx file path.")
    return()
  }
  if(!(!(is.null(statsFile)) && file.exists(statsFile))){
    warning("reportDiagnosis function require a valid stats file.")
    return()
  } 
  
  ### Aestethics Options 
  aestethics <- list("alpha"=0.6,
                     "line" = list("size"= 2/3.78),
                     "point" = list("size"= 2/3.78)
  )
  
  missingColors <- c(
    "DEU"="#7F2704",
    "EUW"="#FC4E2A", "EWN"="#FC4E2A", "FRA"="#E31A1C",                 
    "EUS"="#FFEDA0", "ESW"="#FFEDA0", "ESC"=brewer.pal(9,"YlOrRd")[3],  
    "EUC"="#969696",  "ECS"="#D9D9D9",  "ECE"="#969696",
    "EUN"="#4292C6",  "ENC"="#6BAED6",  "UKI"="#4292C6",
    "NEU"="#78C679",   "NEN"="#78C679",   "NES"="#D9F0A3",
    "CHE"="#78C679",   "ENN"="#78C679",   "ESE"="#D9F0A3","EUI"="#78C679",   "ROE"="#D9F0A3", #older EU
    "SSA"="#00BAFF", "REF"="#D900BC", "CAZ"="#007362", "CHA"="#F24200", 
    "Uranium"="#EF7676", "Goods"= "#00BFC4",
    "optimal"="#00BFC4","feasible"="#ffcc66","infeasible"="#F8766D",
    "yes"="#00BFC4","no"="#F8766D",
    "optimal_alt"="#00BFC4", "feasible_alt"="#ffcc66")
  missingColorsdf <- data.frame(row.names=names(missingColors), color=missingColors)
  
  plotlyButtonsToHide <- list('sendDataToCloud', 'zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'zoom3d', 'pan3d', 'orbitRotation', 'tableRotation', 'resetCameraDefault3d', 'resetCameraLastSave3d', 'hoverClosest3d', 'zoomInGeo', 'zoomOutGeo', 'resetGeo', 'hoverClosestGeo', 'hoverClosestGl2d', 'hoverClosestPie', 'resetSankeyGroup', 'toggleHover', 'resetViews', 'toggleSpikelines', 'resetViewMapbox')
  
  ### Auxiliar functions
  
  #Format time duration in seconds
  format_duration <- function(dur,type="long") {
    dur <- seconds_to_period(round(dur))
    if(type == "short") {
      return(as.character(dur))
    } else { # long format
      funs <- list(day, hour, minute, second)
      names(funs) <- c("day", "hour", "minute", "second")
      dur <- sapply(names(funs), function(x){ ifelse(funs[[x]](dur),paste0(funs[[x]](dur), " ", x, ifelse(funs[[x]](dur)>1, "s", ""), ""),"")})
      if(is.null(dim(dur)))
        return (sub(",([^,]*)$", " and\\1", paste(dur[dur!=""], collapse = ', ')))
      else {
        out <- NULL
        for (i in 1:length(dur[,1])) { 
          x <- dur[i,]
          out <- c(out, sub(",([^,]*)$", " and\\1", paste(x[x!=""], collapse = ', ')))
        }
        return(out)
      }
    }
  }
  
  
  #create diagnosis object
  diag <- list()
  
  ### Pre-loop errors
  
  # model did not created output
  if(!file.exists(gdx)) {
    diag$summary$status <- "fulldata.gdx file not found!"
    return (diag)
  }
  
  # Run failed before nash iterations
  modelstat <- readGDX(gdx, name = "o_modelstat")[[1]]
  if (!(modelstat %in% c(1,2,3,4,5,6,7))){
    diag$summary$status <- "Run failed - Check code, pre-triangular infes ..."
    return (diag)
  }
  
  # TODO: check if model finished properly (full.log and log.txt files) -> "REMIND run finished!",logfile / "Stop" / "Run stopped - Cluster problem, HWCL exceeded ..." 
  
  ### Model diagnosis 
  
  # it converged?
  diag$data$lastIteration <- readGDX(gdx, name = "o_iterationNumber")[[1]] 
  diag$data$maxIterations <- readGDX(gdx, name = "cm_iteration_max")[[1]]
  
  if(diag$data$lastIteration == diag$data$maxIterations) { # check if run converged before iterations limit
    diag$summary$status <- "Iteration Limit"
    diag$summary$statusMessage <- paste0("The solver did NOT converge within the maximum number of iterations allowed!")
  } else { # check run convergence status
    status <- list("1" = list("optimal", paste0("The solver converged after ", diag$data$lastIteration, " iterations")), 
                "2" = list("locally optimal", paste0("The solver converged after ", diag$data$lastIteration, " iterations")),
                "3" = list("unbounded", "The model is unbounded!"), 
                "4" = list("infeasible", "The model is infeasible!"),
                "5" = list("locally infeasible", "The model is locally infeasible!"),
                "6" = list("intermediate infeasible","The model is intermediate infeasible!"),
                "7" = list("intermediate nonoptimal",paste0("The solver found  an intermediate nonoptimal solution after ", diag$data$lastIteration, " iterations"))) # ignored by REMIND in the o_modelstat parameter as it would imply a solution if the initial or any intermediate point is optimal  
    diag$summary$status <- status[[modelstat]][[1]]
    diag$summary$statusMessage <- status[[modelstat]][[2]]
  }
  
  #load runstatistics.rda
  stats <- NULL
  load(file = statsFile)
  diag$data$runStatistics <- stats
  
  # run time
  diag$data$runTime$seconds <- round(ifelse(attr(diag$data$runStatistics$runtime,'units')=="hours", as.numeric(diag$data$runStatistics$runtime*60*60), as.numeric(diag$data$runStatistics$runtime*60))) 
  diag$data$runTime$formated <- format_duration(diag$data$runTime$seconds,type="short") # short formated run time
  diag$data$runTime$longFormat <- format_duration(diag$data$runTime$seconds) # long formated run time
  
  
  ### Iteration details 
  
  p80_repy_iteration <- as.quitte(readGDX(gdx, name = "p80_repy_iteration",restore_zeros = FALSE))[,c("solveinfo80","region","iteration","value")]
  p80_repy_wide <- dcast(p80_repy_iteration, region + iteration ~ solveinfo80, value.var="value")
  
  # adding columns with per region objective difference between iterations (diff.objval) and convergence condition for objective value divergence 
  p80_repy_wide <- p80_repy_wide %>%
    group_by_(~region) %>%
    mutate_(diff.objval = ~ objval - lag(objval, order_by=iteration),
           objvalCondition = ~ ifelse(modelstat=="2",T,
                                    ifelse(modelstat=="7" & is.na(diff.objval), F, 
                                            ifelse(modelstat=="7" & abs(diff.objval) < 1e-4, T, F))))
  
  # adding column with per iteration convergence for objective value divergence (objvalConverge) - if any region did not converged in the difference between iteration objectives, then objvalConverge will be false for all regions in the respective iteration  
  p80_repy_wide <- p80_repy_wide %>%
    group_by_(~iteration) %>%
    mutate_(objvalConverge = ~all(objvalCondition))
  
  # adding column with literal string for model convergence (convergence). values: optimal, feasible or infeasible
  p80_repy_wide$convergence <- "infeasible"
  p80_repy_wide[(p80_repy_wide$modelstat == 1 & p80_repy_wide$solvestat == 1),"convergence"] <-  "optimal"
  p80_repy_wide[(p80_repy_wide$modelstat == 2 & p80_repy_wide$solvestat == 1),"convergence"] <-  "optimal"
  p80_repy_wide[(p80_repy_wide$modelstat == 7 & p80_repy_wide$solvestat == 4),"convergence"] <-  "feasible"
  
  # saving convergence table
  diag$data$summaryTable <- p80_repy_wide
  
  
  ### Solver convergence plots 
  
  ### solver status summary plot
  
  # forcing all convergence levels to be present
  diag$data$summaryTable$convergence <- factor(diag$data$summaryTable$convergence,levels=c("infeasible", "feasible", "optimal"))
  
  # creating tooltip text (plotly) 
  data <- diag$data$summaryTable %>% 
    group_by_(~iteration, ~convergence) %>%
    mutate_(details = ~paste0("Iteration: ", iteration,"<br>region: ",paste0(region, collapse = ", ")))
  
  # solver status summary
  optColor <- plotstyle(as.character(unique(data$convergence)),unknown=missingColorsdf)
  regColor <- plotstyle(as.character(unique(data$region)),unknown=missingColorsdf)
  
  diag$plots$convergence <- ggplot(mapping = aes_(~iteration, ~convergence, text=~details))+
      geom_line(data = data, linetype = "dashed",aes_(group=~region, color=~region), alpha=aestethics$alpha, size=aestethics$line$size) +
      geom_point(data = data %>% group_by_(~iteration, ~convergence, ~details) %>% summarise_(), aes_(fill=~convergence), size=2, alpha=aestethics$alpha) +
      scale_fill_manual(values=optColor) +
      scale_color_manual(values=regColor) +
      scale_y_discrete(breaks=c("infeasible", "feasible", "optimal"), drop = FALSE) +
      theme_minimal() +
      labs(x = NULL, y = NULL)
  
  ### Trade balance plots 
  
  ### Trade convergence
  surplus <- as.quitte(readGDX(gdx, name = "p80_surplus",restore_zeros = F)[,c(2100,2150),])[c(6,7,8,9)] # c("peoil","pegas","pecoal","peur","pebiolc","good","perm")
  surplus$value[is.na(surplus$value)] <- 0
  surplus$type <- ifelse(surplus$all_enty == "good", "Goods trade surplus", ifelse(surplus$all_enty == "perm", "Permits", "Primary energy trade surplus"))
  
  maxTol <- as.quitte(readGDX(gdx, name = "p80_surplusMaxTolerance",restore_zeros = F))[c(7,8)]
  colnames(maxTol) <- c("maxTol","all_enty")
  surplus <- merge(surplus,maxTol,by = "all_enty")
  surplus[which(surplus$period==2150),]$maxTol <- surplus[which(surplus$period==2150),]$maxTol*10
  surplus$rectXmin <- as.numeric(surplus$iteration) - 0.5
  surplus$rectXmax <- as.numeric(surplus$iteration) + 0.5
  surplus$Surplus_within_limits <- ifelse(surplus$value > surplus$maxTol,"no", ifelse(surplus$value < -surplus$maxTol, "no", "yes"))
  
  diag$data$surplus <- surplus
  
  # filter surplus results
  maxTol <- surplus %>% group_by_(~type, ~period, ~iteration) %>% mutate_(Surplus_within_limits = ~ifelse(all(Surplus_within_limits=="yes"),"yes","no"))
  maxTol <- maxTol[which(surplus$all_enty %in% c("peoil","good","perm")),][c(-1)]
  
  vars <- c("pecoal"="Coal","pegas"="Gas","peoil"="Oil","peur"="Uranium","good"="Goods","pebiolc"="Biomass")
  surplus$name <- vars[surplus$all_enty]
  
  booleanColor <- plotstyle(as.character(unique(maxTol$Surplus_within_limits)),unknown=missingColorsdf)
  surplusColor <- plotstyle(vars,unknown=missingColorsdf)
  names(surplusColor) <- names(vars)
           
  surplus$tooltip <- paste0(ifelse(surplus$Surplus_within_limits=="no",
                              ifelse(surplus$value > surplus$maxTol, 
                                     paste0(surplus$name," trade surplus (", surplus$value, ") is greater than maximum tolerance (", surplus$maxTol, ")."),
                                     paste0(surplus$name," trade surplus (", surplus$value, ") is lower than maximum tolerance (-", surplus$maxTol, ").")),
                              paste0(surplus$type," is within tolerance.")),
                            "<br>Iteration: ", surplus$iteration)
 
  maxTol$tooltip <- paste0(maxTol$type, ifelse(maxTol$Surplus_within_limits=="no", " outside tolerance limits.", " within tolerance limits."))
  
  diag$plots$surplusConvergence <- suppressWarnings( ggplot() +
    geom_line(data=surplus, aes_(x=~iteration, y=~value, color=~all_enty, group=~all_enty, text = ~tooltip), alpha=aestethics$alpha, size=aestethics$line$size) + 
    geom_rect(data=maxTol,aes_(xmin=~rectXmin, xmax=~rectXmax, ymin=~-maxTol, ymax=~maxTol, fill=~Surplus_within_limits, text = ~tooltip), inherit.aes = FALSE, alpha=aestethics$alpha) +
    theme_minimal() +
    ggtitle("Tradable goods surplus") +
    facet_grid(type ~ period, scales = 'free_y') +
    scale_color_manual(values=surplusColor) +
    scale_fill_manual(values=booleanColor) +
    labs(x = NULL, y = NULL) ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  diag$plotly$surplusConvergence <- ggplotly(diag$plots$surplusConvergence, tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
  
  ### Trade convergence Summary
  
  surplusCondition <- surplus %>% group_by_(~iteration) %>% summarise_(Surplus_within_limits = ~ifelse(all(Surplus_within_limits=="yes"),"yes","no")) 
  
  surplusCondition$tooltip <- paste0("Iteration: ", surplusCondition$iteration, "<br>Converged")
  for(iter in surplusCondition$iteration){
    if(all(surplusCondition[which(surplusCondition$iteration == iter),]$Surplus_within_limits == "no")){
      tooltip <- NULL
      for (period in unique(surplus$period)){
        for (good in unique(surplus$all_enty)){
          currSurplus <- surplus[which(surplus$iteration == iter & surplus$period == period & surplus$all_enty == good),]
          Surplus_within_limits <- ifelse(currSurplus$value > currSurplus$maxTol,"no", ifelse(currSurplus$value < -currSurplus$maxTol, "no", "yes"))
          if(Surplus_within_limits == "no"){
            tooltip <- paste0(tooltip,"<br> ", period, " | ", good, " | ", ifelse(currSurplus$value > currSurplus$maxTol, paste0(round(currSurplus$value,5), " > ",  currSurplus$maxTol), paste0(round(currSurplus$value,5), " < ",  -currSurplus$maxTol)))
          }
        }
      }
      tooltip <- paste0("Iteration: ", iter, "<br>Did not converged!",
                        "<br>Period | Trade | Surplus", tooltip)
      surplusCondition[which(surplusCondition$iteration == iter),]$tooltip <- tooltip
    }
  }
  
  diag$plots$surplusSummary <- ggplot(surplusCondition, aes_(x = ~iteration, y="Trade\nSurplus", fill=~Surplus_within_limits, text = ~tooltip)) +
    geom_hline(yintercept=0) +
    theme_minimal() +
    geom_point(size=2,alpha=aestethics$alpha) +
    scale_fill_manual(values=booleanColor) +
    scale_y_discrete(breaks=c("Trade\nSurplus"), drop = FALSE) +
    labs(x = NULL, y = NULL)
  
  ### Objective function variation convergence plots 
  
  ### Objective function variation condition. If the convergence condition is not respected, then the axis labels are red
  data <- p80_repy_wide
  
  objVarCondition <- data[which(data$region == data$region[1]),]$objvalConverge 
  
  axisColor <- ifelse(objVarCondition,"black", "red")
  
  diag$plots$objVar <- ggplot(data, aes_(x = ~iteration)) +
    geom_point(aes_(y = ~diff.objval, col=~region),alpha=aestethics$alpha) +
    theme_minimal() +
    theme(axis.text.x = element_text(colour = axisColor))
  
  ### Summary
  objVarCondition <- p80_repy_wide[which(p80_repy_wide$region == p80_repy_wide$region[1]),]$objvalConverge 
  data <- data.frame(iteration = p80_repy_wide[which(p80_repy_wide$region == p80_repy_wide$region[1]),]$iteration, objVarCondition = ifelse(objVarCondition,"yes","no"))
  
  data$tooltip <- paste0("Iteration: ", data$iteration, "<br>Converged")
  for(iter in unique(data$iteration)){
    if(!all(p80_repy_wide[which(p80_repy_wide$iteration == iter),]$objvalCondition)){
      tooltip <- NULL
      for (reg in p80_repy_wide[which(p80_repy_wide$iteration == iter & p80_repy_wide$objvalCondition == F),]$region){
        
        diff <- p80_repy_wide[which(p80_repy_wide$iteration == iter & p80_repy_wide$region == reg),]$diff.objval
        
        tooltip <- paste0(tooltip,"<br> ", reg, "   |     ", round(diff,5))
      }
      tooltip <- paste0("Iteration: ", iter, "<br>Did not converged!",
                        "<br>Region | Deviation", tooltip,"<br>The deviation limit is +- 0.0001")
      data[which(data$iteration == iter),]$tooltip <- tooltip
    }
  }
  
  diag$plots$objVarSummary <- ggplot(data, aes_(x = ~iteration, y="Objective\nDeviation", fill=~objVarCondition, text=~tooltip)) +
    geom_hline(yintercept=0) +  
    theme_minimal() +
    geom_point(size=2,alpha=aestethics$alpha) +
    scale_fill_manual(values=booleanColor) +
    scale_y_discrete(breaks=c("Objective\nDeviation"), drop = FALSE) +
    labs(x = NULL, y = NULL)
  
  
  ### Price anticipation convergence plots 
  
  diag$data$priceAntecipationFadeoutIteration <- as.vector(readGDX(gdx, name = "s80_fadeoutPriceAnticipStartingPeriod"))
  
  data <- data.frame(iteration = 1:diag$data$lastIteration)
  
  data <- data %>% mutate_(fadeoutPriceAnticip = ~ifelse(iteration < diag$data$priceAntecipationFadeoutIteration, 1,0.7**(iteration - diag$data$priceAntecipationFadeoutIteration + 1)),
                          converged = ~ifelse(fadeoutPriceAnticip > 1e-4, "no", "yes"),
                          tooltip = ~ifelse(converged == "yes", paste0("Converged<br>Price Anticipation fade out is low enough<br>", round(fadeoutPriceAnticip,5), " <= 0.0001"), 
                                           paste0("Did not converged<br>Price Anticipation fade out is not low enough<br>", round(fadeoutPriceAnticip,5), " > 0.0001")))
  
  
  diag$plots$priceAnticipation <- suppressWarnings( ggplot(data, aes_(x = ~iteration)) +
    geom_line(aes_(y=~fadeoutPriceAnticip), alpha=0.3, size=aestethics$line$size) +
    geom_point(size=2, aes_(y=0.0001, fill=~converged, text = ~tooltip), alpha=aestethics$alpha) +
    theme_minimal() +
    scale_fill_manual(values=booleanColor) +
    scale_y_continuous(breaks=c(0.0001), labels=c("Price\nAnticipation")) +
    scale_x_continuous(breaks=c(data$iteration)) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(ylim=c(-0.2, 1))
  )
  
  ### Summary plot 
  
  diag$plotly$'Convergence Report' <- subplot(ggplotly(diag$plots$convergence, tooltip = c("text")),
                                              ggplotly(diag$plots$surplusSummary, tooltip = c("text")),
                                              ggplotly(diag$plots$objVarSummary, tooltip = c("text")),
                                              ggplotly(diag$plots$priceAnticipation, tooltip = c("text")),
                                              nrows = 4, shareX = TRUE, titleX = FALSE, heights = c(0.4, 0.2, 0.2, 0.2), margin = c(.1,.1,.1,.0001)) %>%
                                        hide_legend() %>%
                                        config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE) %>%
                                        layout(margin=list(l=-100, r=10))
  
  diag$legend$'Convergence Report'$description <- "<p>Conditions to REMIND convergence.<br>Convergence is only achieved if all conditions are met.</p><br><ul><li>Condition one: each region must be optimal, or at most feasible in a latter iteration.</li><li>Condition two: market clearing for all tradable goods.</li><li>Condition three: stable objective function value for all regions.</li><li>Condition four: price anticipation slack must fade out.</li></ul>"
  diag$legend$'Convergence Report'$contents <- list("Convergence criteria met"=list("fill"=plotstyle("optimal",unknown=missingColorsdf),"linetype"=NULL),
                                                    "Partial convergence target met"=list("fill"=plotstyle("feasible",unknown=missingColorsdf),"linetype"=NULL),
                                                    "Not converged"=list("fill"=plotstyle("infeasible",unknown=missingColorsdf),"linetype"=NULL))
  
  ### Time convergence plots 
  
  # total convergence time per region and convergence type (value)
  cumConvergenceTime <- diag$data$summaryTable %>% 
    group_by_(~region, ~convergence) %>%
    summarise_(value = ~sum(resusd))
  
  # total convergence time per region (total)
  cumConvergenceTime <- cumConvergenceTime %>%
    group_by_(~region) %>%
    mutate_(total = ~sum(value))
  
  # tooltip text (plotly) 
  cumConvergenceTime$Details <- paste0("<br>Region: ",cumConvergenceTime$region, "<br>Duration: ", format_duration(cumConvergenceTime$value), "<br>Convergence: ", cumConvergenceTime$convergence, "<br>Total Duration: ", format_duration(cumConvergenceTime$total)) 
  
  # Slowest iteration per region and convergence type 
  SlowestIteration <- diag$data$summaryTable %>% 
    group_by_(~region, ~convergence) %>% 
    filter_(~ max(resusd) == resusd) %>% 
    summarise_(value = ~resusd, iteration = ~iteration)
  
  # tooltip text (plotly) 
  SlowestIteration$Details <- paste0("<br>Region: ",SlowestIteration$region, "<br>Duration: ", format_duration(SlowestIteration$value), "<br>Iteration: ", SlowestIteration$iteration, "<br>Convergence: ", SlowestIteration$convergence) 
  
  ### cumulative convergence time in minutes
  diag$plots$cumConvergenceTime <- suppressWarnings( ggplot() +
    geom_col(data = cumConvergenceTime, aes_(x = ~region, y = ~value/60, fill = ~convergence, text = ~Details), alpha=aestethics$alpha) +
    theme_minimal() +
    coord_flip(expand = FALSE) +
    labs(x = NULL, y = NULL, title = "") +
    theme(legend.position = 'bottom') + 
    ggtitle("Cumulative convergence time in minutes")) 
  
  ### Slowest iteration per region and convergence type 
  diag$plots$SlowestIteration <- ggplot(data = SlowestIteration, aes_(x = ~region, y = ~value/60, fill = ~convergence, text = ~Details)) +
    geom_bar(stat = "identity", alpha=aestethics$alpha) +
    geom_text(aes_(label=~iteration), position=position_stack(vjust = 1), hjust = 1, size = 3) +
    theme_minimal() +
    coord_flip(expand = FALSE) +
    labs(x = NULL, y = NULL, title = "") +
    theme(legend.position = 'bottom') + 
    ggtitle("Slowest iteration per region and convergence type in minutes") 
  
  ### cumulative convergence and slowest iteration charts merged
  data <- bind_rows(cumConvergenceTime %>% mutate(variable = "Cumulative convergence time in minutes"),
                  SlowestIteration %>% mutate(variable = "Slowest iteration per region and convergence type in minutes"))
  
  convergenceTypeColor <- plotstyle(paste0(as.character(unique(data$convergence))),unknown=missingColorsdf)
  names(convergenceTypeColor) <- as.character(unique(data$convergence))
    
  diag$plots$iterationSummary <- ggplot(data=data,aes_(x = ~region, y = ~value/60, fill = ~convergence, text=~Details)) +
    geom_bar(stat = "identity", alpha=aestethics$alpha) +
    geom_text(aes_(label=~iteration), position=position_stack(vjust = 1), hjust = 1, size = 3) +
    theme_minimal() +
    scale_fill_manual(values=convergenceTypeColor) +
    coord_flip(expand = FALSE) +
    labs(x = NULL, y = NULL, title = "") +
    theme(legend.position = 'bottom', legend.title=element_blank()) +
    facet_wrap(~variable, ncol = 1, scales = 'free_x')
  
  diag$plotly$'Summary' <- ggplotly(diag$plots$iterationSummary, tooltip = c("text")) %>%
                              hide_legend() %>%
                              config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE) %>%
                              layout(legend = list(orientation = "h", x = 0.4, y =-0.05)) %>%
                              style(textposition = "left")
  
  diag$legend$'Summary'$description <- "<p>Total solution time taken by each region and time of the slowest iteration per region and optimality type.</p>"
  diag$legend$'Summary'$contents <- list("Optimal solution"=list("fill"=plotstyle("optimal",unknown=missingColorsdf),"linetype"=NULL),
                                         "Feasible solution"=list("fill"=plotstyle("feasible",unknown=missingColorsdf),"linetype"=NULL))
  diag$legend$'Summary'$units <- "minutes"
  
  ### Detailed convergence time
  data <- diag$data$summaryTable
  
  data$Details <- paste0("<br>Iteration: ",data$iteration, "<br>Duration: ", format_duration(data$resusd), "<br>Convergence: ", data$convergence, "<br>Region: ",data$region) 
   
  diag$plots$iterationDetails <- ggplot(data = data, aes_(x = ~iteration, y = ~resusd, fill = ~convergence, text = ~Details)) +
    geom_col(alpha=aestethics$alpha) +
    theme_minimal() +
    facet_wrap(~region) + #, scales = 'free'
    scale_fill_manual(values=convergenceTypeColor) +
    labs(x = NULL, y = NULL, title = "") +
    theme(legend.position = 'bottom') +
    ggtitle("Convergence time in seconds") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  diag$plotly$iterationDetails <- ggplotly(diag$plots$iterationDetails, tooltip = c("text")) %>%
                                      hide_legend() %>%
                                      config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE) %>%
                                      layout(legend = list(orientation = "h", x = 0.4, y =-0.05))
  
  ### Convergence time per region and iteration (error bars)
  is_outlier <- function(x) { #Get outliers
    return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
  }
  
  data <- diag$data$summaryTable %>%
    group_by_(~iteration) %>%
    mutate_(outlier = ~ifelse(is_outlier(resusd), resusd, as.numeric(NA)))
  
  #tooltip
  data$Outlier_info <- paste0("<br>Region: ",data$region, "<br>Duration: ", format_duration(data$resusd), "<br>Iteration: ", data$iteration)
  
  color <- plotstyle(as.character(unique(data$region)),unknown=missingColorsdf)
  
  diag$plots$iterationProgress <- suppressWarnings( ggplot(data, aes_(x = ~iteration, y = ~resusd)) +
    geom_boxplot(fill=rgb(0,0.75,0.75),alpha=aestethics$alpha) +
    geom_point(aes_(x=~iteration, y=~outlier, fill=~region, text=~Outlier_info), size=2.5,alpha=aestethics$alpha) +
    theme_minimal() +
    scale_fill_manual(values=color) + 
    labs(x = NULL, y = "Seconds") 
    )
 
  diag$plotly$'Execution time' <- ggplotly(diag$plots$iterationProgress, tooltip = c("text")) %>% hide_legend() %>% config(modeBarButtonsToRemove=plotlyButtonsToHide, displaylogo=FALSE)
  
  # remove contour created by plotly on outliers
  diag$plotly$'Execution time'$x$data <- lapply(diag$plotly$'Execution time'$x$data, FUN = function(x){
    if(!(is.null(x$marker$outliercolor))){
      x$marker = list(opacity = 0)
    }
    return(x)
  })
  
  diag$legend$'Execution time'$description <- "<p>Regional time deviation and region outliers per iteration.</p>"
  diag$legend$'Execution time'$contents <- lapply(as.character(unique(data$region)), function(reg) { return(list("fill"=color[reg],"linetype"=NULL)) })
  names(diag$legend$'Execution time'$contents) <- as.character(unique(data$region))
  diag$legend$'Execution time'$units <- "seconds"
  
  #preparing output object
  out <- diag
  
  #remove data from output object
  if(!(includeData)){
    out$data <- NULL
    out$data$runTime$formated <- diag$data$runTime$formated
    out$data$runTime$longFormat <- diag$data$runTime$longFormat
    out$data$runStatistics$config$gms$optimization <- diag$data$runStatistics$config$gms$optimization
  }
  
  #remove extra plot types from output object
  if(!(length(setdiff(c("plotly","ggplot"),chartType))) == 0)
    out[[setdiff(c("plotly","ggplot"),chartType)]] <- NULL
  
  return(out)
}
