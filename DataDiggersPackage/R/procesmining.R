#' DataPreparation for procesMining with Bupar
#' @param pStartTijd starttijd, waarbinnen een order moet vallen om een cnet te bepalen
#' @param pEindTijd Eindtijd, waarbinnen een order moet vallen om een cnet te bepalen
#' @param pThresholds collectie van thresholds waarvoor een causal net wordt gemaakt
#' @import tidyverse
#' @import bupaR
#' @import heuristicsmineR
#' @import petrinetR
#' @importFrom futile.logger flog.debug
#' @importFrom lubridate is.POSIXct
#' @export
procesMining.init <- function(pStartTijd, pEindTijd, pThresholds){
  stopifnot(is.POSIXct(pStartTijd))
  stopifnot(is.POSIXct(pEindTijd))
  flog.debug("bepalen welke orders binnen de periode vallen")
  werkdir = get("werkdir", envir=.DataDiggersPackageOptions)
  
  ordersInDF <- join.ordersWorkflowDF %>%
    filter(Starttijd > pStartTijd) %>%
    filter(Starttijd < pEindTijd) %>%
    group_by(Ordernummer)%>%
    summarize(aantal=n())
  
  for (categorie in (levels(join.ordersWorkflowDF$Categorie))){
    eventLogRDS <- paste("eventlog", format(pStartTijd, "%y-%m-%d-%H-%M"), format(pEindTijd, "%y-%m-%d-%H-%M"),categorie, ".rds", sep = "_")
    locatie = paste(werkdir, eventLogRDS, sep = "/")
    
    # Bepalen eventlog
    if(file.exists(locatie)){
      eventlog <- readRDSdd(eventLogRDS)
    } else{
      totaalDF <- join.ordersWorkflowDF%>%
        filter(Status!="Geannuleerd") %>%
        filter(Ordernummer %in% ordersInDF$Ordernummer) %>%
        filter(as.character(Categorie) == categorie)
      
      startDF <- totaalDF %>%
        mutate(lifecycle_id="start")%>%
        mutate(timestamp=Starttijd)%>%
        mutate(case_id=Ordernummer)%>%
        mutate(activity_id=Taakomschrijving)%>%
        mutate(resource_id=Klantteam)%>%
        select(lifecycle_id, activity_id, timestamp, case_id, resource_id )
      
      eindeDF <- totaalDF %>%
        mutate(lifecycle_id="complete")%>%
        mutate(timestamp=WerkelijkeEindtijd)%>%
        mutate(case_id=Ordernummer)%>%
        mutate(activity_id=Taakomschrijving)%>%
        mutate(resource_id=Klantteam)%>%
        select(lifecycle_id, activity_id, timestamp, case_id, resource_id )
      
      eventlogDF <- rbind(startDF, eindeDF)  
      
      eventlogDF <- eventlogDF%>%
        arrange(timestamp)%>%
        mutate(activity_instance_id = 1:nrow(.))#row_number())
      
      eventlog <- eventlog(eventlogDF
                           , case_id="case_id"
                           , activity_id = "activity_id"
                           , activity_instance_id = "activity_instance_id"
                           , lifecycle_id = "lifecycle_id"
                           , timestamp = "timestamp"
                           , resource_id = "resource_id"
      )
      dumpRDS(eventlog, eventLogRDS)
    }
    
    #eventlog per categorie naar global environment
    assign(paste("eventlog",categorie, sep = ".")
           , eventlog
           , globalenv())
    
    #precedence_matrix bepalen en naar global env
    precedence_matrix <- precedence_matrix(eventlog, type="absolute") 
    assign(paste("precedence_matrix",categorie, sep = ".")
           , precedence_matrix
           , globalenv())
    assign(paste("precedence_matrix",categorie,"plot", sep = ".")
           , precedence_matrix%>%plot()
           , globalenv())
    
    
    #dependency_matrix bepalen en naar global env
    depMatrix <- dependency_matrix(eventlog) 
    assign(paste("dependency_matrix",categorie,"object", sep = ".")
           , dependency_matrix
           , globalenv())
    assign(paste("dependency_matrix",categorie,"matrix", sep = ".")
           , render_dependency_matrix(depMatrix)
           , globalenv())
    
    # per drempel een causal-net bepalen
    for(threshold in pThresholds){
      if(!is.null(threshold)){
      cNet <- causal_net(eventlog, threshold = threshold) 
      assign(paste("cnet",gsub("\\.", "_", as.character(threshold)), categorie, sep = ".")
             , cNet 
             , globalenv())
      flog.debug(paste("cnet: ", paste("cnet",gsub("\\.", "_", as.character(threshold)) ,categorie,".rdx", sep = "_")))
      dumpRDS(cNet, paste("cnet",gsub("\\.", "_", as.character(threshold)),categorie,".rdx", sep = "_"))
      assign(paste("cnet"
                   , as.character(threshold)
                   ,categorie,"plot", sep = ".")
             , render_causal_net(cNet
                                 , title=paste("casual net:"
                                               , categorie
                                               , "~ drempel ="
                                               ,as.character(threshold)
                                               ,sep=" ")
                                 )
             , globalenv())
      }
    }
  }
}

