source("./init.R")
library(DataDiggersPackage)
library(bupaR)
library(pm4py)
library(heuristicsmineR)
library(petrinetR)

flog.threshold(DEBUG)
startPreparation(workdir = "D:/datafiles2", dataframesToGlobalEnvironment = TRUE, rebuild = FALSE)

  
overlappendeWorkflowstappenDF <- readRDSdd( "dq_unique_4a.rds")
overlappendeWorkflowstappenDF <- overlappendeWorkflowstappenDF %>%
  filter(starttijd_1 > as.POSIXct("2019-05-01 00:00:00", tz="UTC")) %>%
  filter(starttijd_1 < as.POSIXct("2019-05-15 00:00:00", tz="UTC")) 


ordersInDF <- join.ordersWorkflowDF %>%
  filter(Starttijd > as.POSIXct("2019-02-01 00:00:00", tz="UTC")) %>%
  filter(Starttijd < as.POSIXct("2019-05-31 00:00:00", tz="UTC")) %>%
  group_by(Ordernummer)%>%
  summarize(aantal=n())
  



levels(join.ordersWorkflowDF$Categorie)

for (categorie in (levels(join.ordersWorkflowDF$Categorie))){
  df <- join.ordersWorkflowDF%>%
    filter(Status!="Geannuleerd") %>%
    filter(Ordernummer %in% ordersInDF$Ordernummer) %>%
    filter(as.character(Categorie) == categorie)

  df_start <- df %>%
    mutate(lifecycle_id="start")%>%
    mutate(timestamp=Starttijd)%>%
    mutate(case_id=Ordernummer)%>%
    mutate(activity_id=Taakomschrijving)%>%
    mutate(resource_id=Klantteam)%>%
    select(lifecycle_id, activity_id, timestamp, case_id, resource_id )
  
  df_einde <- df %>%
    mutate(lifecycle_id="complete")%>%
    mutate(timestamp=WerkelijkeEindtijd)%>%
    mutate(case_id=Ordernummer)%>%
    mutate(activity_id=Taakomschrijving)%>%
    mutate(resource_id=Klantteam)%>%
    select(lifecycle_id, activity_id, timestamp, case_id, resource_id )
  
  df_eventlog <- rbind(df_start, df_einde)  
  
  df_eventlog <- df_eventlog%>%
    arrange(timestamp)%>%
    mutate(activity_instance_id = 1:nrow(.))#row_number())
  
  eventlog <- eventlog(df_eventlog
                       , case_id="case_id"
                       , activity_id = "activity_id"
                       , activity_instance_id = "activity_instance_id"
                       , lifecycle_id = "lifecycle_id"
                       , timestamp = "timestamp"
                       , resource_id = "resource_id"
  )
  dumpRDS(eventlog, paste("eventlog",format(Sys.time(), "%y-%m-%d-%H-%M"),categorie, ".rds", sep = "_"))

  assign(paste("eventlog",categorie, sep = ".")
         , eventlog
         , globalenv())
  
  eventlog %>%process_map()

  precedence_matrix <- precedence_matrix(eventlog, type="absolute") 
  assign(paste("precedence_matrix",categorie, sep = ".")
         , precedence_matrix
         , globalenv())
  assign(paste("precedence_matrix",categorie,"plot", sep = ".")
         , precedence_matrix%>%plot()
         , globalenv())
  

  depMatrix <- dependency_matrix(eventlog) 
  assign(paste("dependency_matrix",categorie,"object", sep = ".")
         , dependency_matrix
         , globalenv())
  assign(paste("dependency_matrix",categorie,"matrix", sep = ".")
         , render_dependency_matrix(depMatrix)
         , globalenv())
  
  cNet <- causal_net(eventlog, threshold = .7) 
  assign(paste("cnet","0_7", categorie, sep = ".")
         , cNet 
         , globalenv())
  dumpRDS(cNet, paste("cnet_0_7",categorie,"rdx", sep = "."))
  assign(paste("cnet", "0_7",categorie,"plot", sep = ".")
         , render_causal_net(cNet, title=paste("casual net:", categorie, "~ drempel = 0.7" ,sep=" "))
         , globalenv())
  
  
  cNet <- causal_net(eventlog, threshold = .9) 
  assign( paste("cnet","0_7", categorie, sep = ".")
          , cNet
          , globalenv())
  dumpRDS(cNet, paste("cnet_0_9",categorie,"rdx", sep = "."))
  assign(paste("cnet", "0_9",categorie,"plot", sep = ".")
         , render_causal_net(cNet, title=paste("casual net:", categorie, "~ drempel = 0.9" ,sep=" "))
         , globalenv())
}

cnet.0_7.NLS.plot
cnet.0_7.Schade.plot
cnet.0_7.Storing.plot

cnet.0_9.NLS.plot
cnet.0_9.Schade.plot
cnet.0_9.Storing.plot

readRDSdd("cnet_0_9.NLS.rdx")
  
eventlog.Schade %>%
  dotted_chart( x="absolute", y="start")

processmonitR::activity_dashboard(eventlog.Schade)
processmonitR::performance_dashboard(eventlog.Schade)


install.packages("processanimateR")
library(processanimateR)
library(eventdataR)
animate_process(eventlog.Schade, mode = "absolute", duration = 300
                , mapping = token_aes(color = token_scale("resource_id", 
                                                        scale = "ordinal", 
                                                        range = RColorBrewer::brewer.pal(7, "Paired"))))


animate_process(eventlog.Schade, mode = "relative", jitter = 10, legend = "color",
                mapping = token_aes(color = token_scale("employee", 
                                                        scale = "ordinal", 
                                                        range = RColorBrewer::brewer.pal(7, "Paired"))))



# simpleEventlog <- simple_eventlog(eventlog=prep.workflowDF
#                                   , case_id="Ordernummer"
#                                   , activity_id = "Taakomschrijving"
#                                   , timestamp = "Starttijd")
# write_rds(simpleEventlog, "simple_eventlog.rds")


