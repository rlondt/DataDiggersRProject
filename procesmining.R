source('./init.R')
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
  filter(starttijd_1 < as.POSIXct("2019-05-08 00:00:00", tz="UTC")) 

df <- join.ordersWorkflowDF%>%
  filter(Status!="Geannuleerd") %>%
  filter(Ordernummer %in% overlappendeWorkflowstappenDF$Ordernummer)

df_start <- df %>%
  mutate(lifecycle_id="start")%>%
  mutate(timestamp=Starttijd)%>%
  mutate(case_id=Ordernummer)%>%
  mutate(activity_id=Taakomschrijving)%>%
  mutate(resource_id=paste(Categorie, Klantteam))%>%
  select(lifecycle_id, activity_id, timestamp, case_id, resource_id )

df_einde <- df %>%
  mutate(lifecycle_id="complete")%>%
  mutate(timestamp=WerkelijkeEindtijd)%>%
  mutate(case_id=Ordernummer)%>%
  mutate(activity_id=Taakomschrijving)%>%
  mutate(resource_id=paste(Categorie, Klantteam))%>%
  select(lifecycle_id, activity_id, timestamp, case_id, resource_id )

df_eventlog <- rbind(df_start, df_einde)  

df_eventlog <- df_eventlog%>%
  arrange(timestamp)%>%
  mutate(activity_instance_id = 1:nrow(.))#row_number())


# simpleEventlog <- simple_eventlog(eventlog=prep.workflowDF
#                                   , case_id="Ordernummer"
#                                   , activity_id = "Taakomschrijving"
#                                   , timestamp = "Starttijd")
# write_rds(simpleEventlog, "simple_eventlog.rds")

eventlog <- eventlog(df_eventlog
                       , case_id="case_id"
                       , activity_id = "activity_id"
                       , activity_instance_id = "activity_instance_id"
                       , lifecycle_id = "lifecycle_id"
                       , timestamp = "timestamp"
                       , resource_id = "resource_id"
)
dumpRDS(eventlog, "eventlog.rds")

eventlog %>%
  process_map()


eventlog%>%
  process_map(performance(median, "days"))

precedence_matrix <- precedence_matrix(eventlog, type="absolute") 
precedence_matrix%>%plot()


depMatrix <- dependency_matrix(eventlog) 
render_dependency_matrix(depMatrix)

cNet <- causal_net(eventlog) 
render_causal_net(cNet) 



eventlog_complete <- eventlog %>% filter_lifecycle("complete")
discovery_alpha(eventlog_complete) -> PN



precedence_matrix <- precedence_matrix(eventlog, type="absolute") 
precedence_matrix%>%plot()

processmonitR::activity_dashboard(eventlog)
processmonitR::performance_dashboard(eventlog)
