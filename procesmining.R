source('./init.R')
library(DataDiggersPackage)
library(bupaR)
library(pm4py)
library(heuristicsmineR)
library(petrinetR)

flog.threshold(DEBUG)
startPreparation(workdir = "D:/datafiles2", dataframesToGlobalEnvironment = TRUE, rebuild = FALSE)




df <- join.ordersWorkflowDF%>%
  filter(Status!="Geannuleerd") %>%
  filter(Starttijd > as.POSIXct("2019-05-01 00:00:00", tz="UTC")) 

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
  arrange(timestamp)


df_eventlog <- df_eventlog%>%
#  group_by(case_id) %>% 
#  mutate(activity_instance_id = row_number())
  mutate(activity_instance_id = row_number())

df_eventlog$activity_instance_id
  
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


depMatrix <- dependency_matrix(eventlog) 
render_dependency_matrix(depMatrix)

cNet <- causal_net(eventlogDF) 
render_causal_net(cNet)

precedence_matrix <- precedence_matrix(eventlog, type="relative") 

dumpRDS(precedence_matrix, "precedence_matrix.rds")

precedence_matrix%>%
  plot()





activities_completes <- eventlogDF %>% filter_lifecycle("eindtijd")

discovery_inductive(activities_completes, variant = variant_inductive_only_dfg()) -> PN


library(processmonitR)
processmonitR::activity_dashboard(el)
processmonitR::performance_dashboard(el)
