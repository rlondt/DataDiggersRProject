source('./init.R')
library(DataDiggersPackage)
flog.threshold(DEBUG)
startPreparation(workdir = "D:/datafiles2", dataframesToGlobalEnvironment = TRUE, rebuild = FALSE)

df <- prep.workflowDF%>%
  filter(Status!="Geannuleerd")

library(tidyverse)

df_start <- df %>%
  mutate(lifecycle_id="start")%>%
  mutate(timestamp=Starttijd)%>%
  mutate(case_id=Ordernummer)%>%
  mutate(activity_id=Taakomschrijving)%>%
  select(lifecycle_id, activity_id, timestamp, case_id )
df_einde <- df %>%
  mutate(lifecycle_id="complete")%>%
  mutate(timestamp=WerkelijkeEindtijd)%>%
  mutate(case_id=Ordernummer)%>%
  mutate(activity_id=Taakomschrijving)%>%
  select(lifecycle_id, activity_id, timestamp, case_id )

df_eventlog <- rbind(df_start, df_einde)  

df_eventlog <- df_eventlog%>%
  order(unclass(timestamp))


df_eventlog <- df_eventlog%>%
  mutate( activity_instance_id = 1:nrow(.),
          resource_id = ".")
  


%>%
  mutate(activity_instance_id=nrow(.)) 

eventlogDF <- eventlog(df_eventlog
                       , case_id="case_id"
                       , activity_id = "activity_id"
                       , activity_instance_id = "activity_instance_id"
                       , lifecycle_id = "lifecycle_id"
                       , timestamp = "timestamp"
                       , resource_id = "resource_id"
)

dumpRDS(eventlogDF, "eventlog.rds")


select(Ordernummer, Taakomschrijving, )







unique(prep.workflowDF$Status)

el <- simple_eventlog(eventlog=prep.workflowDF
               , case_id="Ordernummer"
               , activity_id = "Taakomschrijving"
               , timestamp = "Starttijd")
write_rds(el, "eventlog.rds")


el <- read_rds("eventlog.rds")

process_map(el)

precedence_matrix(el,type="relative") %>%
  plot


library(processmonitR)
processmonitR::activity_dashboard(el)
processmonitR::performance_dashboard(el)
