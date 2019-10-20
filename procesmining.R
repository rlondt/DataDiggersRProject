source("./init.R")
flog.threshold(DEBUG)
startPreparation("D:/datafiles", dataframesToGlobalEnvironment = TRUE)

el <- simple_eventlog(eventlog=prep.workflowDF
               , case_id="Ordernummer"
               , activity_id = "Taakomschrijving"
               , timestamp = "Starttijd")
write_rds(el, "eventlog.rds")


el <- read_rds("eventlog.rds")

process_map(el)

precedence_matrix(el,type="relative") %>%
  plot
