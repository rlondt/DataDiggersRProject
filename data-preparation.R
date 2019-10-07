if (file.exists('./init.R')){
  source('./init.R')
}

medewerkersDF <- read.csv2("Medewerkers.csv")
ordersDF      <- read.csv2("Orders.csv")
roosterdienstenDF <- read.csv2("Roosterdiensten.csv")
tijdschrijvenDF <- read.csv2("Tijdschrijven.csv") 
workflowDF <- rbind(read.csv2("Workflow 1.csv"),read.csv2("Workflow 2.csv"))


# eerste bewerking
# omzetten factoren naar echte waardes
# omzetten e
medewerkersDF <- medewerkersDF %>%
  mutate(MDWID = as.character(MDWID)) 

ordersDF <- ordersDF %>%
  mutate(Ordernummer = as.character(Ordernummer)) %>%
  mutate(CreationDate = convertToDateTime(CreationDate)) %>%
  mutate(Uiterstehersteltijd = convertToDateTime(Uiterstehersteltijd)) %>%
  mutate(GeplandeTG = convertToDateTime(GeplandeTG)) %>%
  mutate(ModifiedOn = convertToDateTime(ModifiedOn))

roosterdienstenDF <- roosterdienstenDF %>%
  mutate(DienstID = as.character(DienstID)) %>%
  mutate(MDWID = as.character(MDWID)) %>%
  mutate(Starttijd = convertToDateTime(Starttijd))%>%
  mutate(Eindtijd = convertToDateTime(Eindtijd))%>%
  mutate(ShiftDate = convertToDate(ShiftDate))
  
tijdschrijvenDF <- tijdschrijvenDF %>%
  mutate(DienstID = as.character(DienstID)) %>%
  mutate(MDWID = as.character(MDWID)) %>%
  mutate(ERPID = as.character(ERPID)) %>%
  mutate(CreatedOn = convertToDateTime(CreatedOn)) %>%
  mutate(ModifiedOn = convertToDateTime(ModifiedOn)) %>%
  mutate(StartDate = convertToDateTime(StartDate)) %>%
  mutate(EndDate = convertToDateTime(EndDate))
  
workflowDF <- workflowDF %>%
  mutate(RecordGewijzigd = convertToDateTime(RecordGewijzigd)) %>%
  mutate(Ordernummer = as.character(Ordernummer)) %>%
  mutate(GeplandeEindtijd = convertToDateTime(GeplandeEindtijd)) %>%
  mutate(WerkelijkeEindtijd = convertToDateTime(WerkelijkeEindtijd)) %>%
  mutate(Starttijd = convertToDateTime(Starttijd))


write_rds(medewerkersDF, "medewerkers.rds")
write_rds(ordersDF, "orders.rds")
write_rds(roosterdienstenDF, "roosterdiensten.rds")
write_rds(tijdschrijvenDF, "tijdschrijven.rds")
write_rds(workflowDF, "workflow.rds")


##
# Backoffice data = workflow 
# workflow actifiteiten met orders
ordersWorkflowDF <- left_join(ordersDF, workflowDF)
write_rds(ordersWorkflowDF, "ordersWorkflowDF.rds")



##
# joins operations = tijdschrijven
# Diensten met medewerkers
dienstMedewerkersDF <- left_join(roosterdienstenDF, medewerkersDF)
# tijdschrijven met diensten en medewerkers
tijdschrijvenDienstMedewerkersDF <- left_join(tijdschrijvenDF, dienstMedewerkersDF)
ordersTijdschrijvenDF <- left_join(ordersDF, tijdschrijvenDienstMedewerkersDF, by=c("Ordernummer" = "ERPID"))
write_rds(ordersTijdschrijvenDF, "ordersTijdschrijvenDF.rds")

