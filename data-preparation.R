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

ordersTijdschrijvenDF <- left_join(ordersDF, tijdschrijvenDienstMedewerkersDF, by=c("Ordernummer" = "ERPID")) %>%
  mutate(DuurTijdschrijvenInSeconden = EndDate - StartDate) %>%
  mutate(VerschilUitersteHerstelEindeTijdschrijvenInSeconden = EndDate - Uiterstehersteltijd) %>%
  filter(Verwerkingsstatus == "Akkoord" | Verwerkingsstatus == "Vrijgegeven ERP") %>%
  filter(OpdrachtVervalt == "ONWAAR") %>%
  filter(Approved == "WAAR") %>%
  select(  # Sleutel
    "MDWID"               
    , "Ordernummer"         
    , "DienstID"           
    # Tijdsindicaties
    , "StartDate"            # tijdschrijven
    , "EndDate"              # tijdschrijven
    , "DuurTijdschrijvenInSeconden"
    , "Uiterstehersteltijd" 
    , "VerschilUitersteHerstelEindeTijdschrijvenInSeconden"
    , "ShiftDate"           
    , "Starttijd"            # dienst
    , "Eindtijd"             # dienst
    , "Normatief"           
    , "GeplandeTG"  
    # Locatie
    , "Postcode4"           
    , "MDWPlaats"          
    
    # gegevens rondom werkzaamheden             
    , "AantalLassen"       
    , "MetersGeul"          
    , "Stagnatie"           
    , "NLSType"             
    
    # order gegevens
    , "Afmeldcode"         
    # medewerker
    , "EPAP"               
    
    
    #, "CreationDate"        
    , "Categorie"           
    
    
    , "Functie"             
    #, "ModifiedOn.x"       
    , "Plaats"             
    , "Klantteam.x"        
    , "Klantteam.y"         
    #, "DSO"                
    , "Dienstomschrijving" 
    , "Status"             
    #, "MoetUrenSchrijven"  
    , "Omschrijving"        
    #, "DSOHistory"          
    #, "CreatedOn"           
    , "Type"                # reis/werk
    , "Beoordeeld"          
    #, "OrderStatus"        
    , "OpdrachtType"       
    , "Orderacceptatie"    
    , "MapAkkoord"         
    , "ModifiedOn.y"       
    #, "Afspraak"           #leeg
    # filters
    #, "Approved"           # filter WAAR
    #, "Verwerkingsstatus"  # filter Akkoord of "Vrijgegeven ERP"
    #, "OpdrachtVervalt"    # filter ONWAAR 
  )
  
  
write_rds(ordersTijdschrijvenDF, "ordersTijdschrijvenDF.rds")

summary(ordersTijdschrijvenDF)

summarizeOrderTijdschrijvenByOrder <- ordersTijdschrijvenDF %>%
  group_by(Ordernummer) %>%
  summarise( VerschillendeDagen       = n_distinct(ShiftDate)
  , AantalTijdschrijven               = n()
  , OverschreidingUitersteHersteltijd = max(VerschilUitersteHerstelEindeTijdschrijvenInSeconden)
  , TotaleSchrijftijdReis             = sum(replace_na(DuurTijdschrijvenInSeconden[Type == "Reis" ], 0) )
  , TotaleSchrijftijdWerk             = sum(replace_na(DuurTijdschrijvenInSeconden[Type != "Reis" ], 0)  )
  , TotaleDoorlooptijdVanuitKlant     = replace_na(max(EndDate[Type != "Reis" ]),max(EndDate)) - replace_na(min(StartDate[Type != "Reis" ], min(StartDate)))
  , TotaleDoorlooptijdVanuitVWT       = max(EndDate) - min(StartDate)
  , StarttijdTijdschrijven            = min(StartDate)
  , EindtijdTijdschrijven             = max(EndDate)
  , Uiterstehersteltijd               = mean(Uiterstehersteltijd)
  )

# summarizeOrderTijdschrijvenByOrder2 <- ordersTijdschrijvenDF %>%
#   group_by(Ordernummer) %>%
#   summarise(  Uiterstehersteltijd               = mean(Uiterstehersteltijd)
#   )
# 
# summarizeOrderTijdschrijvenByOrder <- left_join(summarizeOrderTijdschrijvenByOrder, summarizeOrderTijdschrijvenByOrder2)


write_rds(summarizeOrderTijdschrijvenByOrder, "summarizeOrderTijdschrijvenByOrder")           

hist(as.numeric(summarizeOrderTijdschrijvenByOrder$OverschreidingUitersteHersteltijd, units = "hours"))

qplot(as.numeric(summarizeOrderTijdschrijvenByOrder$OverschreidingUitersteHersteltijd, units = "days"),
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for overschreiding", 
      xlab = "Overschrijding in uren",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(-40,40)
      )

qplot(as.numeric(TotaleSchrijftijdReis, units="hours"), data = summarizeOrderTijdschrijvenByOrder,
      geom="histogram",
      binwidth = 1)

qplot(as.numeric(TotaleSchrijftijdWerk, units="hours"), data = summarizeOrderTijdschrijvenByOrder,
      geom="histogram",
      binwidth = 1,
      xlim=c(0, 100))
