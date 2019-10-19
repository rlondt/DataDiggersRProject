if (file.exists('./init.R')){
  source('./init.R')
}

zetWerkdir("D:/datafiles")
medewerkersDF <- leesCSV("Workflow 1.csv", na.strings=c("","NA"," "))
medewerkersDF2 <- leesCSV("Workflow 1.csv")
medewerkersDF <- leesCSV( "medewerkers.rds")



flog.info(msg = "Inlezen bronbestanden")
if(!file.exists("medewerkers.rds")){
  medewerkersDF <- leesCSV("Medewerkers.csv", na.strings=c("","NA"," "))
  ordersDF      <- leesCSV("Orders.csv", na.strings=c("","NA"," "))
  roosterdienstenDF <- leesCSV("Roosterdiensten.csv", na.strings=c("","NA"," "))
  tijdschrijvenDF <- leesCSV("Tijdschrijven.csv", na.strings=c("","NA"," ")) 
  workflowDF <- rbind(leesCSV("Workflow 1.csv",na.strings=c("","NA"," ")),leesCSV("Workflow 2.csv", na.strings=c("","NA"," ")))
  
  
  # eerste bewerking
  # omzetten factoren naar echte waardes
  # omzetten datums naar R-datums
  
  flog.info(msg = "Omzetten factoren en datums")
  
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
  
  
  flog.info(msg = "Tussenresultaten opslaan")
  
  schrijfRDS(medewerkersDF, "medewerkers.rds")
  schrijfRDS(ordersDF, "orders.rds")
  schrijfRDS(roosterdienstenDF, "roosterdiensten.rds")
  schrijfRDS(tijdschrijvenDF, "tijdschrijven.rds")
  schrijfRDS(workflowDF, "workflow.rds")
} else {
  medewerkersDF <- leesRDS( "medewerkers.rds")
  ordersDF <- leesRDS( "orders.rds")
  roosterdienstenDF <- leesRDS( "roosterdiensten.rds")
  tijdschrijvenDF <- leesRDS( "tijdschrijven.rds")
  workflowDF <- leesRDS( "workflow.rds")
}





##
# Backoffice data = workflow 
# workflow actifiteiten met orders
flog.info(msg = "Workflow actifiteiten met orders")
ordersWorkflowDF <- left_join(ordersDF, workflowDF)
schrijfRDS(ordersWorkflowDF, "ordersWorkflow.rds")



##
# joins operations = tijdschrijven
# Diensten met medewerkers
flog.info(msg = "Combineren dienst, medewerkers en tijdschrijven")

if (!file.exists("ordersTijdschrijven.rds")){
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
  schrijfRDS(ordersTijdschrijvenDF, "ordersTijdschrijven.rds")
} else {
  ordersTijdschrijvenDF <- leesRDS("ordersTijdschrijven.rds")
}

flog.info(msg = "Tijdschrijven summary")

if(!file.exists("summarizeOrderTijdschrijvenByOrder.rds")){
  flog.info(msg = "maken summarizeOrderTijdschrijvenByOrderDF.rds")
  summarizeOrderTijdschrijvenByOrderDF <- ordersTijdschrijvenDF %>%
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
  schrijfRDS(summarizeOrderTijdschrijvenByOrderDF, "summarizeOrderTijdschrijvenByOrder.rds")           
} else {
  flog.info(msg = "Lezen summarizeOrderTijdschrijvenByOrderDF.rds")
  summarizeOrderTijdschrijvenByOrderDF <- leesRDS("summarizeOrderTijdschrijvenByOrder.rds")           
}




flog.info(msg = "Workflow summary")
if(is.null(workflowDF)){
  workflowDF <- leesRDS("workflow.rds")
}

workflowStappen <- c("dummy"
                     # , "Opstellen POD"
                     # , "V&G plan"
                     # , "Opheffen dienst_Gepland"
                     # , "Kostenindicatie bewaken_Gepland"
                     # , "Kostenindicatie bewaken"
                     # , "Kostenindicatie maken"
                     # , "Vervallen opdracht analyse/terugwerken"
                     # , "Bewaken opheffen Dienst"
                     # , "Site Survey / TPO"
                     # , "Site Survey / TPO tbv offerte"
                     # , "Verwerken revisie VWT"
                     # , "Vervuilde grond"
                      # , "Dossier sluiten"
                      # , "Beoordelen opheffen dienst"
                     # , "Gereedmelden KPN systemen (AWO)"
                     # , "Werkvoorbereiding VWT"
                      , "Vergunning aanvraag langlopend"
                     # , "DSQ - lengtebepaling t.b.v. offerte"
                      , "Offerte bewaking"
                      , "Werkvoorbereiding HLD tbv Offerte"
                     # , "Vervallen opdracht afwikkelen"
                     # , "Gereedmelden Netwerkadministratie (Kanvas)"
                     # , "Verwerken Schadedossier"
                      , "Klantafspraak controleren met klant"
                      , "Werkvoorbereiding last minute"
                      , "Klantafspraak  maken"
                      , "Afmelden vergunning verlenende instanties"
                      , "Werkvoorbereiding India"
                      , "Verwerken revisie India"
                     , "Uiterste hersteltijd"
                     , "Afstemming facturatie"
                     , "TAG gereed"
                     , "Beoordelen opgeleverde werkmap"
                     , "Uitvoering"
                     , "Cleanbeoordeling"
)


# workflowStappen <- c("dummy"
#                      , "Afstemming facturatie"
#                      , "TAG gereed"
#                      , "Beoordelen opgeleverde werkmap"
#                      , "Uitvoering"
#                      , "Cleanbeoordeling"
# )


summarizedWorkflowDF <- workflowDF %>%
  filter(Status == "Gereed") %>%
  filter(Taakomschrijving %in% workflowStappen )%>%
  mutate( Starttijd = unclass(Starttijd)
          #,NormDoorlooptijd = NormDoorlooptijd
          ,GeplandeEindtijd = unclass(GeplandeEindtijd)
          ,WerkelijkeEindtijd = unclass(WerkelijkeEindtijd)
  ) %>%
  select( "Ordernummer"
          , "Taakomschrijving"
          , "Starttijd"
          , "NormDoorlooptijd"
          , "GeplandeEindtijd"
          , "WerkelijkeEindtijd") %>%
  transposeWorkflowDataFrame(  c( "Starttijd"
                                , "NormDoorlooptijd"
                                , "GeplandeEindtijd"
                                , "WerkelijkeEindtijd"
                               )
                            ,  Ordernummer ~ Taakomschrijving
                            )

schrijfRDS(summarizedWorkflowDF, "summarizedWorkflow.rds")

flog.info("summarizedWorflowTijdschrijvenDF")
summarizedWorflowTijdschrijvenDF <- full_join(summarizedWorkflowDF, summarizeOrderTijdschrijvenByOrderDF)
schrijfRDS(summarizedWorflowTijdschrijvenDF, "summarizedWorflowTijdschrijven.rds")

