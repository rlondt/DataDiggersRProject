#'  Een functie die de ruwe bestanden verwerkt tot basis dataframes
#'
#' Deze functie maakt van de ruwe bestanden dataframes.
#' Indien mogelijk wordt hier gebruik gemaakt van reeds bestaande dumps
#' Indien aangegeven worden de bestaande dumps juist niet gebruikt maar alles opnieuw bepaald.
#' @param workdir Directory waar de data-bestanden zich bevinden
#' @param rebuild Indicatie of alles opnieuw gebouwd dient te worden
#' @param dataframesToGlobalEnvironment indicatie of de dataframes in het global environment beschikbaar gemaakt moeten worden
#' @keywords import prepare eda 
#' @import sqldf
#' @import tidyverse
#' @importFrom futile.logger flog.debug
#' @importFrom openxlsx convertToDateTime
#' @export
startPreparation <- function(workdir, rebuild=FALSE, dataframesToGlobalEnvironment=FALSE){
  setWorkdir(workdir)
  stopifnot(is.logical(rebuild))
  stopifnot(is.logical(dataframesToGlobalEnvironment))
  
  flog.info(msg = "Inlezen bronbestanden")
  ##
  # medewerkers
  flog.debug(msg = "medewerkers")
  if(!file.exists(getLocationNaam("prep_medewerkers.rds", FALSE))|rebuild){
    
    flog.debug(msg = ".Lees medewerkers.csv")
    org.medewerkersDF <- readCSV("Medewerkers.csv")
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    flog.debug(msg = ".Omzetten null-values, factoren en datums")

    prep.medewerkersDF <- org.medewerkersDF %>%
      mutate(MDWID = as.character(MDWID)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))

    flog.debug(msg = ".Opslaan medewerkers")
    dumpRDS(org.medewerkersDF, "org_medewerkers.rds")
    dumpRDS(prep.medewerkersDF, "prep_medewerkers.rds")
  } else {
    flog.debug(msg = ".Lees medewerkers.rds")
    org.medewerkersDF <- readRDSdd( "org_medewerkers.rds")
    prep.medewerkersDF <- readRDSdd( "prep_medewerkers.rds")
  }
  
  ##
  # orders
  flog.debug(msg = "orders")
  if(!file.exists(getLocationNaam("prep_orders.rds", FALSE))|rebuild){
    flog.debug(msg = "Build orders")
    org.ordersDF      <- readCSV("Orders.csv")
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    flog.debug(msg = ".Omzetten null-values, factoren en datums")
    prep.ordersDF <- org.ordersDF %>%
      mutate(Ordernummer = as.character(Ordernummer)) %>%
      mutate_all(list(~na_if(as.character(.),"")))%>%
      mutate_all(list(~na_if(as.character(.)," ")))%>%
      mutate_all(list(~na_if(as.character(.),"NA")))%>%
      mutate(CreationDate = convertToDateTime(CreationDate)) %>%
      mutate(Uiterstehersteltijd = convertToDateTime(Uiterstehersteltijd)) %>%
      mutate(GeplandeTG = convertToDateTime(GeplandeTG)) %>%
      mutate(ModifiedOn = convertToDateTime(ModifiedOn)) %>%
      mutate(Categorie = case_when(str_length(Categorie) > 0 ~ Categorie,
                                   str_count(as.character(OpdrachtType), "NLS")> 0 ~ "NLS"
                                  )
             )%>%
      mutate(Categorie = as.factor(Categorie)) %>%
      mutate(OrderStatus = as.factor(OrderStatus)) %>%
      mutate(Postcode4 = as.factor(Postcode4)) %>%
      mutate(Plaats = as.factor(Plaats)) %>%
      mutate(Klantteam = as.factor(Klantteam)) %>%
      mutate(NLSType = as.factor(NLSType)) %>%
      mutate(Orderacceptatie = as.factor(Orderacceptatie)) %>%
      mutate(OpdrachtVervalt = as.factor(OpdrachtVervalt)) %>%
      mutate(DSO = as.factor(DSO)) %>%
      mutate(DSOHistory = as.factor(DSOHistory)) %>%
      mutate(Stagnatie = as.factor(Stagnatie)) %>%
      mutate(Afmeldcode = as.factor(Afmeldcode)) %>%
      mutate(OpdrachtType = as.factor(OpdrachtType))
    
    dumpRDS(org.ordersDF, "org_orders.rds")
    dumpRDS(prep.ordersDF, "prep_orders.rds")
  } else {
    flog.debug(msg = ".Lees orders.rds")
    org.ordersDF <- readRDSdd( "org_orders.rds")
    prep.ordersDF <- readRDSdd( "prep_orders.rds")
  }  
  
  ##
  # Roosterdiensten
  flog.debug(msg = "roosterdiensten")
  if(!file.exists(getLocationNaam("prep_roosterdiensten.rds", FALSE))|rebuild){
    flog.debug(msg = ".Lees Roosterdiensten.csv")
    org.roosterdienstenDF <- readCSV("Roosterdiensten.csv")
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    flog.debug(msg = ".Omzetten null-values, factoren en datums")
    prep.roosterdienstenDF <- org.roosterdienstenDF %>%
      mutate(DienstID = as.character(DienstID)) %>%
      mutate(MDWID = as.character(MDWID)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))%>%
      mutate(Starttijd = convertToDateTime(Starttijd))%>%
      mutate(Eindtijd = convertToDateTime(Eindtijd))%>%
      mutate(ShiftDate = convertToDate(ShiftDate))
    
    flog.debug(msg = ".Schrijf roosterdiensten.rds")
    dumpRDS(org.roosterdienstenDF, "org_roosterdiensten.rds")
    dumpRDS(prep.roosterdienstenDF, "prep_roosterdiensten.rds")
  }else {
    flog.debug(msg = ".Lees roosterdiensten.rds")
    org.roosterdienstenDF <- readRDSdd( "org_roosterdiensten.rds")
    prep.roosterdienstenDF <- readRDSdd( "prep_roosterdiensten.rds")
  }
  
  
  ##
  # tijdschrijven
  flog.debug(msg = "tijdschrijven")
  if(!file.exists(getLocationNaam("prep_tijdschrijven.rds", FALSE))|rebuild){
    flog.debug(msg = ".Lees Tijdschrijven.csv")
    org.tijdschrijvenDF <- readCSV("Tijdschrijven.csv") 
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    flog.debug(msg = ".Omzetten null-values, factoren en datums")
    prep.tijdschrijvenDF <- org.tijdschrijvenDF %>%
      mutate(DienstID = as.character(DienstID)) %>%
      mutate(MDWID = as.character(MDWID)) %>%
      mutate(ERPID = as.character(ERPID)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))%>%
      mutate(CreatedOn = convertToDateTime(CreatedOn)) %>%
      mutate(ModifiedOn = convertToDateTime(ModifiedOn)) %>%
      mutate(StartDate = convertToDateTime(StartDate)) %>%
      mutate(EndDate = convertToDateTime(EndDate))
    
    flog.debug(msg = ".Schrijf tijdschrijven.rds")
    dumpRDS(org.tijdschrijvenDF, "org_tijdschrijven.rds")
    dumpRDS(prep.tijdschrijvenDF, "prep_tijdschrijven.rds")
    
  }else {
    flog.debug(msg = ".Lees tijdschrijven.rds")
    org.tijdschrijvenDF <- readRDSdd( "org_tijdschrijven.rds")
    prep.tijdschrijvenDF <- readRDSdd( "prep_tijdschrijven.rds")
  }  
  
  
  ##
  # workflow
  flog.debug(msg = "workflow")
  if(!file.exists(getLocationNaam("prep_workflow.rds", FALSE))|rebuild){
    flog.debug(msg = ".Lees workflow 1+2.csv")
    org.workflowDF <- rbind(readCSV("Workflow 1.csv"),readCSV("Workflow 2.csv"))
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    flog.debug(msg = ".Omzetten null-values, factoren en datums")
    prep.workflowDF <- org.workflowDF %>%
      mutate(Ordernummer = as.character(Ordernummer)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))%>%
      mutate(RecordGewijzigd = convertToDateTime(RecordGewijzigd)) %>%
      mutate(GeplandeEindtijd = convertToDateTime(GeplandeEindtijd)) %>%
      mutate(WerkelijkeEindtijd = convertToDateTime(WerkelijkeEindtijd)) %>%
      mutate(Starttijd = convertToDateTime(Starttijd))
    
    flog.debug(msg = ".schrijf workflow.rds")
    dumpRDS(org.workflowDF, "org_workflow.rds")
    dumpRDS(prep.workflowDF, "prep_workflow.rds")
  }else {
    flog.debug(msg = ".Lees workflow.rds")
    org.workflowDF <- readRDSdd( "org_workflow.rds")
    prep.workflowDF <- readRDSdd( "prep_workflow.rds")
  }  
  
  ##
  # postcodeplaats
  # is al geprepareerd
  flog.info(msg = "postcodeplaats")
  if(!file.exists(getLocationNaam("postcodeplaats.rds", FALSE))|rebuild){
    org.postcodeplaatsDF <- readCSV("PostcodeTabelNLDataOverheid.csv")%>%
      select(pc4, woonplaatsnaam)
    
    org.postcodeplaatsDF$woonplaatsnaam <- casefold(org.postcodeplaatsDF$woonplaatsnaam, upper = FALSE)
    org.postcodeplaatsDF$woonplaatsnaam[org.postcodeplaatsDF$woonplaatsnaam=="'s-gravenhage"] <- "den haag"

    # vervangen in dataoverheid
    org.postcodeplaatsDF$woonplaatsnaam <- trimws(org.postcodeplaatsDF$woonplaatsnaam)
    
    org.postcodeplaatsDF$woonplaatsnaam <- gsub("'", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub("-", " ", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub("y", "ij", org.postcodeplaatsDF$woonplaatsnaam)
    
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" (gld)", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" (nh)", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" (nb)", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" rotterdam", "", org.postcodeplaatsDF$woonplaatsnaam)
    
    # provincie afkortingen verwijderen
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" gld", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" nb", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" zh", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" ov", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" ut", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" lb", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" fr", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" dr", "", org.postcodeplaatsDF$woonplaatsnaam)
    org.postcodeplaatsDF$woonplaatsnaam <- gsub(" gn", "", org.postcodeplaatsDF$woonplaatsnaam)
    
    org.postcodeplaatsDF$sleutel <- paste0(org.postcodeplaatsDF$pc4, org.postcodeplaatsDF$woonplaatsnaam)
    dumpRDS(org.postcodeplaatsDF, "postcodeplaats.rds")
  } else {
    org.postcodeplaatsDF <- readRDSdd( "postcodeplaats.rds")
  }
  
  ##
  # Start joins
  flog.info(msg = "start joins")
  
  ##
  # Backoffice data = workflow 
  # workflow actifiteiten met orders
  if(!file.exists(getLocationNaam("join_ordersWorkflow.rds", FALSE))|rebuild){
    flog.info(msg = "Workflow actifiteiten met orders")
    join.ordersWorkflowDF <- left_join(prep.ordersDF, prep.workflowDF)
    dumpRDS(join.ordersWorkflowDF, "join_ordersWorkflow.rds")
  } else {
    join.ordersWorkflowDF <- readRDSdd("join_ordersWorkflow.rds")
  } 
  
  
  ##
  # joins operations = tijdschrijven
  # Diensten met medewerkers
  flog.info(msg = "Combineren dienst, medewerkers en tijdschrijven")
  if (!file.exists(getLocationNaam("join_ordersTijdschrijven.rds", FALSE))
      |!file.exists(getLocationNaam("join_dienstMedewerkers.rds", FALSE))
      |!file.exists(getLocationNaam("join_tijdschrijvenDienstMedewerkers.rds", FALSE))
      |rebuild){
    join.dienstMedewerkersDF <- left_join(prep.roosterdienstenDF, prep.medewerkersDF)
    # tijdschrijven met diensten en medewerkers
    join.tijdschrijvenDienstMedewerkersDF <- left_join(prep.tijdschrijvenDF, join.dienstMedewerkersDF)
    join.ordersTijdschrijvenDF <- left_join(prep.ordersDF, join.tijdschrijvenDienstMedewerkersDF, by=c("Ordernummer" = "ERPID")) %>%
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
    dumpRDS(join.ordersTijdschrijvenDF, "join_ordersTijdschrijven.rds")
    dumpRDS(join.dienstMedewerkersDF, "join_dienstMedewerkers.rds")
    dumpRDS(join.tijdschrijvenDienstMedewerkersDF, "join_tijdschrijvenDienstMedewerkers.rds")
  } else {
    join.ordersTijdschrijvenDF<- readRDSdd("join_ordersTijdschrijven.rds")
    join.dienstMedewerkersDF<- readRDSdd("join_dienstMedewerkers.rds")
    join.tijdschrijvenDienstMedewerkersDF<- readRDSdd("join_tijdschrijvenDienstMedewerkers.rds")
  }
  
  flog.info(msg = "Tijdschrijven summary")
  
  if(!file.exists(getLocationNaam("summarized_OrderTijdschrijvenByOrder.rds", FALSE))|rebuild){
    flog.info(msg = "maken summarized.OrderTijdschrijvenByOrderDF.rds")
    summarized.OrderTijdschrijvenByOrderDF <- join.ordersTijdschrijvenDF %>%
      group_by(Ordernummer) %>%
      summarise( VerschillendeDagen       = n_distinct(format(StartDate, "%Y-%m-%d"))
                 , AantalTijdschrijven               = n()
                 , AantalVerschillendeMedewerkers    = n_distinct(MDWID)
                 , OverschreidingUitersteHersteltijd = max(VerschilUitersteHerstelEindeTijdschrijvenInSeconden)
                 , TotaleSchrijftijdReis             = sum(replace_na(DuurTijdschrijvenInSeconden[Type == "Reis" ], 0) )
                 , TotaleSchrijftijdWerk             = sum(replace_na(DuurTijdschrijvenInSeconden[Type != "Reis" ], 0)  )
                 , TotaleDoorlooptijdVanuitKlant     = replace_na(max(EndDate[Type != "Reis" ]),max(EndDate)) - replace_na(min(StartDate[Type != "Reis" ], min(StartDate)))
                 , TotaleDoorlooptijdVanuitVWT       = max(EndDate) - min(StartDate)
                 , StarttijdTijdschrijven            = min(StartDate)
                 , EindtijdTijdschrijven             = max(EndDate)
                 , Uiterstehersteltijd               = mean(Uiterstehersteltijd)
      )
    dumpRDS(summarized.OrderTijdschrijvenByOrderDF, "summarized_OrderTijdschrijvenByOrder.rds")           
  } else {
    flog.info(msg = "Lezen summarized_OrderTijdschrijvenByOrderDF.rds")
    summarized.OrderTijdschrijvenByOrderDF <- readRDSdd("summarized_OrderTijdschrijvenByOrder.rds")           
  }
  
  if (!file.exists(getLocationNaam("summarized_Workflow.rds", FALSE))|rebuild){
    flog.debug("create summarizedWorkflowDF")
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
    
    summarized.WorkflowDF <- prep.workflowDF %>%
      filter(Status == "Gereed") %>%
      # filter(Taakomschrijving %in% workflowStappen )%>%
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
    flog.debug("opslaan summarizedWorkflowDF")
    dumpRDS(summarized.WorkflowDF, "summarized_Workflow.rds")
  } else {
    flog.debug("lees summarizedWorkflowDF")
    summarized.WorkflowDF <- readRDSdd("summarized_Workflow.rds")
  }
  
  
  if (!file.exists(getLocationNaam("summarized_WorflowTijdschrijven.rds", FALSE))|rebuild){
    flog.debug("create summarizedWorkflowTijdschrijvenDF")
    summarized.WorflowTijdschrijvenDF <- full_join(summarized.WorkflowDF, summarized.OrderTijdschrijvenByOrderDF)
    
    flog.debug("opslaan summarizedWorkflowDF")
    dumpRDS(summarized.WorflowTijdschrijvenDF, "summarized_WorflowTijdschrijven.rds")
  } else{
    flog.debug("lees summarizedWorkflowDF")
    summarized.WorflowTijdschrijvenDF <- readRDSdd("summarized_WorflowTijdschrijven.rds")
  }
  
  ##
  # link naar globalEnvironment
  
  if(dataframesToGlobalEnvironment){
    assign("org.medewerkersDF", org.medewerkersDF, envir = globalenv())  
    assign("prep.medewerkersDF", prep.medewerkersDF, envir = globalenv())  
    assign("org.tijdschrijvenDF", org.tijdschrijvenDF, envir = globalenv())  
    assign("prep.tijdschrijvenDF", prep.tijdschrijvenDF, envir = globalenv())  
    assign("org.roosterdienstenDF", org.roosterdienstenDF, envir = globalenv())  
    assign("prep.roosterdienstenDF", prep.roosterdienstenDF, envir = globalenv())  
    assign("org.workflowDF", org.workflowDF, envir = globalenv())  
    assign("prep.workflowDF", prep.workflowDF, envir = globalenv())  
    assign("org.ordersDF", org.ordersDF, envir = globalenv())  
    assign("prep.ordersDF", prep.ordersDF, envir = globalenv())  
    
    assign("join.ordersWorkflowDF", join.ordersWorkflowDF, envir = globalenv())  
    assign("join.ordersTijdschrijvenDF", join.ordersTijdschrijvenDF, envir = globalenv())  
    assign("join.dienstMedewerkersDF", join.dienstMedewerkersDF, envir = globalenv())  
    assign("join.tijdschrijvenDienstMedewerkersDF", join.tijdschrijvenDienstMedewerkersDF, envir = globalenv())  
    
    assign("summarized.WorflowTijdschrijvenDF", summarized.WorflowTijdschrijvenDF, envir = globalenv())  
    assign("summarized.WorkflowDF", summarized.WorkflowDF, envir = globalenv())  
    assign("summarized.OrderTijdschrijvenByOrderDF", summarized.OrderTijdschrijvenByOrderDF, envir = globalenv())  
    assign("org.postcodeplaatsDF", org.postcodeplaatsDF, envir = globalenv())
  }
  invisible()
}


