#'  A Function to initialize dataframes to the datadiggers project
#'
#' This function 
#' @param workdir directory where the sourcefiles are located
#' @param rebuild indication of dataframes have to be rebuild when not available
#' @param dataframesToGlobalEnvironment infication of dataframes to global enironment
#' @keywords import prepare eda
#' @export
#' @examples
#' startPreparation( "C:/files", TRUE, FALSE)
startPreparation <- function(workdir, rebuild=FALSE, dataframesToGlobalEnvironment=FALSE){
  setWorkdir(workdir)
  stopifnot(is.logical(rebuild))
  stopifnot(is.logical(dataframesToGlobalEnvironment))
  
  futile.logger::flog.info(msg = "Inlezen bronbestanden")
  if(!file.exists(getLocationNaam("prep_medewerkers.rds", FALSE))){
    
    futile.logger::flog.debug(msg = ".Lees medewerkers.csv")
    org.medewerkersDF <- readCSV("Medewerkers.csv")
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    futile.logger::flog.debug(msg = ".Omzetten null-values, factoren en datums")

    prep.medewerkersDF <- org.medewerkersDF %>%
      mutate(MDWID = as.character(MDWID)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))

    futile.logger::flog.debug(msg = ".Opslaan medewerkers")
    dumpRDS(org.medewerkersDF, "org_medewerkers.rds")
    dumpRDS(prep.medewerkersDF, "prep_medewerkers.rds")
  } else {
    futile.logger::flog.debug(msg = ".Lees medewerkers.rds")
    org.medewerkersDF <- readRDSdd( "org_medewerkers.rds")
    prep.medewerkersDF <- readRDSdd( "prep_medewerkers.rds")
  }
  if(!file.exists(getLocationNaam("prep_orders.rds", FALSE))|rebuild){
    futile.logger::flog.debug(msg = "Build medewerkers")
    org.ordersDF      <- readCSV("Orders.csv")
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    futile.logger::flog.debug(msg = ".Omzetten null-values, factoren en datums")
    prep.ordersDF <- org.ordersDF %>%
      mutate(Ordernummer = as.character(Ordernummer)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))%>%
      mutate(CreationDate = convertToDateTime(CreationDate)) %>%
      mutate(Uiterstehersteltijd = convertToDateTime(Uiterstehersteltijd)) %>%
      mutate(GeplandeTG = convertToDateTime(GeplandeTG)) %>%
      mutate(ModifiedOn = convertToDateTime(ModifiedOn))
    dumpRDS(org.ordersDF, "org_orders.rds")
    dumpRDS(prep.ordersDF, "prep_orders.rds")
  } else {
    futile.logger::flog.debug(msg = ".Lees orders.rds")
    org.ordersDF <- readRDSdd( "org_orders.rds")
    prep.ordersDF <- readRDSdd( "prep_orders.rds")
  }  
  
  if(!file.exists(getLocationNaam("prep_roosterdiensten.rds", FALSE))|rebuild){
    futile.logger::flog.debug(msg = ".Lees Roosterdiensten.csv")
    org.roosterdienstenDF <- readCSV("Roosterdiensten.csv")
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    futile.logger::flog.debug(msg = ".Omzetten null-values, factoren en datums")
    prep.roosterdienstenDF <- org.roosterdienstenDF %>%
      mutate(DienstID = as.character(DienstID)) %>%
      mutate(MDWID = as.character(MDWID)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))%>%
      mutate(Starttijd = convertToDateTime(Starttijd))%>%
      mutate(Eindtijd = convertToDateTime(Eindtijd))%>%
      mutate(ShiftDate = convertToDate(ShiftDate))
    
    futile.logger::flog.debug(msg = ".Schrijf roosterdiensten.rds")
    dumpRDS(org.roosterdienstenDF, "org_roosterdiensten.rds")
    dumpRDS(prep.roosterdienstenDF, "prep_roosterdiensten.rds")
  }else {
    futile.logger::flog.debug(msg = ".Lees roosterdiensten.rds")
    org.roosterdienstenDF <- readRDSdd( "org_roosterdiensten.rds")
    prep.roosterdienstenDF <- readRDSdd( "prep_roosterdiensten.rds")
  }
  
  if(!file.exists(getLocationNaam("prep_tijdschrijven.rds", FALSE))|rebuild){
    futile.logger::flog.debug(msg = ".Lees Tijdschrijven.csv")
    org.tijdschrijvenDF <- readCSV("Tijdschrijven.csv") 
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    futile.logger::flog.debug(msg = ".Omzetten null-values, factoren en datums")
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
    
    futile.logger::flog.debug(msg = ".Schrijf tijdschrijven.rds")
    dumpRDS(org.tijdschrijvenDF, "org_tijdschrijven.rds")
    dumpRDS(prep.tijdschrijvenDF, "prep_tijdschrijven.rds")
    
  }else {
    futile.logger::flog.debug(msg = ".Lees tijdschrijven.rds")
    org.tijdschrijvenDF <- readRDSdd( "org_tijdschrijven.rds")
    prep.tijdschrijvenDF <- readRDSdd( "prep_tijdschrijven.rds")
  }  
  
  if(!file.exists(getLocationNaam("prep_workflow.rds", FALSE))|rebuild){
    futile.logger::flog.debug(msg = ".Lees workflow 1+2.csv")
    org.workflowDF <- rbind(readCSV("Workflow 1.csv"),readCSV("Workflow 2.csv"))
    
    # eerste bewerking
    # omzetten factoren naar echte waardes
    # omzetten datums naar R-datums
    # lege strings, 'NA' en spaties naar NA
    futile.logger::flog.debug(msg = ".Omzetten null-values, factoren en datums")
    prep.workflowDF <- org.workflowDF %>%
      mutate(Ordernummer = as.character(Ordernummer)) %>%
      mutate_all(list(~na_if(.,"")))%>%
      mutate_all(list(~na_if(.," ")))%>%
      mutate_all(list(~na_if(.,"NA")))%>%
      mutate(RecordGewijzigd = convertToDateTime(RecordGewijzigd)) %>%
      mutate(GeplandeEindtijd = convertToDateTime(GeplandeEindtijd)) %>%
      mutate(WerkelijkeEindtijd = convertToDateTime(WerkelijkeEindtijd)) %>%
      mutate(Starttijd = convertToDateTime(Starttijd))
    
    futile.logger::flog.debug(msg = ".schrijf workflow.rds")
    dumpRDS(org.workflowDF, "org_workflow.rds")
    dumpRDS(prep.workflowDF, "prep_workflow.rds")
  }else {
    futile.logger::flog.debug(msg = ".Lees workflow.rds")
    org.workflowDF <- readRDSdd( "org_workflow.rds")
    prep.workflowDF <- readRDSdd( "prep_workflow.rds")
  }  
  
  ##
  # Backoffice data = workflow 
  # workflow actifiteiten met orders
  if(!file.exists(getLocationNaam("join_ordersWorkflow.rds", FALSE))|rebuild){
    futile.logger::flog.info(msg = "Workflow actifiteiten met orders")
    join.ordersWorkflowDF <- left_join(prep.ordersDF, prep.workflowDF)
    dumpRDS(join.ordersWorkflowDF, "join_ordersWorkflow.rds")
  } else {
    join.ordersWorkflowDF <- readRDSdd("join_ordersWorkflow.rds")
  } 
  
  
  ##
  # joins operations = tijdschrijven
  # Diensten met medewerkers
  futile.logger::flog.info(msg = "Combineren dienst, medewerkers en tijdschrijven")
  
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
  
  futile.logger::flog.info(msg = "Tijdschrijven summary")
  
  if(!file.exists(getLocationNaam("summarized_OrderTijdschrijvenByOrder.rds", FALSE))|rebuild){
    futile.logger::flog.info(msg = "maken summarized.OrderTijdschrijvenByOrderDF.rds")
    summarized.OrderTijdschrijvenByOrderDF <- join.ordersTijdschrijvenDF %>%
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
    dumpRDS(summarized.OrderTijdschrijvenByOrderDF, "summarized_OrderTijdschrijvenByOrder.rds")           
  } else {
    futile.logger::flog.info(msg = "Lezen summarized_OrderTijdschrijvenByOrderDF.rds")
    summarized.OrderTijdschrijvenByOrderDF <- readRDSdd("summarized_OrderTijdschrijvenByOrder.rds")           
  }
  
  if (!file.exists(getLocationNaam("summarized_Workflow.rds", FALSE))|rebuild){
    futile.logger::flog.debug("create summarizedWorkflowDF")
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
    futile.logger::flog.debug("opslaan summarizedWorkflowDF")
    dumpRDS(summarized.WorkflowDF, "summarized_Workflow.rds")
  } else {
    futile.logger::flog.debug("lees summarizedWorkflowDF")
    summarized.WorkflowDF <- readRDSdd("summarized_Workflow.rds")
  }
  
  
  if (!file.exists(getLocationNaam("summarized_WorflowTijdschrijven.rds", FALSE))|rebuild){
    futile.logger::flog.debug("create summarizedWorkflowTijdschrijvenDF")
    summarized.WorflowTijdschrijvenDF <- full_join(summarized.WorkflowDF, summarized.OrderTijdschrijvenByOrderDF)
    
    futile.logger::flog.debug("opslaan summarizedWorkflowDF")
    dumpRDS(summarized.WorflowTijdschrijvenDF, "summarized_WorflowTijdschrijven.rds")
  } else{
    futile.logger::flog.debug("lees summarizedWorkflowDF")
    summarized.WorflowTijdschrijvenDF <- readRDSdd("summarized_WorflowTijdschrijven.rds")
  }
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
    
    assign("join.ordersTijdschrijvenDF", join.ordersTijdschrijvenDF, envir = globalenv())  
    assign("join.dienstMedewerkersDF", join.dienstMedewerkersDF, envir = globalenv())  
    assign("join.tijdschrijvenDienstMedewerkersDF", join.tijdschrijvenDienstMedewerkersDF, envir = globalenv())  
    
    assign("summarized.WorflowTijdschrijvenDF", summarized.WorflowTijdschrijvenDF, envir = globalenv())  
    assign("summarized.WorkflowDF", summarized.WorkflowDF, envir = globalenv())  
    assign("summarized.OrderTijdschrijvenByOrderDF", summarized.OrderTijdschrijvenByOrderDF, envir = globalenv())  
  }
  invisible()
}


