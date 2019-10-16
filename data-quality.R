if (file.exists('./init.R')){
  source('./init.R')
}

# Beschikbare bestanden "plat" inlezen
org.medewerkersDF <- read.csv2("Medewerkers.csv")
org.ordersDF      <- read.csv2("Orders.csv")
org.roosterdienstenDF <- read.csv2("Roosterdiensten.csv")
org.tijdschrijvenDF <- read.csv2("Tijdschrijven.csv") 
org.workflowDF <- rbind(read.csv2("Workflow 1.csv"),read.csv2("Workflow 2.csv"))

# Bewerkte bestanden inlezen (factoren en datums)
prep.medewerkersDF <- read_rds( "medewerkers.rds")
prep.ordersDF <- read_rds( "orders.rds")
prep.roosterdienstenDF <- read_rds( "roosterdiensten.rds")
prep.tijdschrijvenDF <- read_rds( "tijdschrijven.rds")
prep.workflowDF <- read_rds( "workflow.rds")

# combinatie bestanden inlezen
join.ordersWorkflowDF <- read_rds("ordersWorkflow.rds")
join.ordersTijdschrijvenDF <- read_rds("ordersTijdschrijven.rds")

# summarized dataframes
summarized.OrderTijdschrijvenByOrderDF <- read_rds("summarizeOrderTijdschrijvenByOrder.rds")
summarized.WorkflowDF <- read_rds("summarizedWorkflow.rds")           
summarized.WorflowTijdschrijvenDF <- read_rds("summarizedWorflowTijdschrijven.rds")


# Compleetheid
# 1. Mogelijkheden voor kruistellingen
# 2. Zijn alle onderdelen van VWT-data vertegenwoordigd
# 3. Missing values analyse
# 3.1 Medewerker
aggr_plot <- aggr(medewerkersDF,oma = c(8,5,5,3), col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(medewerkersDF), cex.axis=.8, 
                  gap=2, 
                  ylab=c("Missing values Medewerker","Combinatie"))

# 3.2 Order
aggr_plot <- aggr(ordersDF,oma = c(8,5,5,3), col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,combined = TRUE,
                  labels=names(ordersDF), cex.axis=.8, 
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Order","Combinatie"))

# alternative
naniar::gg_miss_var(ordersDF)


# 3.3 Roosterdiensten
aggr_plot <- aggr(roosterdienstenDF,oma = c(8,5,5,3), col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(roosterdienstenDF), cex.axis=.8, 
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Roosterdiensten","Combinatie"))

# 3.4 Tijdschrijven
aggr_plot <- aggr(tijdschrijvenDF,oma = c(8,5,5,3), col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(tijdschrijvenDF), cex.axis=.8, 
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Tijdschrijven","Combinatie"))

# 3.5 Workflow
aggr_plot <- aggr(workflowDF,oma = c(8,5,5,3), col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(workflowDF), cex.axis=.8, 
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Workflow","Combinatie"))

# 3.3
# 4. Overcompleetheid, dataoverload
# 5. Komen alle features voor die nodig zijn om een analyse te doen
# ..
# Consistentie
# 1. Komt elke ordernummer van het tijdschrijven voor in de workflowDF, en vice versa
# 2. Alle relaties twee kanten op
#   a. order zonder workflow
#   b. workflow zonder order
#   c. tijdschrijven zonder medewerker
#   d. medewerker zonder tijdschrijven
#   e. tijdschrijven zonder dienst
#   f. dienst zonder tijdschrijven
#   g. tijdschrijven zonder order
#   h. order zonder tijdschrijven
# 3. Is bij tijdschrijven, de begintijd altijd kleiner dan de eindtijd
# 4. Bevinden de begintijd en eindtijd van het tijdschrijven zich binnen de begintijd en eindtijd van het rooster
# 5. De relatie tussen rooster en medewerker en tijdschrijven en medewerker loopt "dubbel". Komen daar inconsistenties in voor
# 6. Datumvelden als datum te behandelen ⇒ OK
# 7. Zijn de order oplopend
# 8. Naamgeving kolommen
# 9. Controle postcode-plaatsnaam zowel voor persoon als order
# ..
# Uniqueness
# 1. Geen dubbele records
# 2. Zijn er medewerkers die tegelijkertijd aan meerdere orders werken ?
# 3. Zijn er medewerkers die tegelijkertijd voor meerdere orders reizen?
# 4. Overlappen de workflowstappen (kan zijn hoor)
# ..
# Validiteit (plausibiliteit/business rules)
# 1. Is er daarbinnen nog verschil tussen werk en reistijd? Hoe gaan we om met overwerk?
# 2. Vervallen orders waar wel geakkoordeerd is tijdgeschreven
# 3. Anomaly detection/outlier verklaring.. 
dfAnomalize <- summarizeOrderTijdschrijvenByOrderDF[,c("OverschreidingUitersteHersteltijd", "EindtijdTijdschrijven")]
dfAnomalizeCompleteCases <- dfAnomalize[complete.cases(dfAnomalize), ]

# implement the “anomalize” (anomaly detection) workflow
#https://www.r-bloggers.com/anomalize-tidy-anomaly-detection/
dfAnomalizeCompleteCases %>%
  time_decompose(OverschreidingUitersteHersteltijd) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

dfAnomalizeCompleteCases %>%
  time_decompose(OverschreidingUitersteHersteltijd, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  # Plot Anomaly Decomposition
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("Anomalies gedetecteerd in UitersteHersteltijd")



btc_ts <- 
  time_decompose(as_tibble(summarizeOrderTijdschrijvenByOrderDF$EindtijdTijdschrijven)) %>%
  anomalize(summarizeOrderTijdschrijvenByOrderDF$OverschreidingUitersteHersteltijd) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
# 4. Inventarisatie business rules
# 5. Heeft medewerker altijd een woonplaats
# 6. Heeft elke order een plaats
# 7. Orders die binnen een periode 'x' (vb een minuut) zijn uitgevoerd
# 8. Verdeling van de reistijd over de medewerkers
# 9. Aantal tijdschrijvers per order
# 10. Verhouding aantal tijdschrijvers tov normtijd (outlier??)
# 11. Tijdschrijvers ten opzichte van het order-klantteam (klantenteams VWT NOC geen tijdschrijvers??)
# 
# 
# 
# Accuraatheid
# 1. Verdeling aantal orders per over de tijd uitgezet.(Heatmap timeline )
# 
# Analyses voor normtijden
# 1. Reistijd tussen verschillende orders met zelfde plaats 
# 2. Gezien door de tijd
# 3. Zelfde woonplaats medewerker en order plaats
# 4. Meetbaar datakwaliteit
# 5. Score opstellen a.d.v. bovenstaande bolletjes
# 6. Verschil AP/EP (ander personeel/eigen personeel)
# 

