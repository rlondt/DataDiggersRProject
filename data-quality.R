if (file.exists('./init.R')){
  source('./init.R')
}

ordersWorkflowDF <- read_rds("ordersWorkflowDF.rds")
ordersTijdschrijvenDF <- read_rds("ordersTijdschrijvenDF.rds")



# Compleetheid
# 1. Mogelijkheden voor kruistellingen
# 2. Zijn alle onderdelen van VWT-data vertegenwoordigd
# 3. Missing values analyse 
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




# 1. completeness

# 2. consistency
# 2.1 empty values
# 2.1.1 medewerker
# replace empty strings with 'NA'
medewerkersDF[medewerkersDF==""]<-NA
aggr_plot <- aggr(medewerkersDF, col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(medewerkersDF), cex.axis=.8, 
                  gap=2, 
                  ylab=c("Missing data Medewerker","Combinatie"))

# 2.1.2 Order
aggr_plot <- aggr(ordersDF, col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(ordersDF), cex.axis=.8, 
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing data Order","Combinatie"))


# 2.1.2 anomaly detection

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

# 3. conformity

# 4. accuracy

# 5. integrity

# 6. timeliness

