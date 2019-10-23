source('./init.R')
library(DataDiggersPackage)
flog.threshold(DEBUG)
startPreparation(workdir = "D:/datafiles2", dataframesToGlobalEnvironment = TRUE)
initializeDQScoringFramework()


# Compleetheid
# 1. Mogelijkheden voor kruistellingen
addScoreToDQFramework(COMPLEETHEID, waarde=5, weging=5)
addScoreToDQFramework(CONSISTENTIE, waarde=5, weging=5)
addScoreToDQFramework(UNICITEIT, waarde=5, weging=5)
addScoreToDQFramework(ACCURAATHEID, waarde=5, weging=5)


plotDQ()

# 2. Zijn alle onderdelen van VWT-data vertegenwoordigd
# 3. Missing values analyse
# 3.1 Medewerker
aggr_plot <- aggr(medewerkersDF,oma = c(8,5,5,3), col=c('lightblue','red'), 
aggr_plot <- aggr(prep.medewerkersDF,oma = c(8,5,5,3), col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(medewerkersDF), cex.axis=.8, 
                  labels=names(prep.medewerkersDF), cex.axis=.8, 
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

# 4. Heeft medewerker altijd een woonplaats


# pie chart Medewerker plaats (Leeg vs gevuld)
org.medewerkersDF[org.medewerkersDF==""]<-NA #TIJDELIJK TOTDAT NA GEFIXED IS
org.medewerkersDF[org.medewerkersDF==" "]<-NA #TIJDELIJK TOTDAT NA GEFIXED IS
dfMDWPie <- org.medewerkersDF %>%
  mutate(PlaatsEmpty = ifelse(is.na(MDWPlaats), TRUE, FALSE))

dataPie <- dfMDWPie %>% 
  group_by(PlaatsEmpty) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(PlaatsEmpty))
dataPie$label <- scales::percent(dataPie$per)
ggplot(data=dataPie)+
  geom_bar(aes(x="", y=per, fill=PlaatsEmpty), stat="identity", width = 1)+
  labs(title="Plaats Medewerker leeg vs gevuld", fill="Plaats leeg") +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

# 5. Overcompleetheid, dataoverload
# 6. Komen alle features voor die nodig zijn om een analyse te doen
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
  addScoreToDQFramework(UNICITEIT, waarde=4.5, weging=3)
# 2. Zijn er medewerkers die tegelijkertijd aan meerdere orders werken?
  t1DF <- prep.tijdschrijvenDF %>%
    mutate(StartDate = unclass(StartDate))%>%
    mutate(EndDate = unclass(EndDate))%>%
    filter(Type == "Werk")%>%
    select(MDWID, ERPID, StartDate, EndDate)

  t2DF <- sqldf("select * 
                from t1DF t1
                ,    t1DF t2 
                on t1.MDWID = t2.MDWID 
                and t1.StartDate > t2.StartDate 
                and t1.StartDate <= t2.EndDate-1
               ")
  # 29 voorkomens
  dumpRDS(t2DF, "dq_unique_2.rds")
  addScoreToDQFramework(UNICITEIT, waarde=4.5, weging=1)
  
  
# 3. Zijn er medewerkers die tegelijkertijd voor meerdere orders reizen?
  t1DF <- prep.tijdschrijvenDF %>%
    mutate(StartDate = unclass(StartDate))%>%
    mutate(EndDate = unclass(EndDate))%>%
    filter(Type != "Werk")%>%
    select(MDWID, ERPID, StartDate, EndDate)
  
  t2DF <- sqldf("select * 
                from t1DF t1
                ,    t1DF t2 
                on t1.MDWID = t2.MDWID 
                and t1.StartDate > t2.StartDate 
                and t1.StartDate <= t2.EndDate-1
               ")
  # 8 voorkomens  
  dumpRDS(t2DF, "dq_unique_3.rds")
  addScoreToDQFramework(UNICITEIT, waarde=5, weging=1)
  
# 4. Overlappen de workflowstappen (kan zijn hoor)
  levels(prep.workflowDF$Status)
  t1DF <- prep.workflowDF %>%
    filter(Status != "Geannuleerd")%>%
    mutate(Starttijd = unclass(Starttijd))%>%
    mutate(Eindtijd  = unclass(WerkelijkeEindtijd))%>%
    select(Ordernummer, Taakomschrijving, Starttijd, Eindtijd)
    
  overlappendeWorkflowstappenDF <- sqldf("select t1.Ordernummer
                , t1.taakomschrijving taakomschrijving_1
                , t2.taakomschrijving taakomschrijving_2
                , t1.Starttijd starttijd_1
                , t1.Eindtijd  eindtijd_1
                , t2.Starttijd starttijd_2
                , t2.Eindtijd  eindtijd_2
                from t1DF t1
                ,    t1DF t2 
                on t1.Ordernummer = t2.Ordernummer 
                and t1.Starttijd > t2.Starttijd 
                and t1.Starttijd <= t2.Eindtijd-1
               ")
  
  dumpRDS(overlappendeWorkflowstappenDF, "dq_unique_4.rds")
  # 397654 voorkomens
  
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
tempDF <- tijdschrijvenDF %>%
  mutate(DuurTijdschrijvenInUren = as.numeric((EndDate - StartDate)/3600),unit="hours")

meanDuurPerMedewerkerType <- aggregate(tempDF$DuurTijdschrijvenInUren, by=list(MDWID=tempDF$MDWID, Type=tempDF$Type), mean)

# boxplot
ggplot(data = meanDuurPerMedewerkerType, aes(x = Type, y=x)) +
  geom_boxplot(fill = "blue", alpha = .2) +
  scale_y_continuous(breaks=0:15) +
  stat_summary(aes(group = Type), fun.y=mean, colour="darkred", geom="point") +
  labs(title="Verdeling gemiddelde van tijdschrijf uren van medewerkers per Type tijd", y="Uren")

# check medewerker with mean 12 hours 'Reis' tijd
checkMDW <- subset(medewerkersDF , MDWID == 'c7990d76-898a-4811-9fca-faa32ffc8386') # Medewerker is 'Uit dienst'
checkTijdMDW <- subset(tijdschrijvenDF , MDWID == 'c7990d76-898a-4811-9fca-faa32ffc8386' & Type =="Reis")
checkOrder <- subset(ordersDF , Ordernummer == 'B0050110') #Plaats=Utrecht

# calculate mean per Type
meanTypes <- ddply(meanDuurPerMedewerkerType, "Type", summarise, grp.mean=mean(x))

# Change line colors by groups
ggplot(meanDuurPerMedewerkerType, aes(x=x, color=Type, fill=Type)) +
  geom_histogram(binwidth=.2, position="dodge") +
  scale_x_continuous(breaks=0:15) +
  geom_vline(data=meanTypes, aes(xintercept=grp.mean, color=Type),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Verdeling gemiddelde van tijdschrijf uren van medewerkers per Type tijd", x="Uren", y="Count")
  theme_classic()

# 9. Aantal tijdschrijvers per order
# 10. Verhouding aantal tijdschrijvers tov normtijd (outlier??)
# 11. Tijdschrijvers ten opzichte van het order-klantteam (klantenteams VWT NOC geen tijdschrijvers??)
# 
# 
# 
# Accuraatheid
# 1. Verdeling aantal orders per over de tijd uitgezet.(Heatmap timeline )

heatmapStarttijdDF <- summarized.WorkflowDF%>%
  filter(!is.na.POSIXlt(Uitvoering_Starttijd))%>%
  mutate(Uitvoering_Starttijd = as.Date(Uitvoering_Starttijd))%>%
  group_by(Uitvoering_Starttijd) %>%
  summarise(aantal = n())

dumpRDS(heatmapStarttijdDF, "dq_accuraatheid_1a.rds")

heatmapEindtijdDF <- summarized.WorkflowDF%>%
  filter(!is.na.POSIXlt(Uitvoering_WerkelijkeEindtijd))%>%
  mutate(Uitvoering_WerkelijkeEindtijd = as.Date(Uitvoering_WerkelijkeEindtijd))%>%
  group_by(Uitvoering_WerkelijkeEindtijd) %>%
  summarise(aantal = n())
dumpRDS(heatmapEindtijdDF, "dq_accuraatheid_1b.rds")

#r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384") 
calendarHeat(tempDF$Uitvoering_Starttijd
             , tempDF$aantal
             , ncolors = 99
             , color = "w2b"
             , varname="Starttijd orders uitvoering")

#r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384") 
calendarHeat(tempDF$Uitvoering_WerkelijkeEindtijd
             , tempDF$aantal
             , ncolors = 99
             , color = "w2b"
             , varname="Werkelijke eindtijd uitvoering")

hist(summarized.WorkflowDF$Uitvoering_Starttijd
#          , tempDF$aantal
, "days"
          )

# 
# Analyses voor normtijden
# 1. Reistijd tussen verschillende orders met zelfde plaats 
# 2. Gezien door de tijd
# 3. Zelfde woonplaats medewerker en order plaats
# 4. Meetbaar datakwaliteit
# 5. Score opstellen a.d.v. bovenstaande bolletjes
# 6. Verschil AP/EP (ander personeel/eigen personeel)
# 

