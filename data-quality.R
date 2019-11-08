source('./init.R')
library(DataDiggersPackage)

# zet logging op debug
flog.threshold(DEBUG)

startPreparation(workdir = "D:/datafiles2", dataframesToGlobalEnvironment = TRUE, rebuild = FALSE)

# gebruik uniforme kleuren
kleuren <- c("#ff6633", "#66ccff", "#ffcc33")
kleuren2Items <- c("#ff6633", "#66ccff")

# Compleetheid
# 1. Mogelijkheden voor kruistellingen
# 2. Zijn alle onderdelen van VWT-data vertegenwoordigd
# 3. Missing values analyse
# 3.1 Medewerker
aggr_plot <- aggr(prep.medewerkersDF,oma = c(8,5,5,3), col=kleuren,
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(prep.medewerkersDF), cex.axis=.8,
                  gap=2,
                  ylab=c("Missing values Medewerker","Combinatie"))

# 3.2 Order
aggr_plot <- aggr(prep.ordersDF,oma = c(8,5,5,3), col=kleuren,
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,combined = TRUE,
                  labels=names(prep.ordersDF), cex.axis=.8,
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Order","Combinatie"))

# alternative
gg_miss_var(prep.ordersDF)


# 3.3 roosterdiensten
aggr_plot <- aggr(prep.roosterdienstenDF,oma = c(8,5,5,3), col=kleuren,
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(prep.roosterdienstenDF), cex.axis=.8,
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Roosterdiensten","Combinatie"))

# 3.4 tijdschrijven
aggr_plot <- aggr(prep.tijdschrijvenDF,oma = c(8,5,5,3), col=kleuren,
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(prep.tijdschrijvenDF), cex.axis=.8,
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Tijdschrijven","Combinatie"))

# 3.5 workflow
aggr_plot <- aggr(prep.workflowDF,oma = c(8,5,5,3), col=kleuren,
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(prep.workflowDF), cex.axis=.8,
                  gap=2, cex.numbers=.5,
                  ylab=c("Missing values Workflow","Combinatie"))

# 4. heeft medewerker altijd een woonplaats?
# Score: 2 (met name door ontbreken van plaatsnaam bij medewerker) !!!Ook al geteld bij 3.
# add field for distincting 'plaats' filled or empty
medewerkersPieDF <- prep.medewerkersDF %>%
  mutate(PlaatsEmpty = ifelse(is.na(MDWPlaats), TRUE, FALSE))

# create dataframe for pie chart
dataPie <- medewerkersPieDF %>%
  group_by(PlaatsEmpty) %>%
  count() %>%
  ungroup() %>%
  mutate(per=`n`/sum(`n`)) %>%
  arrange(desc(PlaatsEmpty))
dataPie$label <- scales::percent(dataPie$per)

# create pie chart
ggplot(data=dataPie)+
  geom_bar(aes(x="", y=per, fill=PlaatsEmpty), stat="identity", width = 1)+
  labs(title="Plaats Medewerker leeg vs gevuld", fill="Plaats leeg") +
  scale_color_manual(values=c("#66ccff","#ff6633")) + #colors for geom_vline
  scale_fill_manual(values=c("#66ccff", "#ff6633")) + #colors for geom_histogram
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))+
  scale_fill_manual(values=kleuren)

addScoreToDQFramework(COMPLEETHEID, waarde=2, weging=4)
# Resultaat: 444 medewerker zonder plaats, 324 met


# 5. Overcompleetheid, dataoverload
# 6. Komen alle features voor die nodig zijn om een analyse te doen
# ..
## Consistentie
# 1. Komt elke ordernummer van het tijdschrijven voor in de workflowDF, en vice versa

OrdersZonderTijdschrijvenZonderWorkflowDF <- prep.ordersDF %>%
  filter(!Ordernummer %in% prep.tijdschrijvenDF$ERPID)%>%
  filter(!Ordernummer %in% prep.workflowDF$Ordernummer)%>%
  filter(OpdrachtVervalt == "ONWAAR")%>%
  mutate(Klantteam = fct_explicit_na(Klantteam, na_level = "Onbekend"))%>%
  mutate(Categorie = fct_explicit_na(Categorie, na_level = "Onbekend"))
# 15

TijdschrijvenZonderWorkflowDF <- prep.tijdschrijvenDF %>%
  filter(!ERPID %in% prep.workflowDF$Ordernummer)%>%
  filter(is.na(ERPID)==FALSE) # Null 265833
# 396

WorkflowZonderTijdschrijvenDF <- prep.workflowDF %>%
  filter(!Ordernummer %in% prep.tijdschrijvenDF$ERPID)%>%
  filter(!Ordernummer %in% prep.ordersDF$Ordernummer)
# 700054 aantal workflow-regels

nrow(table(WorkflowZonderTijdschrijvenDF$Ordernummer)) # 56688 aantal verschillende orders

nrow(table(prep.ordersDF$Ordernummer))   # 147441 unieke orders (gelijk aan aantal records)
nrow(table(prep.workflowDF$Ordernummer)) # 147349 unieke orders
nrow(table(prep.tijdschrijvenDF$ERPID))  #  90663 unieke orders

dumpRDS(WorkflowZonderTijdschrijvenDF, "dq_consistentie_1a.rds")
dumpRDS(TijdschrijvenZonderWorkflowDF, "dq_consistentie_1b.rds")


addScoreToDQFramework(COMPLEETHEID, waarde=1, weging=1)
addScoreToDQFramework(CONSISTENTIE, waarde=4, weging=5)

tFactor <- as.factor(c("Orders (147441)", "Workflows (140349)", "Tijdschrijven (90663)"))

circles <- data.frame(
  names = factor(tFactor, levels = tFactor)
)

ggplot(circles, aes(x = 1, fill = names)) +
  geom_bar(width = 1) +
  coord_polar() +
  xlab("") + ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = FALSE))+
  scale_fill_manual(values=kleuren)


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
dftijdschrijvenEindatumGroterDanBegindatum <- prep.tijdschrijvenDF %>%
  filter(prep.tijdschrijvenDF$StartDate > prep.tijdschrijvenDF$EndDate)

addScoreToDQFramework(CONSISTENTIE, waarde=4, weging=2)
# resultaat: komt niet voor. Wel is het zo dat er 3 tijdschrijf records zijn waar de startdatum en eindddatum gelijk zijn.

# 4. Bevinden de begintijd en eindtijd van het tijdschrijven zich
#    binnen de begintijd en eindtijd van het rooster
dq.tijdschrijvenVoorRoosterDF <- join.tijdschrijvenDienstMedewerkersDF %>%
  filter(is.na(ERPID) == FALSE) %>%
  filter( StartDate < Starttijd)%>%
  #filter( Type == "Werk")%>%
  mutate(periodeTeVroeg = case_when(Starttijd > StartDate~ StartDate-Starttijd)) %>%
  select(ERPID, Type, periodeTeVroeg, Starttijd, StartDate, EndDate, Eindtijd)%>%
  arrange(StartDate)

ggplot(dq.tijdschrijvenVoorRoosterDF, aes(x=Starttijd, fill=Type, color=Type))+
  geom_histogram(position = "stack")+
  scale_color_manual(values=kleuren)+
  scale_fill_manual(values=kleuren)+
  ylab("Aantal")+
  labs(title="Aantal tijdschrijven voor starttijd rooster")

# voorral midden 2017 worden de roostertijden overschreden

dq.tijdschrijvenNaRoosterDF <- join.tijdschrijvenDienstMedewerkersDF %>%
  filter(is.na(ERPID) == FALSE) %>%
  filter(Eindtijd < EndDate)%>%
  mutate(periodeTelaat = case_when(Eindtijd < EndDate~ EndDate- Eindtijd)) %>%
  select(ERPID, Type, Starttijd, StartDate, EndDate, Eindtijd, periodeTelaat)%>%
  arrange(StartDate)

ggplot(dq.tijdschrijvenNaRoosterDF, aes(x=Starttijd, fill=Type, color=Type))+
  geom_histogram(position = "stack")+
  scale_color_manual(values=kleuren)+
  scale_fill_manual(values=kleuren)+
  ylab("Aantal")+
  labs(title="Aantal tijdschrijven na eindtijd rooster")


ggplot(prep.roosterdienstenDF, aes(x=Starttijd))+
  geom_histogram( color = kleuren[1], fill=kleuren[1])+
  scale_color_manual(values=kleuren)+
  scale_fill_manual(values=kleuren)+
  ylab("Aantal")+
  labs(title="Aantal roosterdiensten per starttijd")

ggplot(prep.tijdschrijvenDF, aes(x=StartDate))+
  geom_histogram(color = kleuren[1], fill=kleuren[1])+
  scale_color_manual(values=kleuren)+
  scale_fill_manual(values=kleuren)+
  ylab("Aantal")+
  labs(title="Aantal Tijdschrijven per starttijd")


# roosterdiensten zijn niet gelijjk verdeeld

## 5. De relatie tussen rooster en medewerker en tijdschrijven en medewerker loopt "dubbel". Komen daar inconsistenties in voor
t1DF <- prep.tijdschrijvenDF %>%
  anti_join(prep.roosterdienstenDF)
# 1391 veel maar niet alle roosterdiensten leeg.

ggplot(t1DF, aes(x=StartDate))+
  geom_histogram(color = kleuren[1], fill=kleuren[1])
# over het algemeen redelijk verdeeld maar uitschieter aan het einde van de data


# 6. Datumvelden als datum te behandelen ⇒ OK
# 7. Zijn de order oplopend
# 8. Naamgeving kolommen
# 9. Controle postcode-plaatsnaam zowel voor persoon als order
# ..
# Uniqueness
# 1. Geen dubbele records, functioneel en obv primary key
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
  vervallenOrdersDF <- prep.ordersDF%>%
    filter(OpdrachtVervalt=="WAAR")

  wfDF <- prep.workflowDF %>%
    filter(Status != "Geannuleerd")%>%
    filter(!Ordernummer %in% vervallenOrdersDF$Ordernummer)%>%
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
                from wfDF t1
                ,    wfDF t2
                on t1.Ordernummer = t2.Ordernummer
                and t1.Starttijd > t2.Starttijd
                and t1.Starttijd <= t2.Eindtijd-1
               ")

  summarized.wfDubbelDF <- overlappendeWorkflowstappenDF %>%
    group_by(taakomschrijving_1, taakomschrijving_2)%>%
    summarize(aantal = n())


  dumpRDS(overlappendeWorkflowstappenDF, "dq_unique_4a.rds")
  summarized.wfDubbelDF <-readRDSdd( "dq_unique_4b.rds")

  summarized.wfDubbelDF<- summarized.wfDubbelDF%>%
    arrange(aantal*-1)
  library(grid)
  library(gridExtra)

  grid.table(summarized.wfDubbelDF[1:10,])

  # 397654 voorkomens

  # Zie ook procesmining voor verdere analyse

# ..
# Validiteit (plausibiliteit/business rules)
# 1. Is er daarbinnen nog verschil tussen werk en reistijd? Hoe gaan we om met overwerk?
  # join tijdschrijven en roosterdienst
  # tijdRoosterDF <-  left_join(prep.tijdschrijvenDF, prep.roosterdienstenDF, by=c("DienstID" = "DienstID", "MDWID" = "MDWID")) %>%
  # select(MDWID, StartDate, EndDate, Type)

  # urenPerDagTotaal <- prep.tijdschrijvenDF %>%
  #   select(MDWID, StartDate, EndDate, Type, Approved) %>%
  #   filter(Approved == "WAAR") %>%
  #   filter(as.Date(StartDate) == as.Date(EndDate)) %>%
  # mutate(DuurTijdschrijvenInUren = round((EndDate - StartDate),-1)/3600) # round to nearest 10

  # urenPerDagWerk <- urenPerDagTotaal %>%
  #   filter(Type=="Werk")

  # aggregeer uren op MDW, startdatum, Type
  aggUrenPerDagWerk <- setNames(aggregate(urenPerDagWerk[,c("DuurTijdschrijvenInUren")], by=list(urenPerDag$MDWID, as.Date(urenPerDag$StartDate), urenPerDag$Type), "sum"), c("MDWID", "Datum", "Type", "Duur"))

  # Spreiding tijdschrijf uren medewerkers
   # ggplot(data=aggUrenPerDagWerk, aes(aggUrenPerDagWerk$Duur)) + geom_histogram(binwidth=.5, boundary = 0, color = "black", fill = "lightblue") +
   #   scale_x_continuous(breaks=0:20) +
   # xlab("Tijdschrijf uren medewerkers per dag")

  # filter overwerk (overwerk=urenPerDag>9)
  # TODO: overwerk=overschreiding van roostertijd
  # urenOverwerk <- aggUrenPerDag %>%
  #   filter(Duur > 9)

  # Spreiding overwerk uren medewerkers per dag
  ggplot(data=urenOverwerk, aes(urenOverwerk$Duur)) + geom_histogram(binwidth=.5, boundary = 0, color = "black", fill = "lightblue") +
    scale_x_continuous(breaks=0:20) +
    xlab("Overwerk uren medewerkers per dag")

  #addScoreToDQFramework(COMPLEETHEID, waarde=2, weging=4) wat hier invullen?
  #Resultaat/conclusie: overwerk: uitschieters in aantal uren per dag vooral 10 en 12 uur. Max is 19 uur.

# 2. Vervallen orders waar wel geakkoordeerd is tijdgeschreven
  vervallenOrdersMetTijdschrijvenDF <- left_join(prep.ordersDF, join.tijdschrijvenDienstMedewerkersDF, by=c("Ordernummer" = "ERPID")) %>%
    mutate(DuurTijdschrijvenInSeconden = EndDate - StartDate) %>%
    mutate(VerschilUitersteHerstelEindeTijdschrijvenInSeconden = EndDate - Uiterstehersteltijd) %>%
    filter(Verwerkingsstatus == "Akkoord" | Verwerkingsstatus == "Vrijgegeven ERP") %>%
    filter(OpdrachtVervalt == "WAAR") %>%
    filter(Approved == "WAAR")

  # count distinct values
  vervallenOrdersMetTijdschrijvenDF %>%
    group_by(Ordernummer) %>%
    summarise(n_distinct(Ordernummer))

  #addScoreToDQFramework(COMPLEETHEID, waarde=2, weging=4) wat hier invullen?
  #Resultaat: er zijn 3107 vervallen orders waarin in totaal 9188 tijdschrijf records voor geregistreerd staan

# 3. Anomaly detection/outlier verklaring
  #https://www.datacamp.com/community/tutorials/detect-anomalies-anomalize-r
  anomalyDF <- left_join(summarized.OrderTijdschrijvenByOrderDF, prep.ordersDF, by=c("Ordernummer" = "Ordernummer"))%>%
    arrange(StarttijdTijdschrijven) %>%
    filter(Approved == "WAAR")
    filter(is.na(Ordernummer)==FALSE)%>%
    filter(is.na(Categorie)==FALSE)%>%
    group_by(Ordernummer, Categorie, StarttijdTijdschrijven) %>%
    summarise(werktijd = sum(TotaleSchrijftijdWerk)/3600)

  # sorteer op startdatum tijdschrijven
  anomalyDF <- anomalyDF[order(anomalyDF$StarttijdTijdschrijven),]

  # prepareer anomaly data frames
  anomalyNLSDF <- anomalyDF %>% filter(Categorie=="NLS")
  anomalyNLSDF <- anomalyNLSDF[,c("StarttijdTijdschrijven", "werktijd")]
  anomalySchadeDF <- anomalyDF %>% filter(Categorie=="Schade")
  anomalySchadeDF <- anomalySchadeDF[,c("StarttijdTijdschrijven", "werktijd")]
  anomalyStoringDF <- anomalyDF %>% filter(Categorie=="Storing")
  anomalyStoringDF <- anomalyStoringDF[,c("StarttijdTijdschrijven", "werktijd")]

# anomalies werktijd NLS
  anomalyNLSDF %>%
  time_decompose(werktijd, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
    #scale_color_manual(values=c("#66ccff", "#ff6633")) + #colors for geom_vline
    #scale_fill_manual(values=c("#ff6633", "#66ccff", "#ffcc33")) + #colors for geom_histogram
  xlab("Starttijd tijdschrijven") +
  ylab("Uren") +
  ggtitle("Anomalies werktijd NLS ")

  # anomalies werktijd Schade
  anomalySchadeDF %>%
    time_decompose(werktijd, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.05) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    xlab("Starttijd tijdschrijven") +
    ylab("Uren") +
    ggtitle("Anomalies werktijd Schade")


  # anomalies werktijd Storing
  anomalyStoringDF %>%
    time_decompose(werktijd, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.02) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    xlab("Starttijd tijdschrijven") +
    ylab("Uren") +
    ggtitle("Anomalies werktijd Storing")

  addScoreToDQFramework(VALIDITEIT, waarde=2, weging=3) # veel uitschieters die wrs niet valide zijn, met name bij storing

# 4. Inventarisatie business rules
# 5. Heeft elke order een plaats

# 6. Orders die binnen een periode 'x' (vb een minuut) zijn uitgevoerd
orderBinnenMinuut <- summarized.OrderTijdschrijvenByOrderDF %>%
  filter(difftime(EindtijdTijdschrijven, StarttijdTijdschrijven,units="min")< 1)

# testWF <- prep.workflowDF %>%
#   filter(Ordernummer=='B0036800')
#
# testTijd <- prep.tijdschrijvenDF %>%
#   filter(ERPID=='B0036800')

addScoreToDQFramework(VALIDITEIT, waarde=5, weging=1)
#Resultaat: er wordt altijd minimaal 15 minuten tijdgeschreven. Orders duren op basis van tijdschrijven dus nooit korter dan
#           15 minuten.
# TODO: nog iets doen met doorlooptijd van order (op basis van workflowstappen) vs tijdschrijf tijd?


# 7. Verdeling van de reis- en werktijd over de medewerkers

# geaccordeerde uren per dag per medewerker per type
urenPerDagTotaal <- prep.tijdschrijvenDF %>%
  select(MDWID, StartDate, EndDate, Type, Approved) %>%
  filter(Approved == "WAAR") %>%
  filter(as.Date(StartDate) == as.Date(EndDate)) %>%
  mutate(DuurTijdschrijvenInUren = round((EndDate - StartDate),-1)/3600) # round to nearest 10

# aggregeer uren op MDW, startdatum, Type
aggUrenPerDagTotaal <- setNames(aggregate(urenPerDagTotaal[,c("DuurTijdschrijvenInUren")], by=list(urenPerDagTotaal$MDWID, as.Date(urenPerDagTotaal$StartDate), urenPerDagTotaal$Type), "sum"), c("MDWID", "Datum", "Type", "Duur"))

# boxplot
ggplot(data = aggUrenPerDagTotaal, aes(x = Type, y=Duur)) +
  geom_boxplot(fill = kleuren2Items) +
  scale_y_continuous(breaks=0:25) +
  stat_summary(aes(group = Type), fun.y=mean, colour=kleuren2Items, geom="point") +
  labs(title="Verdeling van tijdschrijf uren van medewerkers per Type tijd per dag", y="Uren")

# check medewerker with mean 15 hours 'Reis' tijd
checkMDW <- subset(prep.medewerkersDF , MDWID == '87b43ab6-2322-4f21-8ff1-ac74b3dc28de') 
checkTijdMDW <- subset(prep.tijdschrijvenDF , MDWID == '87b43ab6-2322-4f21-8ff1-ac74b3dc28de' & Type =="Reis")
checkOrder <- subset(prep.ordersDF , Ordernummer == 'B0050110') #Plaats=Utrecht

# calculate mean per Type
library(plyr)
meanTypes <- ddply(aggUrenPerDagTotaal, "Type", summarise, grp.mean=mean(Duur))

# show histogram
ggplot(aggUrenPerDagTotaal, aes(x=Duur, color=Type, fill=Type)) +
  geom_histogram(binwidth=.5, position="dodge") +
  scale_x_continuous(breaks=0:25) +
  geom_vline(data=meanTypes, aes(xintercept=grp.mean, color=Type),
             linetype="dashed")+
  scale_color_manual(values=c("#ff6633", "#66ccff", "#ffcc33")) + #colors for geom_vline
  scale_fill_manual(values=c("#ff6633", "#66ccff", "#ffcc33")) + #colors for geom_histogram
  labs(title="Verdeling van tijdschrijf uren van medewerkers per Type tijd per dag", x="Uren", y="Count")

# 8. Aantal tijdschrijvers per order



# 9. Verhouding aantal tijdschrijvers tov normtijd (outlier??)




# 10. Tijdschrijvers ten opzichte van het order-klantteam (klantenteams VWT NOC geen tijdschrijvers??)





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

calendarHeat(heatmapStarttijdDF$Uitvoering_Starttijd
             , heatmapStarttijdDF$aantal
             , ncolors = 99
             , color = "kleuren"
             , varname="Starttijd orders uitvoering")

calendarHeat(heatmapEindtijdDF$Uitvoering_WerkelijkeEindtijd
             , heatmapEindtijdDF$aantal
             , ncolors = 99
             , color = "kleuren"
             , varname="Werkelijke eindtijd uitvoering")

# uitvoer dashboard
plotDQ()
