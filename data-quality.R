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
# voeg boolean veld toe dat aangeeft op de plaatsnaam leeg is
medewerkersPieDF <- prep.medewerkersDF %>%
  mutate(PlaatsEmpty = ifelse(is.na(MDWPlaats), TRUE, FALSE))

# # creëer dataframe voor pie chart
dataPie <- medewerkersPieDF %>%
  group_by(PlaatsEmpty) %>%
  count() %>%
  ungroup() %>%
  mutate(per=`n`/sum(`n`)) %>%
  arrange(desc(PlaatsEmpty))
dataPie$label <- scales::percent(dataPie$per)

# # creëer  pie chart
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

cons.order_workflow <- anti_join(prep.ordersDF, prep.workflowDF, by =  c("Ordernummer" = "Ordernummer")) %>%
  filter(OrderStatus != "Gesloten") %>%
  group_by(Afmeldcode) %>%
  summarize(aantal = n())

# Antwoord 92 van de 147.441 orders kennen geen workflow (0,062%), mogelijk "Zwevende orders" indien niet gesloten...
# <> Gesloten orders

cons.order_workflow$Vergelijking      <- c("1. order_workflow")
cons.order_workflow <- cons.order_workflow %>%
  select(Vergelijking, aantal)

TotaalDF <- prep.ordersDF %>%
  group_by(Ordernummer) %>%
  summarize(aantal = n()) %>%
  count(aantal)

totaal <- TotaalDF$n

cons.order_workflow$Totaal_aantal <- c(totaal)
cons.order_workflow$Omschrijving  <- c("Orders kennen geen workflow en zijn niet gesloten, mogelijk zwevende orders")

#   b. workflow zonder order

cons.workflow_order <- anti_join(prep.workflowDF, prep.ordersDF, by =  c("Ordernummer" = "Ordernummer")) %>%
  group_by(Ordernummer) %>%
  summarize(aantal = n())

totaal <- cons.workflow_order$aantal
if (is_empty(totaal) == TRUE) cons.workflow_order<-data.frame(Ordernummer="Onbekend",aantal=0) else totaal

totaal <- prep.workflowDF %>%
  summarize(aantal = n()) %>%
  count(aantal)


totaal <- totaal$aantal

cons.workflow_order$Totaal_aantal      <- c(totaal)
cons.workflow_order$Vergelijking      <- c("2. workflow_order")
cons.workflow_order$Omschrijving      <- c("Alle workflows hebben een order")

cons.workflow_order <- cons.workflow_order %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving)

cons.output <- rbind(cons.order_workflow, cons.workflow_order)


#   c. tijdschrijven zonder medewerker
cons.tijdschrijven_medewerkers <- anti_join(prep.tijdschrijvenDF, prep.medewerkersDF, by =  c("MDWID" = "MDWID")) %>%
  group_by(MDWID) %>%
  summarize(aantal = n()) %>%
  arrange(desc(MDWID))

totaal <- cons.tijdschrijven_medewerkers$aantal
if (is_empty(totaal) == TRUE) cons.tijdschrijven_medewerkers<-data.frame(MDWID="Onbekend",aantal=0) else totaal

totaal <- prep.tijdschrijvenDF %>%
  summarize(aantal = n()) %>%
  count(aantal)

totaal <- totaal$aantal

cons.tijdschrijven_medewerkers$Totaal_aantal      <- c(totaal)
cons.tijdschrijven_medewerkers$Vergelijking      <- c("3. tijdschrijven_medewerkers")
cons.tijdschrijven_medewerkers$Omschrijving      <- c("7 tijdschrijven van 1 medewerker die niet voorkomst in medewerkers")

cons.tijdschrijven_medewerkers <- cons.tijdschrijven_medewerkers %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving)

cons.output <- rbind(cons.output, cons.tijdschrijven_medewerkers)

# Antwoord: 7 tijdschrijven van 1 medewerker die niet voorkomst in medewerkers


#   d. medewerker zonder tijdschrijven
cons.medewerkers_tijdschrijven <- anti_join(prep.medewerkersDF, prep.tijdschrijvenDF, by =  c("MDWID" = "MDWID")) %>%
  group_by(MoetUrenSchrijven) %>%
  summarize(aantal = n()) %>%
  arrange(desc(MoetUrenSchrijven)) %>%
  filter(MoetUrenSchrijven == "WAAR")

totaal <- cons.medewerkers_tijdschrijven$aantal
if (is_empty(totaal) == TRUE) cons.medewerkers_tijdschrijven<-data.frame(MDWID="Onbekend",aantal=0) else totaal

totaal <- prep.medewerkersDF %>%
  summarize(aantal = n()) %>%
  count(aantal)

totaal <- totaal$aantal

cons.medewerkers_tijdschrijven$Totaal_aantal      <- c(totaal)
cons.medewerkers_tijdschrijven$Vergelijking      <- c("4. medewerkers_tijdschrijven")
cons.medewerkers_tijdschrijven$Omschrijving      <- c("Niet alle medewerkers hebben tijdgeschreven die dit wil zouden moeten")

cons.medewerkers_tijdschrijven <- cons.medewerkers_tijdschrijven %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving)

cons.output <- rbind(cons.output, cons.medewerkers_tijdschrijven)

# Antwoord 379 komen niet voor in tijdschrijven, maar een deel hoeft ook geen tijd te schrijven (363 MDWID). 16 moeten dit wel maar komen er niet in voor

#   e. tijdschrijven zonder dienst
cons.tijdschrijven_roosterdienst <- anti_join(prep.tijdschrijvenDF, prep.roosterdienstenDF, by =  c("DienstID" = "DienstID")) %>%
  summarize(aantal = n())

totaal <- cons.tijdschrijven_roosterdienst$aantal
if (is_empty(totaal) == TRUE) cons.tijdschrijven_roosterdienst <-data.frame(MDWID="Onbekend",aantal=0) else totaal

totaal <- prep.tijdschrijvenDF %>%
  summarize(aantal = n()) %>%
  count(aantal)

totaal <- totaal$aantal

cons.tijdschrijven_roosterdienst$Totaal_aantal      <- c(totaal)
cons.tijdschrijven_roosterdienst$Vergelijking      <- c("5. tijdschrijven_roosterdienst")
cons.tijdschrijven_roosterdienst$Omschrijving      <- c("Tijdschrijven zonder roosterdienst en betreft niet alleen verlof ook orders...")

cons.tijdschrijven_roosterdienst <- cons.tijdschrijven_roosterdienst %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving)

cons.output <- rbind(cons.output, cons.tijdschrijven_roosterdienst)

# Antwoord 1391 tijdschrijven zonder roosterdienst en betreft niet alleen verlof ook orders...tw

#   f. dienst zonder tijdschrijven
cons.roosterdienst_tijdschrijven <- anti_join(prep.roosterdienstenDF, prep.tijdschrijvenDF , by =  c("DienstID" = "DienstID")) %>%
  summarize(aantal = n())

# Antwoord 43.729 roosterdiensten zonder tijdschrijven...
# Mogelijk komt dit doordat er een deel op ETL worden verrekend, zie ook orders zonder tijdschrijven

cons.roosterdienst_tijdschrijven2 <- anti_join(prep.roosterdienstenDF, prep.tijdschrijvenDF , by =  c("DienstID" = "DienstID"))
cons.roosterdienst_tijdschrijven2 <- left_join(cons.roosterdienst_tijdschrijven2, prep.medewerkersDF, by =  c("MDWID" = "MDWID"))

cons.roosterdienst_tijdschrijven2$EPAP <-as.character(cons.roosterdienst_tijdschrijven2$EPAP)

cons.roosterdienst_tijdschrijven2$EPAP <- cons.roosterdienst_tijdschrijven2$EPAP %>% 
  replace_na("Onbekend")

cons.roosterdienst_tijdschrijven2 <- cons.roosterdienst_tijdschrijven2 %>%
  filter(EPAP != "AP")

cons.roosterdienst_tijdschrijven2 <- cons.roosterdienst_tijdschrijven2 %>%
  summarize(aantal = n())

totaal <- sum(cons.roosterdienst_tijdschrijven2$aantal)
if (is_empty(totaal) == TRUE){
  cons.roosterdienst_tijdschrijven <-data.frame(MDWID="Onbekend",aantal=0)
}  else {
  totaal
}

totaal <- prep.roosterdienstenDF %>%
  summarize(aantal = n()) %>%
  count(aantal)

totaal <- totaal$aantal

cons.roosterdienst_tijdschrijven2$Totaal_aantal <- c(totaal)
cons.roosterdienst_tijdschrijven2$Vergelijking  <- c("6. roosterdienst_tijdschrijven")
cons.roosterdienst_tijdschrijven2$Omschrijving  <- c("Roosterdiensten zonder tijdschrijven")

cons.roosterdienst_tijdschrijven2 <- cons.roosterdienst_tijdschrijven2 %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving)

cons.output <- rbind(cons.output, cons.roosterdienst_tijdschrijven2)

#   g. tijdschrijven zonder order
cons.tijdschrijven_orders <- anti_join(prep.tijdschrijvenDF, prep.ordersDF, by =  c("ERPID" = "Ordernummer")) %>%
  filter(ERPID != "") %>%
  group_by(Afspraak) %>%
  summarize(aantal = n()) %>%
  arrange(desc(Afspraak))


# Antwoord wel tijdschrijven zonder order maar dit allemaal algemeen (Zie tabel). Tijdschrijven met ordernummer die niet voor komt in order is 0


totaal <- cons.tijdschrijven_orders$aantal
if (is_empty(totaal) == TRUE) cons.tijdschrijven_orders <-data.frame(ERPID="Onbekend",aantal=0) else totaal

totaal <- prep.tijdschrijvenDF %>%
  summarize(aantal = n()) %>%
  count(aantal)

totaal <- totaal$aantal

cons.tijdschrijven_orders$Totaal_aantal      <- c(totaal)
cons.tijdschrijven_orders$Vergelijking      <- c("7. tijdschrijven_orders")
cons.tijdschrijven_orders$Omschrijving      <- c("Tijdschrijven (anders dan algemeen, verlof etc..) op order waarbij de order niet gevonden kan worden")

cons.tijdschrijven_orders <- cons.tijdschrijven_orders %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving)

cons.output <- rbind(cons.output, cons.tijdschrijven_orders)


#   h. order zonder tijdschrijven

cons.orders_tijdschrijven <- anti_join(prep.ordersDF, prep.tijdschrijvenDF, by =  c("Ordernummer" = "ERPID")) %>%
  filter(Ordernummer != "")

# Antwoord er zijn 56.778 orders zonder tijdschrijven, dit kan echter omdat er ook op ETL wordt ingekocht. 
# Zie ook ook Roosterdiensten zonder tijdschrijven
# Kijken of van deze 56.778 orders het allemaal AP zijn

cons.orders_tijdschrijven <- anti_join(prep.ordersDF, prep.tijdschrijvenDF, by =  c("Ordernummer" = "ERPID")) %>%
  group_by(OpdrachtVervalt) %>%
  summarize(aantal = n()) %>%
  arrange(desc(OpdrachtVervalt)) %>%
  # Vanuitgaande dat op vervallen opdrachten geen tijd hoeft te worden geschreven...
  filter(OpdrachtVervalt == "ONWAAR")


totaal <- cons.orders_tijdschrijven$aantal
if (is_empty(totaal) == TRUE) cons.orders_tijdschrijven <-data.frame(OpdrachtVervalt="Onbekend",aantal=0) else totaal

totaal <- prep.ordersDF %>%
  summarize(aantal = n()) %>%
  count(aantal)

totaal <- totaal$aantal

cons.orders_tijdschrijven$Totaal_aantal      <- c(totaal)
cons.orders_tijdschrijven$Vergelijking      <- c("8. orders_tijdschrijven")
cons.orders_tijdschrijven$Omschrijving      <- c("Orders waarvoor geen tijd geschreven is")

cons.orders_tijdschrijven <- cons.orders_tijdschrijven %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving)

cons.output <- rbind(cons.output, cons.orders_tijdschrijven)

cons.output$Kwaliteit <- 100-(cons.output$aantal/cons.output$Totaal_aantal)*100
cons.output$Kwaliteit <- round(cons.output$Kwaliteit, digits = 2)
cons.output$nr. <- substr(cons.output$Vergelijking, 1, 1)


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


# roosterdiensten zijn niet gelijk verdeeld

## 5. De relatie tussen rooster en medewerker en tijdschrijven en medewerker loopt "dubbel". Komen daar inconsistenties in voor
t1DF <- prep.tijdschrijvenDF %>%
  anti_join(prep.roosterdienstenDF)
# 1391 veel maar niet alle roosterdiensten leeg.

ggplot(t1DF, aes(x=StartDate))+
  geom_histogram(color = kleuren[1], fill=kleuren[1])
# over het algemeen redelijk verdeeld maar uitschieter aan het einde van de data


# 6. Datumvelden als datum te behandelen ⇒ OK
# 7. Zijn de order oplopend
cons.orders_nummering <- prep.ordersDF %>%
  select(Ordernummer)%>%
  arrange(Ordernummer)

cons.orders_nummering$nr <- substr(cons.orders_nummering$Ordernummer, 2, 8)
cons.orders_nummering$nr <- as.numeric(cons.orders_nummering$nr)

order_range <- data.frame(c(min(cons.orders_nummering$nr):max(cons.orders_nummering$nr)))
colnames(order_range)[1] <- "nr"

missing_orders <- anti_join(order_range, cons.orders_nummering, by =  c("nr" = "nr"))

#### Tijdframe van de missing_orders ####

# Dichtsbijzijnde order van missende orders
a <- missing_orders$nr
b <- cons.orders_nummering$nr
cuts <- c(-Inf, b[-1]-diff(b)/2, Inf)

match <- cut(a, breaks=cuts, labels=b)
match <- unfactor(match)
match <- unname(match)
missing_orders$match <- match

# Creatiedatum van dichtsbijzijnde order
orders <- prep.ordersDF %>%
  select(Ordernummer, CreationDate)%>%
  arrange(Ordernummer)
orders$nr <- substr(orders$Ordernummer, 2, 8)
orders$nr <- as.numeric(orders$nr)
colnames(orders)[1] <- "match.ordernummer"

missing_orders <- left_join(missing_orders, orders, by =  c("match" = "nr"))
missing_orders[4] <- as.Date(missing_orders$CreationDate)
missing_orders$text_length <- nchar(missing_orders$nr)

toevoeging <- data.frame(text_length = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                         toevoeging = c("NB", "B000000", "B00000", "B0000", "B000","B00","B0", "B", ""))

missing_orders <- left_join(missing_orders, toevoeging, by =  c("text_length" = "text_length"))
missing_orders$ordernummer <- paste0(unfactor(missing_orders$toevoeging),as.character(missing_orders$nr))
missing_orders <- missing_orders %>%
  select(ordernummer, match.ordernummer, CreationDate)


# Tijdlijn missende orders 
library(timelineS)

graphic <- missing_orders %>%
  select(ordernummer, CreationDate) %>% 
  group_by(CreationDate) %>%
  summarize(aantal = n()) 

colnames(graphic)[2] <- "Events"
colnames(graphic)[1] <- "Event_Dates"

levels <- graphic %>%
  select(Events) %>% 
  group_by(Events) %>%
  summarize(aantal = n()) 
levels$join <- levels$Events
levels$Events <- factor(levels$Events, levels = (levels$Events), ordered = TRUE)
levels$Events <- paste0("",levels$Events)

colnames(graphic)[2] <- "join"
graphic <- left_join(graphic, levels, by =  c("join" = "join"))
graphic <- graphic %>%
  select(Events, Event_Dates)

timelineS(graphic, main = "# Missende orders", xlab = NA, buffer.days = 56.2,
          line.width = 2, line.color = "black",
          scale = "year", scale.format = "%Y", scale.font = 2, scale.orient = 1,
          scale.above = FALSE, scale.cex = 1, scale.tickwidth = 2,
          labels = paste(graphic[[1]]), label.direction = "downup",
          label.length = c(0.5,0.5,0.8,0.8), label.position = c(1,3),
          label.color = "gray44", label.cex = 0.8, label.font = 2, label.angle = 0,
          pch = 20, point.cex = 1, point.color = "gray44")

#### totaal Score missende orders ####

# Missende orders = aantal
cons.missing_orders <- missing_orders %>%
  summarize(aantal = n()) %>%
  count(aantal)

# totaal orders = Totaal_aantal
cons.output_missingorders <- prep.ordersDF %>%
  summarize(Totaal_aantal = n()) %>%
  count(Totaal_aantal)

cons.output_missingorders$Vergelijking      <- c("7. Missing_Orders")
cons.output_missingorders$Omschrijving      <- c("Zijn de orders oplopend")
cons.output_missingorders$aantal <- cons.missing_orders$aantal

cons.output_missingorders$Kwaliteit <- 100-(cons.output_missingorders$aantal/cons.output_missingorders$Totaal_aantal)*100
cons.output_missingorders$Kwaliteit <- round(cons.output_missingorders$Kwaliteit, digits = 2)
cons.output_missingorders <- cons.output_missingorders %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving, Kwaliteit)

cons.output_missingorders$nr <- 1




# 8. Naamgeving kolommen
# 9. Controle postcode-plaatsnaam zowel voor persoon als order
cons.order_postcodeplaats <- prep.ordersDF %>%
  select(Ordernummer, Postcode4, Plaats)
cons.order_postcodeplaats$Plaats <- casefold(cons.order_postcodeplaats$Plaats, upper = FALSE)
cons.order_postcodeplaats$Postcode4 <- as.numeric(unfactor(cons.order_postcodeplaats$Postcode4))

# vervangen in orders
cons.order_postcodeplaats$Plaats <- trimws(cons.order_postcodeplaats$Plaats)

# tekensvervangen
cons.order_postcodeplaats$Plaats <- gsub("y", "ij", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub("'", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub("'", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub("-", " ", cons.order_postcodeplaats$Plaats)

# provincie afkortingen verwijderen
cons.order_postcodeplaats$Plaats <- gsub(" gld", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" nb", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" zh", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" nh", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" ov", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" ut", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" lb", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" fr", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" dr", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" gn", "", cons.order_postcodeplaats$Plaats)

# plaatsafhankelijke toevoegingen verwijderen
cons.order_postcodeplaats$Plaats[cons.order_postcodeplaats$Plaats=="'s-gravenhage"] <- "den haag"
cons.order_postcodeplaats$Plaats[cons.order_postcodeplaats$Plaats=="'s gravenhage"] <- "den haag"
cons.order_postcodeplaats$Plaats[cons.order_postcodeplaats$Plaats=="s gravenhage"] <- "den haag"
cons.order_postcodeplaats$Plaats[cons.order_postcodeplaats$Plaats=="'s gravenhage"] <- "den haag"
cons.order_postcodeplaats$Plaats[cons.order_postcodeplaats$Plaats=="sgravenhage"] <- "den haag"
cons.order_postcodeplaats$Plaats <- gsub(" ( limburg )", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" texel", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" gem asten", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" rt", "", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub(" ad ", " aan den ", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub("st o", "sint o", cons.order_postcodeplaats$Plaats)
cons.order_postcodeplaats$Plaats <- gsub("st w", "sint w", cons.order_postcodeplaats$Plaats)

cons.order_postcodeplaats$sleutel <- paste0(cons.order_postcodeplaats$Postcode4, cons.order_postcodeplaats$Plaats)

# join
cons.order_postcodeplaats <- left_join(cons.order_postcodeplaats, org.postcodeplaatsDF, by =  c("sleutel"="sleutel"))
cons.order_postcodeplaats[is.na(cons.order_postcodeplaats)] <- ""

# verschil
cons.order_postcodeplaats$verschil <- ifelse(cons.order_postcodeplaats$Plaats == cons.order_postcodeplaats$woonplaatsnaam,"Nee", "Ja")
cons.order_postcodeplaats <- cons.order_postcodeplaats %>%
  filter(verschil == "Ja")

# Op te lossen verschillen
verschillen <- cons.order_postcodeplaats %>%
  select(Postcode4, Plaats, verschil) %>%
  group_by(Postcode4, Plaats) %>%
  summarize(aantal = n())

#### totaal Score postcode/plaats ####

# Geen match = aantal
cons.order_postcodeplaats <- cons.order_postcodeplaats %>%
  summarize(aantal = n()) %>%
  count(aantal)

# totaal orders = Totaal_aantal
cons.output_postcodeplaats <- prep.ordersDF %>%
  summarize(Totaal_aantal = n()) %>%
  count(Totaal_aantal)

cons.output_postcodeplaats$Vergelijking      <- c("9. postcode_plaats_match")
cons.output_postcodeplaats$Omschrijving      <- c("Kan er bij de postcode dezelfde plaast gevonden worden")
cons.output_postcodeplaats$aantal <- cons.order_postcodeplaats$aantal

cons.output_postcodeplaats$Kwaliteit <- 100-(cons.output_postcodeplaats$aantal/cons.output_postcodeplaats$Totaal_aantal)*100
cons.output_postcodeplaats$Kwaliteit <- round(cons.output_postcodeplaats$Kwaliteit, digits = 2)
cons.output_postcodeplaats <- cons.output_postcodeplaats %>%
  select(Vergelijking, aantal, Totaal_aantal, Omschrijving, Kwaliteit)
cons.output_postcodeplaats$nr <- 1


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

  grid.table(summarized.wfDubbelDF[1:10,])

  # 397654 voorkomens

  # Zie ook procesmining voor verdere analyse

# ..
# Validiteit (plausibiliteit/business rules)
# 1. Is er daarbinnen nog verschil tussen werk en reistijd? Hoe gaan we om met overwerk?
  # vervallen, overwerk al bij 'Consistentie' onderzocht

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
  #Resultaat: er zijn 3107 vervallen orders waarin in totaal 9188 tijdschrijf records voor geregistreerd staan

# 3. Anomaly detection (outliers)
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

# 4. Inventarisatie business rules
# 5. Heeft elke order een plaats

# 6. Orders die binnen een periode 'x' (vb een minuut) zijn uitgevoerd
orderBinnenMinuutDF <- summarized.OrderTijdschrijvenByOrderDF %>%
  filter(difftime(EindtijdTijdschrijven, StarttijdTijdschrijven,units="min")< 1)

#Resultaat: er wordt altijd minimaal 15 minuten tijdgeschreven. Orders duren op basis van tijdschrijven dus nooit korter dan
#           15 minuten.

# 7. Verdeling van de reis- en werktijd over de medewerkers
# geaccordeerde uren per dag per medewerker per type
urenPerDagTotaalDF <- prep.tijdschrijvenDF %>%
  select(MDWID, StartDate, EndDate, Type, Approved) %>%
  filter(Approved == "WAAR") %>%
  filter(as.Date(StartDate) == as.Date(EndDate)) %>%
  mutate(DuurTijdschrijvenInUren = round((EndDate - StartDate),-1)/3600) # round to nearest 10

# aggregeer uren op MDW, startdatum, Type
aggUrenPerDagTotaalDF <- setNames(aggregate(urenPerDagTotaalDF[,c("DuurTijdschrijvenInUren")], by=list(urenPerDagTotaalDF$MDWID, as.Date(urenPerDagTotaalDF$StartDate), urenPerDagTotaalDF$Type), "sum"), c("MDWID", "Datum", "Type", "Duur"))

# boxplot
ggplot(data = aggUrenPerDagTotaalDF, aes(x = Type, y=Duur)) +
  geom_boxplot(fill = kleuren2Items) +
  scale_y_continuous(breaks=0:25) +
  stat_summary(aes(group = Type), fun.y=mean, colour=kleuren2Items, geom="point") +
  labs(title="Verdeling van tijdschrijf uren van medewerkers per Type tijd per dag", y="Uren")

# bereken gemiddelde per Type
library(plyr)
meanTypes <- ddply(aggUrenPerDagTotaalDF, "Type", summarise, grp.mean=mean(Duur))

# toon histogram
ggplot(aggUrenPerDagTotaalDF, aes(x=Duur, color=Type, fill=Type)) +
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
