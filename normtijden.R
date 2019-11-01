source("./init.R")
library(DataDiggersPackage)
library(lubridate)
flog.threshold(DEBUG)
startPreparation(workdir = "D:/datafiles2", rebuild = FALSE,  dataframesToGlobalEnvironment = TRUE)


#snel normtijd berekend orders per jaar
prep.ordersDF %>%
  group_by(format(CreationDate, "%Y")) %>%
  summarise(aantal=n(), aantalDagen= n_distinct(format(CreationDate, "%Y%m%d")))

summary(prep.medewerkersDF)


##
# Over alle categorieen heen
norm.summary.totaalDF <- summarized.OrderTijdschrijvenByOrderDF %>%
  filter(is.na(Ordernummer)==FALSE)%>%
  group_by(Ordernummer) %>%
  summarise(werktijd = sum(TotaleSchrijftijdWerk)
            , AantalVerschillendeMedewerkers = AantalVerschillendeMedewerkers)%>%
  summarise( gemiddeldeWerktijdPerOrder = mean(werktijd)
             , spreidingWerktijdPerOrder = sd(werktijd)
             , gemiddeldAantalMedewerkersPerOrder = mean(AantalVerschillendeMedewerkers)
             , spreidingAantalMedewerkersPerOrdePerOrder = sd(AantalVerschillendeMedewerkers)
             , aantal = n())%>%
  mutate(gemiddeldeWerktijdPerOrder = as.double(gemiddeldeWerktijdPerOrder, units="hours"))%>%
  mutate(spreidingWerktijdPerOrder = as.double(spreidingWerktijdPerOrder)/(60*60))

dumpRDS(norm.summary.totaalDF, "norm_summary_totaalDF.rds")  


##
# per categorie
norm.summary.perCategorieDF <- left_join(summarized.OrderTijdschrijvenByOrderDF, prep.ordersDF, by=c("Ordernummer" = "Ordernummer"))%>%
  filter(is.na(Ordernummer)==FALSE)%>%
  group_by(Ordernummer, Categorie) %>%
  summarise(werktijd = sum(TotaleSchrijftijdWerk)
            , AantalVerschillendeMedewerkers = AantalVerschillendeMedewerkers)%>%
  group_by(Categorie) %>%
  summarise( gemiddeldeWerktijdPerOrder = mean(werktijd)
             , spreidingWerktijdPerOrder = sd(werktijd)
             , gemiddeldAantalMedewerkersPerOrder = mean(AantalVerschillendeMedewerkers)
             , spreidingAantalMedewerkersPerOrdePerOrder = sd(AantalVerschillendeMedewerkers)
             , aantal = n())%>%
  mutate(gemiddeldeWerktijdPerOrder = as.double(gemiddeldeWerktijdPerOrder, units="hours"))%>%
  mutate(spreidingWerktijdPerOrder = as.double(spreidingWerktijdPerOrder)/(60*60))
dumpRDS(norm.summary.perCategorieDF, "norm_summary_perCategorieDF.rds")  


##
# per categorie per maand
norm.summary.perCategoriePerMaandDF <- left_join(summarized.OrderTijdschrijvenByOrderDF, prep.ordersDF, by=c("Ordernummer" = "Ordernummer"))%>%
  filter(is.na(Ordernummer)==FALSE)%>%
  filter(is.na(Categorie)==FALSE)%>%
  mutate(maand = format(CreationDate, "%Y-%m")) %>%
  group_by(Ordernummer, Categorie, maand) %>%
  summarise(werktijd = sum(TotaleSchrijftijdWerk)
            , AantalVerschillendeMedewerkers = AantalVerschillendeMedewerkers)%>%
  group_by(Categorie, maand) %>%
  summarise( gemiddeldeWerktijdPerOrder = mean(werktijd)
             , spreidingWerktijdPerOrder = sd(werktijd)
             , gemiddeldAantalMedewerkersPerOrder = mean(AantalVerschillendeMedewerkers)
             , spreidingAantalMedewerkersPerOrdePerOrder = sd(AantalVerschillendeMedewerkers)
             , aantal = n())%>%
  mutate(gemiddeldeWerktijdPerOrder = as.double(gemiddeldeWerktijdPerOrder, units="hours"))%>%
  mutate(spreidingWerktijdPerOrder = as.double(spreidingWerktijdPerOrder)/(60*60))
dumpRDS(norm.summary.perCategoriePerMaandDF, "norm_summary_perCategoriePerMaandDF.rds")  

##
# per categorie per klantteam per maand
norm.summary.perCategoriePerPerKlantteamMaandDF <- left_join(summarized.OrderTijdschrijvenByOrderDF, prep.ordersDF, by=c("Ordernummer" = "Ordernummer"))%>%
  filter(is.na(Ordernummer)==FALSE)%>%
  filter(is.na(Categorie)==FALSE)%>%
  mutate(maand = format(CreationDate, "%Y-%m")) %>%
  group_by(Ordernummer, Categorie, Klantteam, maand) %>%
  summarise(werktijd = sum(TotaleSchrijftijdWerk)
            , AantalVerschillendeMedewerkers = AantalVerschillendeMedewerkers)%>%
  group_by(Categorie, Klantteam, maand) %>%
  summarise( gemiddeldeWerktijdPerOrder = mean(werktijd)
             , spreidingWerktijdPerOrder = sd(werktijd)
             , gemiddeldAantalMedewerkersPerOrder = mean(AantalVerschillendeMedewerkers)
             , spreidingAantalMedewerkersPerOrdePerOrder = sd(AantalVerschillendeMedewerkers)
             , aantal = n())%>%
  mutate(gemiddeldeWerktijdPerOrder = as.double(gemiddeldeWerktijdPerOrder, units="hours"))%>%
  mutate(spreidingWerktijdPerOrder = as.double(spreidingWerktijdPerOrder)/(60*60))
dumpRDS(norm.summary.perCategoriePerKlantteamPerMaandDF, "norm_summary_perCategoriePerKlantenteamPerMaandDF.rds")  




# Multiple line plot
ggplot(norm.summary.perCategoriePerMaandDF, aes(x = maand, y = gemiddeldeWerktijdPerOrder, fill = Categorie)) + 
  geom_bar(stat="identity",position = position_dodge()) +
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# Multiple line plot
ggplot(norm.summary.perCategoriePerPerKlantteamMaandDF, aes(x = maand, y = gemiddeldeWerktijdPerOrder, fill = Categorie)) +
  geom_bar(stat="identity",position = position_dodge()) +
  scale_y_continuous(limits = c(0,35))+
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  facet_wrap(~Klantteam, nrow = 4)
theme_minimal()


# Multiple plot per category
ggplot(norm.summary.perCategoriePerPerKlantteamMaandDF%>% filter(str_count(maand,"2017")== 0), aes(x = maand, y = gemiddeldeWerktijdPerOrder, fill = Klantteam)) +
  geom_bar(stat="identity",position = position_dodge()) +
  scale_y_continuous(limits = c(0,15))+
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  facet_wrap(~Categorie, nrow = 4)+
  theme(axis.text.x = element_text(face = "bold", color = "blue", 
                                   size = 9, angle = 45, hjust = 1))
theme_minimal()




# Multiple plot per category
ggplot(norm.summary.perCategoriePerPerKlantteamMaandDF%>% filter(str_count(maand,"2017")== 0), aes(x = maand, y = gemiddeldAantalMedewerkersPerOrder, fill = Klantteam)) +
  geom_bar(stat="identity",position = position_dodge()) +
  scale_y_continuous(limits = c(0,4))+
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  facet_wrap(~Categorie, nrow = 4)+
  theme(axis.text.x = element_text(face = "bold", color = "blue", 
                                   size = 9, angle = 45, hjust = 1))

# ggplot(norm.summary.perCategoriePerMaandDF, aes(x = maand, y = gemiddeldeWerktijdPerOrder, fill = Categorie)) + 
#   geom_smooth(stat="identity", )
#   
#   
#   geom_bar(stat="identity",position = position_dodge()) +
#   #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
#   theme_minimal()
# 
# 
# 









norm.ordersDF <-  left_join(summarized.OrderTijdschrijvenByOrderDF, prep.ordersDF, by=c("Ordernummer" = "Ordernummer"))%>%
  mutate(Startdag = as.Date(StarttijdTijdschrijven))%>%
  mutate(Categorie = as.character(Categorie))%>%
  filter(is_na(Categorie) == FALSE)%>%
  group_by(Startdag, Categorie)%>%
  summarize(werktijd = mean(as.double(TotaleSchrijftijdWerk, units="hours")))


# dataframe for deviation visualization
norm.categoryDF <- left_join(summarized.OrderTijdschrijvenByOrderDF, prep.ordersDF, by=c("Ordernummer" = "Ordernummer"))%>%
  filter(is.na(Ordernummer)==FALSE)%>%
  filter(is.na(Categorie)==FALSE)%>%
  group_by(Ordernummer, Categorie) %>%
  summarise(werktijd = sum(TotaleSchrijftijdWerk)/3600)%>%
  filter(werktijd < 150) # filter outliers
  
# boxplot
ggplot(data = norm.categoryDF, aes(x = Categorie, y=werktijd)) +
  scale_y_continuous(limits = c(0, 150)) +
  geom_boxplot(fill = "blue", alpha = .2) +
  stat_summary(aes(group = Categorie), fun.y=mean, colour="darkred", geom="point") +
  labs(title="Verdeling werktijd per order categorie", y="Uren")

# histogram
ggplot(norm.categoryDF, aes(x=werktijd, color=Categorie, fill=Categorie)) +
  geom_histogram(binwidth=1, position="dodge") +
  scale_x_continuous(limits = c(0, 200)) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Verdeling werktijd per order categorie", x="Werktijd", y="Count")


# dataframe voor gemiddelden
meanTypes <- norm.summary.perCategorieDF %>%
  filter(Categorie != 'NA')


  # single chart with deviation chart 
ggplot(norm.categoryDF, aes(x=werktijd, fill=Categorie))+ 
    geom_histogram(bins=50)+
    scale_x_continuous(limits = c(0, 250000/3600)) +
    facet_grid(. ~ Categorie)+
    geom_vline(data=meanTypes, aes(xintercept=meanTypes$gemiddeldeWerktijdPerOrder, color=meanTypes$Categorie), show.legend = FALSE,linetype="dashed") +
    xlab("Werktijd (uren)") +
    ggtitle("Verdeling werktijd per order categorie")
  + theme(legend.position="none")