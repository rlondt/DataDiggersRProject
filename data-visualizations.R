# init
source('./init.R')
library(DataDiggersPackage)
startPreparation(workdir = "C:/Users/louis/OneDrive/studie/vakken/applied big data/datafiles", dataframesToGlobalEnvironment = TRUE)


# visualisations


hist(as.numeric(summarized.OrderTijdschrijvenByOrderDF$OverschreidingUitersteHersteltijd, units = "hours"))

qplot(as.numeric(summarized.OrderTijdschrijvenByOrderDF$OverschreidingUitersteHersteltijd, units = "days"),
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for overschreiding", 
      xlab = "Overschrijding in uren",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(-40,40)
)

qplot(as.numeric(TotaleSchrijftijdReis, units="hours"), data = summarized.OrderTijdschrijvenByOrderDF,
      geom="histogram",
      binwidth = 1,
      xlim=c(0, 25))

qplot(as.numeric(TotaleSchrijftijdWerk, units="hours"), data = summarized.OrderTijdschrijvenByOrderDF,
      geom="histogram",
      binwidth = 1,
      xlim=c(0, 100))

qplot(as.numeric(TotaleDoorlooptijdVanuitKlant, units="hours"), data = summarized.OrderTijdschrijvenByOrderDF,
      geom="histogram",
      binwidth = 1,
      xlim=c(0, 100))

