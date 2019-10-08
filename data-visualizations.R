# init
if (file.exists('./init.R')){
  source('./init.R')
}

# readRDS

summarizeOrderTijdschrijvenByOrderDF <- read_rds("summarizeOrderTijdschrijvenByOrderDF.rds")


# visualisations


hist(as.numeric(summarizeOrderTijdschrijvenByOrderDF$OverschreidingUitersteHersteltijd, units = "hours"))

qplot(as.numeric(summarizeOrderTijdschrijvenByOrderDF$OverschreidingUitersteHersteltijd, units = "days"),
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for overschreiding", 
      xlab = "Overschrijding in uren",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(-40,40)
)

qplot(as.numeric(TotaleSchrijftijdReis, units="hours"), data = summarizeOrderTijdschrijvenByOrderDF,
      geom="histogram",
      binwidth = 1,
      xlim=c(0, 25))

qplot(as.numeric(TotaleSchrijftijdWerk, units="hours"), data = summarizeOrderTijdschrijvenByOrderDF,
      geom="histogram",
      binwidth = 1,
      xlim=c(0, 100))

qplot(as.numeric(TotaleDoorlooptijdVanuitKlant, units="hours"), data = summarizeOrderTijdschrijvenByOrderDF,
      geom="histogram",
      binwidth = 1,
      xlim=c(0, 100))

