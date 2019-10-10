if (file.exists('./init.R')){
  source('./init.R')
}


ordersWorkflowDF <- read_rds("ordersWorkflowDF.rds")
ordersTijdschrijvenDF <- read_rds("ordersTijdschrijvenDF.rds")

# 1. completeness

# 2. consistency
# 2.1 empty values
# 2.1.1 medewerker
# replace empty strings with 'NA'
medewerkersDF[medewerkersDF==""]<-NA
library(VIM)
aggr_plot <- aggr(medewerkersDF, col=c('lightblue','red'), 
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(medewerkersDF), cex.axis=.8, 
                  gap=2, 
                  ylab=c("Missing data Medewerker","Combinatie"))


# 2.1.2 anomaly detection
library(tidyverse)
library(anomalize)

dfAnomalize <- summarizeOrderTijdschrijvenByOrderDF[,c("OverschreidingUitersteHersteltijd", "EindtijdTijdschrijven")]
dfAnomalize2 <- dfAnomalize[complete.cases(dfAnomalize), ]

# implement the “anomalize” (anomaly detection) workflow
#https://www.r-bloggers.com/anomalize-tidy-anomaly-detection/
dfAnomalize2 %>%
  time_decompose(OverschreidingUitersteHersteltijd) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

dfAnomalize2 %>%
  time_decompose(OverschreidingUitersteHersteltijd, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  # Plot Anomaly Decomposition
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("Anomalies gedetecteerd in Eindtijd tijdschrijven")



#Install the devtools package then github packages
install.packages('anomalize')
library(dplyr)
library(anomalize) #tidy anomaly detectiom
btc_ts <- 
  time_decompose(as_tibble(summarizeOrderTijdschrijvenByOrderDF$EindtijdTijdschrijven)) %>%
  anomalize(summarizeOrderTijdschrijvenByOrderDF$OverschreidingUitersteHersteltijd) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

# 3. conformity

# 4. accuracy

# 5. integrity

# 6. timeliness

