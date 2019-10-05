setwd('D:/datafiles')

packages <- c("elastic"
              ,"tidyverse"
              ,"dplyr"
              ,"sf"
              ,"futile.logger"
              ,"lubridate"
#              ,"RJDBC"
              ,"shiny")

## Installeer packages
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, repos = 'http://cran.us.r-project.org')
  }
}

## Laad the packages
for (p in packages){
  suppressPackageStartupMessages(
    library(p, quietly = TRUE, character.only = TRUE ) 
  )
}


medewerkersDF <- read.csv2("Medewerkers.csv")
ordersDF      <- read.csv2("Orders.csv")
roosterdienstenDF <- read.csv2("Roosterdiensten.csv")
tijdschrijvenDF <- read.csv2("Tijdschrijven.csv") 
workflowDF <- rbind(read.csv2("Workflow 1.csv"),read.csv2("Workflow 2.csv"))

colnames(medewerkersDF)

colnames(ordersDF)

colnames(roosterdienstenDF)

colnames(workflowDF)

head(workflowDF)
head(ordersDF)

write_rds(medewerkersDF, "medewerkers.rds")
write_rds(ordersDF, "orders.rds")
write_rds(roosterdienstenDF, "roosterdiensten.rds")
write_rds(tijdschrijvenDF, "tijdschrijven.rds")
write_rds(workflowDF, "workflow.rds")




# joins
## Diensten met medewerkers
dienstMedewerkersDF <- left_join(roosterdienstenDF, medewerkersDF)
## tijdschrijven met diensten en medewerkers
tijdschrijvenDienstMedewerkersDF <- left_join(tijdschrijvenDF, dienstMedewerkersDF)
## workflow actifiteiten met orders
workflowOrdersDF <- left_join(workflowDF, ordersDF)

## 
workflowDienstMedewerkersDF <- left_join(workflowDF, dienstMedewerkersDF)

