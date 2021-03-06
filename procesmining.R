source("./init.R")
library(DataDiggersPackage)
library(processanimateR)
library(eventdataR)
library(bupaR)
library(pm4py)
library(heuristicsmineR)
library(petrinetR)

flog.threshold(DEBUG)

startPreparation(workdir = "D:/datafiles2", dataframesToGlobalEnvironment = TRUE, rebuild = FALSE)

setWorkdir("D:/datafiles2")
procesMining.init(as.POSIXct("2019-05-01 00:00:00", tz="UTC")
                  , as.POSIXct("2019-05-08 00:00:00", tz="UTC")
                  , c(0.3,  0.6,  0.9))

cnet.0.9.NLS.plot
cnet.0.6.NLS.plot
cnet.0.3.NLS.plot
cnet.0.9.Schade.plot
cnet.0.9.Storing.plot
cnet.0.6.Storing.plot
cnet.0.3.Storing.plot

processmonitR::activity_dashboard(eventlog.Schade)
processmonitR::performance_dashboard(eventlog.Schade)

lEventlog <- eventlog.Storing%>%
  filter_activity_frequency(percentage = .65)

lEventlog%>%
  dotted_chart( x="absolute", sort="start" )

lEventlog1 <- eventlog.Storing%>%
  filter_activity_frequency(percentage = .95)

lEventlog2 <- eventlog.Storing%>%
  anti_join(lEventlog1)


animate_process(lEventlog, mode = "absolute", duration = 300
                , mapping = token_aes(color = token_scale("resource_id", 
                                                        scale = "ordinal", 
                                                        range = RColorBrewer::brewer.pal(7, "Paired"))))


animate_process(eventlog.Schade, mode = "relative", jitter = 10, legend = "color",
                mapping = token_aes(color = token_scale("employee", 
                                                        scale = "ordinal", 
                                                        range = RColorBrewer::brewer.pal(7, "Paired"))))
