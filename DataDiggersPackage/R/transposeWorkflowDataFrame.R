#'  A Function to transpose a data based on some parameters
#'
#' This function blablablabla
#' @param data the dataframe that has to be transposed
#' @param meaureVars a list of measured values
#' @param dcastFormula the formula that is used to transpose via dcast
#' @keywords melt dcast transposed
#' @export
#' @import tidyverse
#' @import naniar
#' @import dplyr
#' @import caret
#' @import doSNOW
#' @import sf
#' @import futile.logger
#' @import openxlsx
#' @import anomalize
#' @import VIM
#' @import reshape2
#' @import shiny
#' @import naniar
#' @import ggplot2
#' @import tidyquant
#' @import sqldf
#' @examples
#' transposeWorkflowDataFrame( workflowDF, c("measure1", "measure2"),)
transposeWorkflowDataFrame <- function (data, measureVars, dcastFormula){
  # converteren posix naar numeric
  for (i in names(data)){
    if(is.POSIXct(data[,i])){
      flog.debug(i)
      data[,i] = unclass(data[,i])
      flog.debug(class(data[,i]))
    }
  }
  flog.debug(all.vars(dcastFormula))
  flog.debug(colnames(data))
  meltDF <- melt(data, id.vars = all.vars(dcastFormula), measure.vars = measureVars)
  flog.debug(colnames(meltDF))
  dcastFormula <- update(dcastFormula, ~.+variable)
  tempDF <- dcast(meltDF, dcastFormula, value.var = "value")
  # converteren naar posix
  for (i in names(tempDF)){
    if(is.numeric(tempDF[,i])){
      if(!str_detect(i, "NormDoorlooptijd")){
        flog.debug(i)
        tempDF[,i] = as.POSIXct(tempDF[,i], origin="1970-01-01")
      }
    }
  }
  tempDF
}
