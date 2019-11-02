#'  A Function to transpose a data based on some parameters
#'
#' Deze functie kantel het ingegeven dataframe en bewaard hierbij de ingegeven meetwaarden.
#' @param data het dataframe dat bewerkt wordt
#' @param measureVars lijst met variabelen die worden gemeten
#' @param dcastFormula de formule die gebruikt wordt voor het transponeren van het dataframe
#' @keywords melt dcast transposed
#' @export
#' @importFrom futile.logger flog.debug flog.info
#' @importFrom stringr str_detect
#' @importFrom lubridate is.POSIXct
#' @importFrom reshape2 dcast
#' @importFrom dplyr full_join case_when left_join mutate_all n n_distinct summarize 
#' @importFrom stats update.formula
#' @importFrom reshape2 melt
#' 
#' 
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
  dcastFormula <- update.formula(dcastFormula, ~.+variable)
  tempDF <- dcast(meltDF, dcastFormula, value.var = "value")
  # converteren naar posix
  for (i in names(tempDF)){
    if(is.numeric(tempDF[,i])){
      # van alle numerieke kolommen datums maken m.u.v. Normdoorlooptijd
      if(!str_detect(i, "NormDoorlooptijd")){
        flog.debug(i)
        tempDF[,i] = as.POSIXct(tempDF[,i], origin="1970-01-01")
      }
    }
  }
  tempDF
}

