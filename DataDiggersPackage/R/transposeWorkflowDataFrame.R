#'  A Function to transpose a data based on some parameters
#'
#' This function blablablabla
#' @param data the dataframe that has to be transposed
#' @param measureVars a list of measured values
#' @param dcastFormula the formula that is used to transpose via dcast
#' @keywords melt dcast transposed
#' @export
#' @importFrom futile.logger flog.debug flog.info
#' @importFrom stringr str_detect
#' @importFrom lubridate is.POSIXct
#' @importFrom reshape2 dcast
#' @ importFrom tidyverse full_join group replace_na case_when left_join melt mutate_all n n_distinct summarize 
#' @importFrom dplyr full_join case_when left_join mutate_all n n_distinct summarize 
#' @ importFrom tidyverse 
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
      if(!str_detect(i, "NormDoorlooptijd")){
        flog.debug(i)
        tempDF[,i] = as.POSIXct(tempDF[,i], origin="1970-01-01")
      }
    }
  }
  tempDF
}

