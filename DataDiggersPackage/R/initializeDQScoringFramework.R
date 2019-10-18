#' A Function to transpose a data based on some parameters
#'
#' This function blablablabla
#' @keywords init DQ Scoring
#' @export
#' @examples
#' initializeDQScoringFramework()

initializeDQScoringFramework <- function(){
  
  DQScoringDF <- data.frame ( categorie = integer()
                            , waarde = integer()
                            , weging = integer()
                            , stringsAsFactors=FALSE
                            )
  assign("DQScoringDF", DQScoringDF,  envir=.DataDiggersPackageOptions)
  #assign("DQScoringDF", df, envir=pkg.env)
  invisible()
}

#' A Function to transpose a data based on some parameters
#'
#' This function blablablabla
#' @param category
#' @param waarde
#' @param weging
#' @keywords melt dcast transposed
#' @export
#' @examples
#' addScoreToDQFramework(category, waarde, weging)

addScoreToDQFramework <- function(categorie, waarde, weging){
  
  stopifnot(categorie %in% c(COMPLEETHEID, CONSISTENTIE, UNICITEIT, VALIDITEIT, ACCURAATHEID))
  stopifnot(waarde < 1)
  stopifnot(waarde > 5)
  df <- data.frame(categorie, waarde, weging)
    names(df) <- c ("categorie", "waarde", "weging")

    DQScoringDF <- get("DQScoringDF", envir=.DataDiggersPackageOptions)
    DQScoringDF <- rbind(DQScoringDF, df)
    assign("DQScoringDF", DQScoringDF,  envir=.DataDiggersPackageOptions)
    
    futile.logger::flog.debug(DQScoringDF)
    invisible()
}
