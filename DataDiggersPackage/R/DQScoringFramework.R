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
  invisible()
}

#' A Function to transpose a data based on some parameters
#'
#' This function blablablabla
#' @param categorie categorie
#' @param waarde waarde 1-5
#' @param weging weging van het item
#' @keywords melt dcast transposed
#' @import tidyverse
#' @export
#' @examples
#' addScoreToDQFramework(COMPLEETHEID, 3, 5)
addScoreToDQFramework <- function(categorie, waarde, weging){
  
  stopifnot(categorie %in% c(COMPLEETHEID, CONSISTENTIE, UNICITEIT, VALIDITEIT, ACCURAATHEID))
  stopifnot(waarde >= 0)
  stopifnot(waarde <= 5)
  df <- data.frame(categorie, waarde, weging)
  names(df) <- c ("categorie", "waarde", "weging")
  
  DQScoringDF <- tryCatch({get("DQScoringDF", envir=.DataDiggersPackageOptions)}
                          , warning = function(w){
                            initializeDQScoringFramework()
                            return (get("DQScoringDF", envir=.DataDiggersPackageOptions))
                            }
                          , error = function(e){
                            initializeDQScoringFramework()
                            return (get("DQScoringDF", envir=.DataDiggersPackageOptions))
                          }
  )
  # if (is.null(DQScoringDF)){
  #   initializeDQScoringFramework()
  #   DQScoringDF <- get("DQScoringDF", envir=.DataDiggersPackageOptions)
  # }
  DQScoringDF <- rbind(DQScoringDF, df)
  assign("DQScoringDF", DQScoringDF,  envir=.DataDiggersPackageOptions)
  futile.logger::flog.debug(DQScoringDF)
  invisible()
}

#' A Function display the results of the DQF
#'
#' This function plots the marvelous DataQuality-dashboard 
#' @keywords DQFramework dashboard
#' @import ggplot2
#' @import tidyverse
#' @importFrom graphics plot title
#' @export
plotDQ <- function(){
  
  DQScoringDF <- get("DQScoringDF", envir=.DataDiggersPackageOptions)
  utils::globalVariables(names(DQScoringDF))
  utils::globalVariables(c("percentage", "group","label", "title"))
  
  df <- DQScoringDF %>%
    group_by(categorie)%>%
    summarise(percentage = round((sum((waarde * weging)/sum(weging))/5),2))%>%
    mutate(categorie = dplyr::recode(as.character(categorie), '1' = "Compleetheid"
                  , '2' = "Consistentie"
                  , '3' = "Uniciteit"
                  , '4' = "Validiteit"
                  , '5' = "Accuraatheid"))
  futile.logger::flog.debug(df)
  df <- df %>% mutate(group=ifelse(percentage <0.25, "red",
               ifelse(percentage>=0.25 & percentage<0.8, "orange","green")),
              label=paste0(percentage*100, "%"),
                      title=categorie)

  ggplot(df, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
    geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
    geom_rect() + 
    coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
    geom_text(aes(x = 0, y = 0, label = label, colour=group), size=6.5, family="Poppins SemiBold") +
    geom_text(aes(x=1.5, y=1.5, label=title), family="Poppins Light", size=4.2) + 
    facet_wrap(~title, ncol = 5) +
    theme_void() +
    scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
    scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill=FALSE) +
    guides(colour=FALSE)
}