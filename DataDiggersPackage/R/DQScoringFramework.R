#' Initialisatie van het data-quality-framework
#'
#' Deze funtie initaliseert het framework om datakwaliteit te scoren.
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

#' Een functie om een score aan het framework toe te voegen
#'
#' Een functie om een score aan het framework toe te voegen
#' @param categorie categorie; één van de volgende opties: COMPLEETHEID, CONSISTENTIE, UNICITEIT, VALIDITEIT, ACCURAATHEID
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
  
  # maken nieuw dataframe met nieuwe score
  df <- data.frame(categorie, waarde, weging)
  names(df) <- c ("categorie", "waarde", "weging")
  
  # ophalen huidige dataframe of initaliseren indien het niet bestaat
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

  # combineren huidige en nieuwe dataframe
  DQScoringDF <- rbind(DQScoringDF, df)
  
  # opslaan nieuwe dataframe
  assign("DQScoringDF", DQScoringDF,  envir=.DataDiggersPackageOptions)
  futile.logger::flog.debug(DQScoringDF)
  invisible()
}

#' Een functie die het dataframe resultaat in een dashboard toont
#'
#' Een functie die het dataframe resultaat in een super cool dashboard toont
#' @keywords DQFramework dashboard
#' @import ggplot2
#' @import tidyverse
#' @importFrom graphics plot title
#' @export
plotDQ <- function(){
  
  DQScoringDF <- get("DQScoringDF", envir=.DataDiggersPackageOptions)
  
  plotDF <- DQScoringDF %>%
    group_by(categorie)%>%
    summarise(percentage = round((sum((waarde * weging)/sum(weging))/5),2))%>%
    mutate(categorie = dplyr::recode(as.character(categorie), '1' = "Compleetheid"
                  , '2' = "Consistentie"
                  , '3' = "Uniciteit"
                  , '4' = "Validiteit"
                  , '5' = "Accuraatheid"))
  
  futile.logger::flog.debug(plotDF)
  
  plotDF <- plotDF %>% mutate(group=ifelse(percentage <0.25, "red",
               ifelse(percentage>=0.25 & percentage<0.7, "orange","green")),
              label=paste0(percentage*100, "%"),
                      title=categorie)

  ggplot(plotDF, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
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
