% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DQScoringFramework.R
\name{addScoreToDQFramework}
\alias{addScoreToDQFramework}
\title{Een functie om een score aan het framework toe te voegen}
\usage{
addScoreToDQFramework(categorie, waarde, weging)
}
\arguments{
\item{categorie}{categorie; één van de volgende opties: COMPLEETHEID, CONSISTENTIE, UNICITEIT, VALIDITEIT, ACCURAATHEID}

\item{waarde}{waarde 1-5}

\item{weging}{weging van het item}
}
\description{
Een functie om een score aan het framework toe te voegen
}
\examples{
addScoreToDQFramework(COMPLEETHEID, 3, 5)
}
\keyword{dcast}
\keyword{melt}
\keyword{transposed}
