% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weather_forecast.R
\name{get_forecast}
\alias{get_forecast}
\title{get_forecast}
\usage{
get_forecast(lieu)
}
\arguments{
\item{lieu}{des coordonnées ou une adresse}
}
\value{
une liste de 2 éléments : une liste de 3 graphiques et une table
}
\description{
Cette fonction permet d'obtenir des prévisions météo facilement lisibles sur
1 semaine, heure par heure, à partir du nom, de l'adresse ou des coordonnées
gps d'un lieu. Elles sont issues de l'API Open Météo.
}
\examples{
get_forecast("Parc des Princes")
get_forecast(c(48.841363,2.253069))

}
