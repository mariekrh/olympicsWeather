

url <- "https://api.open-meteo.com/v1/forecast"

# Fonctions internes

#' perform_request
#'
#'@description
#'Cette fonction permet d'effectuer une requête sur l'API Open Meteo à partir
#'de coordonnées gps. Elle porte sur l'heure, la température, la température
#'ressentie, la probabilité qu'il pleuve et la quantité de pluie prévue sur 1
#'semaine.
#'
#' @param latitude latitude (numeric)
#' @param longitude longitude (numeric)
#'
#' @return un tibble avec 5 lignes
#'
#' @examples
#' perform_request(48.85,2.35)
#'
perform_request <- function(latitude, longitude) {
  url <- "https://api.open-meteo.com/v1/forecast"
  response_table <- httr2::request(url) |>
    httr2::req_url_query(
      latitude = latitude,
      longitude = longitude,
      hourly = c(
        "temperature_2m",
        "apparent_temperature",
        "precipitation_probability",
        "precipitation"
      ),
      .multi = "comma"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    tibble::as_tibble()
  return(response_table)

}



#' unnest_response
#'
#' @description
#' Cette fonction permet d'améliorer la lisibilité des résultats obtenus grâce à
#' la fonction perform_request. Elle permet de créer un tibble de 5 colonnes où
#' chacune d'elles est une catégorie d'informations et chaque ligne correspond à
#' une prévision, donc à 1 heure.
#'
#' @param resp un tibble avec 5 lignes
#'
#' @return un tibble avec 5 colonnes
#'
#' @examples
#' unnest_response(perform_request(48.85,2.35))
#'
unnest_response <- function (resp) {
  tibble::tibble(
    date_heure = unlist(resp$hourly[1][[1]]),
    temperature_celsius = unlist(resp$hourly[2][[1]]),
    temperature_ressentie_celsius = unlist(resp$hourly[3][[1]]),
    chance_pluie = unlist(resp$hourly[4][[1]]),
    quantite_pluie = unlist(resp$hourly[5][[1]])
  )
}


#' get_gps_coordinate
#'
#' @description
#' Cette fonction permet d'obtenir un vecteur de coordonnées (latitude et
#' longitude) à partir d'une adresse ou d'un nom de lieu.
#'
#' @param adresse une adresse
#'
#' @return un vecteur de coordonnées (lat, long)
#'
#' @examples
#' get_gps_coordinate("Parc des Princes")
#'
get_gps_coordinate <- function (adresse) {
  ad <- tidygeocoder::geo(adresse)
  xy <- c(ad$lat, ad$long)
  return(xy)
}


#' get_graphs
#'
#' @description
#' Cette fonction affiche 3 graphiques représentant chacun l'évolution des
#' caractéristiques météo prévues.
#'
#'
#' @param data un tibble avec 5 colonnes
#'
#' @return une liste de 3 graphiques
#'
#' @examples
#' get_graphs(unnest_response(perform_request(48.85,2.35)))
#'
get_graphs <- function(data) {

  graph_temp <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = seq_along(temperature_celsius), y = temperature_celsius, color = "Température réelle")) +
    ggplot2::geom_line(ggplot2::aes(x = seq_along(temperature_ressentie_celsius), y = temperature_ressentie_celsius, color = "Température ressentie")) +
    ggplot2::labs(title = "Prévisions de température réelle et ressentie", y = "Température en °C", x = "Temps", color = "Légende") +
    ggplot2::theme_light() +
    ggplot2::geom_vline(xintercept = c(24, 48, 72, 96, 120, 144), color = "black", linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144), labels = c("J0", "J1", "J2", "J3", "J4", "J5", "J6")) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c("Température réelle" = "#4EA72E", "Température ressentie" = "#E97132")) +
    ggplot2::theme(legend.position = "bottom")

  graph_pb_pluie <- ggplot2::ggplot(data, ggplot2::aes(x = seq_along(chance_pluie), y = chance_pluie)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Prévisions de probabilité de pluie", y = "Probabilité de pluie en %", x = "Temps") +
    ggplot2::theme_light() +
    ggplot2::geom_vline(xintercept = c(24, 48, 72, 96, 120, 144), color = "lightgray", linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144), labels = c("J0", "J1", "J2", "J3", "J4", "J5", "J6")) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())

  graph_qt_pluie <- ggplot2::ggplot(data, ggplot2::aes(x = seq_along(quantite_pluie), y = quantite_pluie)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Prévisions de quantité de pluie", y = "Quantité de pluie en mm", x = "Temps") +
    ggplot2::theme_light() +
    ggplot2::geom_vline(xintercept = c(24, 48, 72, 96, 120, 144), color = "lightgray", linetype = "dashed") +
    ggplot2:: scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144), labels = c("J0", "J1", "J2", "J3", "J4", "J5", "J6")) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())

  graphs <- list(graph_temp, graph_pb_pluie, graph_qt_pluie)

  return(graphs)
}



# Fonction finale


#' get_forecast
#'
#' @description
#' Cette fonction permet d'obtenir des prévisions météo facilement lisibles sur
#' 1 semaine, heure par heure, à partir du nom, de l'adresse ou des coordonnées
#' gps d'un lieu. Elles sont issues de l'API Open Météo.
#'
#' @param lieu des coordonnées ou une adresse
#' @return une liste de 2 éléments : une liste de 3 graphiques et une table
#'
#' @export
#' @examples
#' get_forecast("Parc des Princes")
#' get_forecast(c(48.841363,2.253069))
#'
get_forecast <- function(lieu) {
  UseMethod("get_forecast")
}


#' get_forecast.numeric
#'
#' @description
#' Cette fonction permet d'obtenir des prévisions météo facilement lisibles sur
#' 1 semaine, heure par heure, à partir des coordonnées gps d'un lieu. Elles
#' sont issues de l'API Open Météo.
#'
#' @param xy un vecteur comprenant une latitude et une longitude
#' @return une liste de 2 éléments : une liste de 3 graphiques et une table
#'
#' @export
#' @examples
#' get_forecast.numeric(c(48.841363,2.253069))
#'
get_forecast.numeric <- function(xy) {
  if (is.vector(xy) && length(xy) == 2) {
    resultat <- perform_request(xy[1], xy[2]) |>
                unnest_response()
    tb <- knitr::kable(resultat)
    graph <- get_graphs(resultat)
    #graph <- gridExtra::grid.arrange(graph[[1]], graph[[2]], graph[[3]], graph[[4]], ncol = 2, nrow = 2)
    return(list(graphs = graph, table = tb))
  } else {
    return("Erreur")
  }
}


#' get_forecast.character
#'
#' @description
#' Cette fonction permet d'obtenir des prévisions météo facilement lisibles sur
#' 1 semaine, heure par heure, à partir du nom ou de l'adresse d'un lieu. Elles
#' sont issues de l'API Open Météo.
#'
#' @param address une adresse
#' @return une liste de 2 éléments : une liste de 3 graphiques et une table
#'
#' @export
#' @examples
#' get_forecast.character("Parc des Princes")
get_forecast.character <- function(address) {
  if (is.character(address) && length(address) == 1) {
    resultat <- get_gps_coordinate(address) |>
                get_forecast()
    return(resultat)
  } else {
    return("Erreur")
  }
}

