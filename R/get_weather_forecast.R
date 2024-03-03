

url <- "https://api.open-meteo.com/v1/forecast"

# Fonctions internes

#' perform_request
#'
#'@description
#'Cette fonction permet d'effectuer une requête sur l'API Open Meteo à partir
#'de coordonnées gps. Elle porte sur l'heure,la température, la température
#'ressentie, le % de chance qu'il pleuve et la quantité de pluie prévue sur 1
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
#' Cette fonction permet d'améliorer la lisibilité des réultats obtenus grâce à
#' la fonction perform_request. Elle permet de créer un tible de 5 colonnes ou
#' chacune d'elles est une catégorie d'information et chaque ligne correspond à
#' une prévision et donc à 1 heure.
#'
#' @param resp a tibble with 5 rows, from the request on the API
#'
#' @return a tibble with 5 columns, more readable
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
#' longitude) à partir d'une addresse ou d'un nom de lieu.
#'
#' @param adresse une adresse
#'
#' @return un vecteur de coordonnées
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
#' Cette fonction affiche 4 graphiques représentant chacun l'évolution d'une des
#' caractéristiques météo prévues.
#'
#'
#' @param data a tibble with 5 columns
#'
#' @return 4 plots, the evolution of the values
#'
#' @examples
#' get_graphs(get_forecast(c(48.841363,2.253069)))
#'
get_graphs <- function(data) {
  graphics::par(mfrow = c(2, 2))

  plot(
    data$temperature_celsius,
    type = "l",
    main = "Prévisions de température",
    ylab = "Température en °C",
    xlab = "Temps",
    xaxt = "n"
  )
  graphics::abline(
    v = c(24, 48, 72, 96, 120, 144),
    col = "lightgray",
    lty = 2
  )
  graphics::axis(
    side = 1,
    at = c(1, 24, 48, 72, 96, 120, 144),
    labels = c("J0", "J1", "J2", "J3", "J4", "J5", "J6")
  )

  plot(
    data$temperature_ressentie_celsius,
    type = "l",
    main = "Prévisions de température ressentie",
    ylab = "Température en °C",
    xlab = "Temps",
    xaxt = "n"
  )
  graphics::abline(
    v = c(24, 48, 72, 96, 120, 144),
    col = "lightgray",
    lty = 2
  )
  graphics::axis(
    side = 1,
    at = c(1, 24, 48, 72, 96, 120, 144),
    labels = c("J0", "J1", "J2", "J3", "J4", "J5", "J6")
  )

  plot(
    data$chance_pluie,
    type = "l",
    main = "Prévisions de probalilité de pluie",
    ylab = "Probalibilité de pluie en %",
    xlab = "Temps",
    xaxt = "n"
  )
  graphics::abline(
    v = c(24, 48, 72, 96, 120, 144),
    col = "lightgray",
    lty = 2
  )
  graphics::axis(
    side = 1,
    at = c(1, 24, 48, 72, 96, 120, 144),
    labels = c("J0", "J1", "J2", "J3", "J4", "J5", "J6")
  )

  plot(
    data$quantite_pluie,
    type = "l",
    main = "Prévisions de quantité de pluie",
    ylab = "Quantité de pluie en mm",
    xlab = "Temps",
    xaxt = "n"
  )
  graphics::abline(
    v = c(24, 48, 72, 96, 120, 144),
    col = "lightgray",
    lty = 2
  )
  graphics::axis(
    side = 1,
    at = c(1, 24, 48, 72, 96, 120, 144),
    labels = c("J0", "J1", "J2", "J3", "J4", "J5", "J6")
  )

  graphics::par(mfrow = c(1, 1))
}


# Fonction finale


#' get_forecast
#'
#' @description
#' Cette fonction permet d'obtenir des prévisions météo facilement lisibles sur
#' 1 semaine, heure par heure, à partir du nom, de l'adresse ou des coordonnées
#' gps d'un lieu. Elles sont issues de l'API Open Météo.
#'
#' @param lieu coord or adress
#' @return a tibble with 5 columns
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
#' @param xy a vector, latitude and longitude
#' @return a tibble with 5 columns
#'
#' @export
#' @examples
#' get_forecast.numeric(c(48.841363,2.253069))
#'
get_forecast.numeric <- function(xy) {
  if (is.vector(xy) && length(xy) == 2) {
    resultat <- perform_request(xy[1], xy[2]) |> unnest_response()
    get_graphs(resultat)
    resultat <- knitr::kable(resultat)
    return(resultat)
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
#' @param address a character, an address
#' @return a tibble with 5 columns
#'
#' @export
#' @examples
#' get_forecast.character("Parc des Princes")
get_forecast.character <- function(address) {
  if (is.character(address) && length(address) == 1) {
    resultat <- get_gps_coordinate(address) |> get_forecast()
    resultat <- knitr::kable(resultat)
    return(resultat)
  } else {
    return("Erreur")
  }
}

