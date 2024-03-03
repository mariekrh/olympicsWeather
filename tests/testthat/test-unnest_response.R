library(testthat)
library(tibble)

data <- perform_request(48.85, 2.35)
data_test <- data |>  unnest_response()

test_that("unnest_response_multiplication works", {
  expect_equal(nrow(data_test), 168)
  expect_equal(data_test$temperature_celsius, unlist(data$hourly[2][[1]]))
  expect_equal(
    colnames(data_test),
    c(
      "date_heure",
      "temperature_celsius",
      "temperature_ressentie_celsius",
      "chance_pluie",
      "quantite_pluie"
    )
  )
  expect_equal(ncol(data_test), 5)
})
