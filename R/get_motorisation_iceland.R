#' get_motorisation_iceland
#'
#' @details documentation for pxweb https://ropengov.github.io/pxweb/articles/pxweb.html or https://github.com/rOpenGov/pxweb
#' for the underlying API https://www.scb.se/api_en/
#'
#' @return tibble with motorisation level by year for iceland
#' @export
#'
get_motorisation_iceland <- function() {
  url <- "https://px.hagstofa.is:443/pxen/api/v1/en/Umhverfi/5_samgongur/3_okutaekiogvegir/SAM03101.px"

  pxq <- pxweb_query(here::here("json", "query_level_motorisation.json"))

  px <- pxweb_get(url = url,
                  query = pxq)

  df <- as.data.frame(px) |>
    janitor::clean_names() |>
    rename(n_vehicles = registered_motor_vehicles_1950_2021) |>
    mutate(year = as.numeric(year)) |>
    select(-vehicles) |>
    mutate(area = "Iceland")

  return(df)
}
