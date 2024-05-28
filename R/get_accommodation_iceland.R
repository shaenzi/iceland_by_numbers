#' get_accommodation_by_region
#'
#' #' @details documentation for pxweb https://ropengov.github.io/pxweb/articles/pxweb.html or https://github.com/rOpenGov/pxweb
#' for the underlying API https://www.scb.se/api_en/
#'
#' @return tibble with accomodation iceland by year, month and redion
#' @export
#'
get_accommodation_by_region <- function() {
  url <- "https://px.hagstofa.is:443/pxen/api/v1/en/Atvinnuvegir/ferdathjonusta/gisting/3_allartegundirgististada/SAM01601.px"

  pxq <- pxweb_query(here::here("json", "query_accommodation_region.json"))

  px <- pxweb_get(url = url,
                  query = pxq)

  df <- as.data.frame(px) |>
    janitor::clean_names() |>
    mutate(year = as.numeric(year),
           month = factor(month,
                          levels = c("January", "February", "March", "April",
                                     "May", "June", "July", "August",
                                     "September", "October", "November",
                                     "December"))) |>
    rename(value = overnight_stays_and_arrivals_in_all_types_of_registered_accommodation_1998)

  return(df)
}

#' get_accommodation_by_origin
#'
#' #' @details documentation for pxweb https://ropengov.github.io/pxweb/articles/pxweb.html or https://github.com/rOpenGov/pxweb
#' for the underlying API https://www.scb.se/api_en/
#'
#' @return tibble accommodation iceland by origin of guests
#' @export
#'
get_accommodation_by_origin <- function() {
  url <- "https://px.hagstofa.is:443/pxen/api/v1/en/Atvinnuvegir/ferdathjonusta/gisting/3_allartegundirgististada/SAM01601.px"

  pxq <- pxweb_query(here::here("json", "query_accommodation_origin.json"))

  px <- pxweb_get(url = url,
                  query = pxq)

  return(as.data.frame(px))
}

