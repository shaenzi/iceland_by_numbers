

get_motorisation_ch <- function() {
  url <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/30148893/master"
  df <- rio::import(url)

  # don't ask, it is a human but not machine-readable excel...
  temp1 <- df[2:3,2:ncol(df)]
  df <- as_tibble(t(temp1)) |>
    rename(year = 1, n_vehicles = 2) |>
    mutate(year = stringr::str_replace(year, "r", ""), # r means revised
           year = stringr::str_replace(year, " 2", ""), # refers to a footnote
           year = as.numeric(year),
           n_vehicles = as.numeric(n_vehicles),
           area = "Switzerland")

  return(df)
}

get_motorisation_zh <- function() {
  url <- "https://www.stadt-zuerich.ch/content/dam/web/de/politik-verwaltung/statistik-und-daten/daten/mobilitaet/VER200T2007_Motorisierungsgrad-nach-Quartier.xlsx"

  df <- rio::import(url, sheet = "T_1", range = "A8:V10")

  df <- df[, 2:ncol(df)] |>
    t() |>
    as_tibble() |>
    rename(year = 1, n_vehicles = 2) |>
    mutate(area = "city of Zurich")

  return(df)
}
