
get_accommodation_ch <- function(hotels_only = FALSE) {
  # hotels
  hotel_nr <- "px-x-1003020000_102"
  query_filter <- prepare_metadata_hotels(hotel_nr)
  df_hotels <- BFS::bfs_get_data(hotel_nr, language = "en",
                                 query = query_filter,
                                 clean_names = TRUE) |>
    dplyr::select(-visitors_country_of_residence, -canton) |>
    dplyr::rename(value = hotel_sector_arrivals_and_overnight_stays_of_open_establishments) |>
    dplyr::mutate(accommodation = "hotels",
                  year = as.numeric(year))

  if (hotels_only) {
    return(df_hotels)
  }

  # supplementary accomodation (DE: Parahotellerie) is separate, and is again split
  # into group accommodation, apartments and campgrounds
  # campground data starts in 2008 but looks different until 2015 - start in 2016

  # group accommodation
  # group accommodation link: https://dam-api.bfs.admin.ch/hub/api/dam/assets/29725938/master
  group_file <- here::here("temp", "je-d-10.03.02.01.02.01.xlsx")
  # there might be hidden sheets... keep everything that starts with "20" (earliest data m)
  sheets_group <- readxl::excel_sheets(group_file) |>
    purrr::keep(\(x) stringr::str_starts(x, "20"))|>
    purrr::keep(\(x) as.numeric(x) > 2015)

  df_group <- purrr::map(sheets_group, \(x) read_sheet_supp_acc(group_file, x, "A4:G22")) |>
    purrr::list_rbind()

  # apartments
  # apartments link: https://dam-api.bfs.admin.ch/hub/api/dam/assets/29725942/master
  ap_file <- here::here("temp", "je-d-10.03.02.01.01.01.xlsx")
  # there might be hidden sheets... keep everything that starts with "20" (earliest data m)
  sheets_ap <- readxl::excel_sheets(ap_file) |>
    purrr::keep(\(x) stringr::str_starts(x, "20"))|>
    purrr::keep(\(x) as.numeric(x) > 2015)

  df_ap <- purrr::map(sheets_ap, \(x) read_sheet_supp_acc(ap_file, x, "A4:G22")) |>
    purrr::list_rbind()

  # campgrounds
  # campgrounds link: https://dam-api.bfs.admin.ch/hub/api/dam/assets/29725937/master
  camp_file <- here::here("temp", "je-d-10.03.02.01.03.23.xlsx")
  # there might be hidden sheets... keep everything that starts with "20" (earliest data m)
  sheets_camp <- readxl::excel_sheets(camp_file) |>
    purrr::keep(\(x) stringr::str_starts(x, "20")) |>
    purrr::keep(\(x) as.numeric(x) > 2015)

  df_camp <- purrr::map(sheets_camp, \(x) read_sheet_supp_acc(camp_file, x, "A5:G23")) |>
    purrr::list_rbind()

  df_supp <- dplyr::bind_rows(df_group, df_ap, df_camp) |>
    dplyr::select(-dplyr::contains("_int"), -dplyr::contains("_ch"), -monat) |>
    tidyr::pivot_longer(dplyr::contains("total")) |>
    dplyr::mutate(indicator = dplyr::if_else(name == "arr_total",
                                             "Arrivals",
                                             "Overnight stays"),
                  accommodation = "supplementary",
                  month = month.name[month_num]) |>
    dplyr::select(-name, -month_num)

  # join the hotels and the rest

  df <- dplyr::bind_rows(df_hotels, df_supp) |>
    dplyr::mutate(month = factor(month, levels = month.name)) |>
    # only keep the years for which we have both hotel and supplementary data
    # hotel data since 2005, supplementary only since 2016 :-/
    dplyr::filter(year %in% intersect(unique(df_supp$year), unique(df_hotels$year)))

  return(df)
}

# get_accomodation_bs <- function() {
#   url <- "https://data.bs.ch/api/v2/catalog/datasets/100107/exports/csv"
#
#   df <- data.table::fread(url) |>
#     rename(date = datum,
#            year = jahr,
#            month = monat,
#            arrivals = anzankuenfte,
#            nights = anzlogiernaechte,
#            country = herkunftsland)
#
#   return(df)
# }

# get_accommodation_zh <- function() {
#   url <- "https://www.zuerich.com/en/api/v2/data?id=71"
#
#
# }



