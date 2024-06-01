#' prepare_metadata_hotels
#'
#' @param bfs_nr_hotel BFS ID/number identifying the dataset, excpecting the number for hotels
#'
#' @return named list, to be used for querying
#' @export
prepare_metadata_hotels <- function(bfs_nr_hotel){
  # prepare metadata so I can query only specified aggregations, otherwise I
  # have separate items and their aggregations
  meta_hotels <- BFS::bfs_get_metadata(bfs_nr_hotel, language = "en")

  metadata_tidy <- meta_hotels |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(valueTexts = meta_hotels |>
                    dplyr::select(valueTexts) |>
                    tidyr::unnest_longer(valueTexts) |>
                    dplyr::pull(valueTexts)) |>
    dplyr::select(code, text, values, valueTexts, dplyr::everything())

  # select dimensions
  # all years - no filter
  # months: separate months, not the total per year as well
  dim1 <- metadata_tidy |>
    dplyr::filter(text == "Month" & valueTexts %in% month.name)
  # no separation by canton, just all of Switzerland
  dim2 <- metadata_tidy |>
    dplyr::filter(text == "Canton" & valueTexts == "Switzerland")
  # no separation by visitor country of residence
  dim3 <- metadata_tidy |>
    dplyr::filter(text == "Visitors' country of residence" & valueTexts == "Visitors' country of residence - total")

  # build dimensions list object
  dimensions <- list(
    dim1$values,
    dim2$values,
    dim3$values
  )

  names(dimensions) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code)
  )

  dimensions
}

#' read_sheet_supp_acc
#'
#' @param file path to excel file to be read
#' @param sheet_name which sheet to be read
#' @param range what range on the sheet to be read
#'
#' @return tibble of cleaned/filtered data in that range
#' @export
read_sheet_supp_acc <- function(file,
                                sheet_name,
                                range) {
  german_month_names <- readr::date_names_lang("de")$mon

  df <- readxl::read_excel(file,
                           sheet = sheet_name,
                           range = range) |>
    janitor::clean_names() |>
    dplyr::filter(monat %in% german_month_names) |>
    dplyr::rename(arr_ch = inlander_innen_2,
                  arr_int = auslander_innen_3,
                  arr_total = total_4,
                  nights_ch = inlander_innen_5,
                  nights_int = auslander_innen_6,
                  nights_total = total_7) |>
    dplyr::mutate(year = as.numeric(sheet_name),
                  month_num = dplyr::row_number())

  return(df)
}

