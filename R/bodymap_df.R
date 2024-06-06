#' Returns formatted data frame for SMMART Body Map plot
#'
#' @param df A data frame containing the data to be plotted on the body map.
#' @param col_name_date A string containing the column name of the column in `df` containing date
#' values. Optional, if df column is named "lesion_date".
#' @param col_name_lookup A string containing the column name of the column in `df` containing
#' values to join with the lesion_lookup dataset. Optional, if df column is named "lookup_name".
#' @param col_name_desc A string containing the column name of the column in `df` containing
#' lesion description values. Optional, if df column is named "site_description".
#' @param col_name_lat A string containing the column name of the column in `df` containing
#' lesion laterality values. Optional, if df column is named "laterality".
#' @param col_name_type A string containing the column name of the column in `df` containing
#' lesion type values (Primary, Metastatic). Optional, if df column is named "lesion_type".
#' @param remove_duplicate_sites Logical value indicating whether duplicate lesion sites should
#' be removed from data frame. Default is TRUE.
#'
#' @return Data Frame
#' @export
#'
#' @examples
#' bodymap_df(example_patient_dataset)
bodymap_df <- function(df,
                       col_name_date = NULL,
                       col_name_lookup = NULL,
                       col_name_desc = NULL,
                       col_name_lat = NULL,
                       col_name_type = NULL,
                       remove_duplicate_sites = TRUE) {
  # Verify Arguments --------------------------------------------------------
  checkmate::assert(
    combine = "and",
    checkmate::check_data_frame(df,
      all.missing = FALSE,
      min.cols = 5
    ),
    checkmate::check_string(
      col_name_date,
      null.ok = TRUE
    ),
    checkmate::check_string(
      col_name_lookup,
      null.ok = TRUE
    ),
    checkmate::check_string(
      col_name_desc,
      null.ok = TRUE
    ),
    checkmate::check_string(
      col_name_lat,
      null.ok = TRUE
    ),
    checkmate::check_string(
      col_name_type,
      null.ok = TRUE
    ),
    checkmate::check_logical(remove_duplicate_sites, null.ok = FALSE)
  )

  # Rename and verify columns -------------------------------------------------
  v_input <- c(
    coalesce(col_name_date, "NULL"),
    coalesce(col_name_lookup, "NULL"),
    coalesce(col_name_desc, "NULL"),
    coalesce(col_name_lat, "NULL"),
    coalesce(col_name_type, "NULL")
  )
  v_rename <- c(
    "lesion_date",
    "lookup_name",
    "site_description",
    "laterality",
    "lesion_type"
  )
  var_names <- data.frame(v_input, v_rename)

  var_names <- var_names %>%
    dplyr::mutate(v_input = na_if(as.character(v_input), "NULL")) %>%
    dplyr::mutate(v_input = stringr::str_to_lower(v_input)) %>%
    dplyr::mutate(v_check = dplyr::coalesce(v_input, v_rename))

  df <- dplyr::rename_with(df, tolower)


  checkmate::assert_names(names(df),
    type = "unique",
    must.include = var_names$v_check,
    .var.name = "`df` columns",
    what = "colnames"
  )

  cols_rename <- var_names %>%
    dplyr::filter(!is.na(v_input))

  if (nrow(cols_rename) > 0) {
    x <- rlang::set_names(cols_rename$v_input, nm = cols_rename$v_rename)
    df <- dplyr::rename(df, all_of(x))
  }

  rm(var_names, cols_rename)

  coll <- checkmate::makeAssertCollection()

  if (is.character(df$lesion_date)) {
    date_vals <- imap(df$lesion_date, dates_convert)
    df$lesion_date <- list_simplify(date_vals)
  } else if (lubridate::is.POSIXt(df$lesion_date) ||
    lubridate::is.POSIXct(df$lesion_date) ||
    lubridate::is.POSIXlt(df$lesion_date)) {
    df$lesion_date <- lubridate::as_date(df$lesion_date)
  } else {
    checkmate::assert_date(df$lesion_date, add = coll)
  }


  checkmate::assert_character(df$lookup_name, add = coll)
  checkmate::assert_character(df$site_description, null.ok = TRUE, add = coll)
  checkmate::assert_character(df$laterality, null.ok = TRUE, add = coll)
  checkmate::assert_character(df$lesion_type, add = coll)

  checkmate::reportAssertions(coll)

  missing_site <- nrow(dplyr::filter(df, is.na(site_description)))

  if (missing_site > 0) {
    df <- df %>%
      dplyr::filter(!is.na(site_description))
    rlang::warn(message = glue::glue(
      "{missing_site} row(s) ",
      "dropped due to empty ",
      "`site_description`"
    ))
  }

  rm(missing_site)

  # Prepare DF --------------------------------------------------------------

  df <- df %>%
    dplyr::mutate(
      label_build =
        case_match(
          laterality,
          c("Not a paired site", "Left", "Right", NA) ~ NA,
          "Unknown" ~ "laterality Unknown",
          .default = laterality
        )
    ) %>%
    dplyr::mutate(label_text = case_match(label_build,
      NA ~ site_description,
      .default =
        str_c(site_description,
          label_build,
          sep = "\n"
        )
    )) %>%
    dplyr::select(!label_build)

  if (remove_duplicate_sites == TRUE) {
    ct <- nrow(df)
    df <- df %>%
      dplyr::group_by(
        site_description,
        laterality,
        lesion_type,
        lookup_name
      ) %>%
      dplyr::arrange(lesion_date) %>%
      dplyr::slice_head() %>%
      dplyr::ungroup()
    ct <- ct - nrow(df)
    if (ct > 0) {
      rlang::inform(message = glue::glue("{ct} duplicate lesions dropped"))
    }
    rm(ct)
  }

  # Clean laterality
  # bilateral - create rows with right laterality
  df_bilat <- df %>%
    dplyr::filter(laterality == "Bilateral") %>%
    dplyr::select(!laterality) %>%
    dplyr::mutate(laterality = "Right")

  # bilateral - create rows with left laterality
  df <- df %>%
    dplyr::mutate(laterality = case_match(laterality,
      "Bilateral" ~ "Left",
      .default = laterality
    ))
  # append right rows
  df <-
    dplyr::bind_rows(df, df_bilat)

  rm(df_bilat)



  # Add Grid Locations ------------------------------------------------------

  df_lookup <- smmartbodymap::lesion_lookup

  df_joined <- dplyr::left_join(df, df_lookup, by = c(
    "lookup_name" = "Site",
    "lesion_type" = "Type"
  ))

  df_unmatched <- df_joined %>%
    dplyr::filter(is.na(`ICD Code`))

  if (nrow(df_unmatched) > 0) {
    df_unmatched <- df_unmatched %>%
      dplyr::mutate(unmatched = glue::glue(
        "lookup_name: {lookup_name},",
        " site_description: {site_description}"
      ))
    msg <- paste("The following lesion sites did not have matches",
      "in the lesion_lookup dataset.",
      "Please verify `lesion_type` matches `Type`",
      "in lesion_lookup.",
      sep = " "
    )

    rlang::inform(message = paste(msg,
      stringr::str_flatten(df_unmatched$unmatched, collapse = "\n"),
      sep = "\n"
    ))
  }

  df_unplotted <- df_joined %>%
    dplyr::filter(is.na(y))

  if (nrow(df_unplotted) > 0) {
    df_unplotted <- df_unplotted %>%
      dplyr::mutate(unmatched = glue::glue(
        "lookup_name: {lookup_name},",
        " site_description: {site_description}"
      ))
    msg <- paste("The following lesion sites do not have xy plot values in the",
      "lesion_lookup dataset. Please add xy values in lesion_lookup.",
      sep = " "
    )

    rlang::inform(message = paste(msg,
      stringr::str_flatten(df_unplotted$unmatched, collapse = "\n"),
      sep = "\n"
    ))
  }

  rm(df_unmatched)
  rm(df_unplotted)


  # Select x values -----------------------------------------------------------------------------

  df_joined <- df_joined %>%
    dplyr::filter(!is.na(y)) %>%
    dplyr::filter(!is.na(`ICD Code`)) %>%
    dplyr::mutate(
      x = case_match(
        laterality,
        "Left" ~ coalesce(x_left, x, x_right),
        "Right" ~ coalesce(x_right, x, x_left),
        c("Not a paired site", "Midline", "Unknown") ~ coalesce(x, x_left, x_right),
        NA ~ coalesce(x, x_left, x_right),
        .default = coalesce(x, x_left, x_right)
      ),
      .keep = "unused"
    )


  # Return df -----------------------------------------------------------------------------------


  return(df_joined)
}

# Private Helper Functions ------------------------------------------------

dates_convert <- function(x, i) {
  tryCatch(
    {
      y <- lubridate::parse_date_time(x, c(
        "mdy",
        "ymd",
        "dby",
        "dbyHMS",
        "ymdHMS",
        "mdyHMS"
      ))
      lubridate::as_date(y, tz = "UTC")
    },
    error = function(cnd) {
      message(
        glue::glue(
          "Error parsing date for row {i}:",
          " {conditionMessage(cnd)} \n",
          "This is the data at that position: {x}\n"
        )
      )
      return(NA)
    },
    warning = function(cnd) {
      message(
        glue::glue(
          "Warning parsing date for row {i}:",
          "{conditionMessage(cnd)} \n",
          "This is the data at that position: {x}\n"
        )
      )
      return(NA)
    }
  )
}
