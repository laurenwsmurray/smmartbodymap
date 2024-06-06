# Testing Variables -------------------------------------------------------


char_dates <- c(
  "February 20th 1973",
  "01 3 2010",
  "1 3 10",
  "1 13 89",
  "5/27/1979",
  "12/31/99",
  "DOB:12/11/00",
  "Thu, 1 July 2004 22:30:00",
  "Thu, 1st of July 2004 at 22:30:00",
  "Thu, 1July 2004 at 22:30:00",
  "Thu, 1July2004 22:30:00",
  "Thu, 1July04 22:30:00",
  "21 Aug 2011, 11:15:34 pm",
  "1979-05-27 05:00:59",
  "1979-05-27",
  "3 jan 2000",
  "17 april 85",
  "27/5/1979",
  "20 01 89",
  "00/13/10",
  "14 12 00",
  "03:23:22 pm"
)

var_let <- LETTERS[1:22]
var_num <- 1:22

var_let_miss <- var_let
var_let_miss[[1]] <- NA

v_list <- list(1, 2, 3, 4, 5)
df_list <- data.frame(v_list)
df_col_test <- data.frame(test = LETTERS[1:3])
df_char_dates <- data.frame(
  lesion_date = char_dates,
  lookup_name = var_let,
  site_description = var_let,
  laterality = var_let,
  lesion_type = var_let
)
df_num_dates <- data.frame(
  lesion_date = var_num,
  lookup_name = var_let,
  site_description = var_let,
  laterality = var_let,
  lesion_type = var_let
)
df_num_other <- data.frame(
  lesion_date = char_dates,
  lookup_name = var_num,
  site_description = var_num,
  laterality = var_num,
  lesion_type = var_num
)
df_missing_site <- data.frame(
  lesion_date = char_dates,
  lookup_name = var_let,
  site_description = var_let_miss,
  laterality = var_let,
  lesion_type = var_let
)
df_dupe_rows <- df_char_dates %>%
  dplyr::rows_append(tibble(
    lesion_date = c("1/1/2020", "2/1/2020"),
    lookup_name = c("a", "a"),
    site_description = c("a", "a"),
    laterality = c("a", "a"),
    lesion_type = c("a", "a")
  ))

df_unplotted <- data.frame(
  lesion_date = c("1/1/2020", "1/1/2021"),
  lookup_name = c("Upper lip, NOS", "External upper lip"),
  site_description = c("Upper lip, NOS", "External upper lip"),
  laterality = c("Not a paired site", "Not a paired site"),
  lesion_type = c("Primary", "Primary")
)

# Tests -------------------------------------------------------------------


test_that("argument errors work", {
  expect_error(bodymap_df(df = NULL), "Assertion on 'df' failed: Must be of type 'data.frame', not 'NULL'")
  expect_error(bodymap_df(v_list), "Assertion on 'df' failed: Must be of type 'data.frame', not 'list'.")
  expect_error(
    bodymap_df(
      df = df_list,
      col_name_lookup = 1
    ),
    regex(".+Must be of type 'string' \\(or 'NULL'\\), not 'double'.")
  )
  expect_error(
    bodymap_df(
      df = df_list,
      col_name_date = "c"
    ),
    regex(".+ but is missing elements \\{'c'.+", multiline = TRUE)
  )
  expect_error(
    bodymap_df(df_num_dates),
    regex(".+ Variable 'df\\$lesion_date': Must be of class 'Date', not 'integer'")
  )
  expect_error(
    bodymap_df(df_num_other),
    regex(".+ Variable 'df\\$lookup_name': Must be of type 'character'.+", multiline = TRUE)
  )
})

test_that("cleaning messages work", {
  expect_message(
    bodymap_df(df_char_dates),
    regex("Warning parsing date for row.+", multiline = TRUE)
  )
  expect_warning(bodymap_df(df_missing_site), regexp = "\\d row\\(s\\) dropped due to empty `site_description`")
  expect_message(bodymap_df(df_dupe_rows), regexp = "duplicate lesions dropped")
})

test_that("lookup messages work", {
  expect_message(bodymap_df(df_dupe_rows), regexp = "The following lesion sites did not have matches in the lesion_lookup dataset")
  expect_message(bodymap_df(df_unplotted), regexp = "The following lesion sites do not have xy plot values in the lesion_lookup dataset")
})
