## code to prepare `lesion_lookup` dataset goes here

lesion_lookup <- readxl::read_excel("./data-raw/lesion_lookup.xlsx")
usethis::use_data(lesion_lookup, overwrite = TRUE)
