## code to prepare `example_patient_dataset` dataset goes here

example_patient_dataset <- readxl::read_excel("./data-raw/example_patient_dataset.xlsx")
usethis::use_data(example_patient_dataset, overwrite = TRUE)
