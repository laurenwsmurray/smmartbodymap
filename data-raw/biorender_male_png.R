## code to prepare `biorender_male.png` dataset goes here
biorender_male.png <- png::readPNG("./data-raw/biorender_male.png")

usethis::use_data(biorender_male.png, overwrite = TRUE)
