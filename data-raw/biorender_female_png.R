## code to prepare `biorender_female.png` dataset goes here
biorender_female.png <- png::readPNG("./data-raw/biorender_female.png")

usethis::use_data(biorender_female.png, overwrite = TRUE)
