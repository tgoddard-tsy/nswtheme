# Palette
nswgov_palette <- readxl::read_excel(path = './data-raw/nswgov_palette.xlsx')
nswgov_palette_simple <- structure(nswgov_palette$value, names = nswgov_palette$name)

usethis::use_data(nswgov_palette, overwrite = TRUE)
usethis::use_data(nswgov_palette_simple, overwrite = TRUE)
