# Palette
nswgov_palette_df <- readxl::read_excel(path = './data-raw/nswgov_palette.xlsx')
nswgov_palette <- structure(nswgov_palette_df$value, names = nswgov_palette_df$name)

usethis::use_data(nswgov_palette_df, overwrite = TRUE)
usethis::use_data(nswgov_palette, overwrite = TRUE)
