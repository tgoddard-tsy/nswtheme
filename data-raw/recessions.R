# Recessions
recessions <- function(sheet, path = './data-raw/recessions.xlsx'){
  df_name <- paste("recessions", sheet, sep = "_")
  data <- readxl::read_excel(path = path, sheet = sheet) |> 
    dplyr::mutate(peak = as.Date(peak), trough = as.Date(trough))
  assign(df_name, data, envir = .GlobalEnv)
}

recessions('aus')
recessions('nsw')

usethis::use_data(recessions_aus, overwrite = TRUE)
usethis::use_data(recessions_nsw, overwrite = TRUE)
