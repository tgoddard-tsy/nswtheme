# Lockdowns
lockdowns <- function(sheet, path = './data-raw/lockdowns.xlsx'){
  df_name <- paste("lockdowns", sheet, sep = "_")
  data <- readxl::read_excel(path = path, sheet = sheet) |> 
    dplyr::mutate(start = as.Date(start), end = as.Date(end))
  assign(df_name, data, envir = .GlobalEnv)
}

lockdowns('nsw')
lockdowns('vic')
lockdowns('qld')
lockdowns('wa')

usethis::use_data(lockdowns_nsw, overwrite = TRUE)
usethis::use_data(lockdowns_vic, overwrite = TRUE)
usethis::use_data(lockdowns_qld, overwrite = TRUE)
usethis::use_data(lockdowns_wa, overwrite = TRUE)
