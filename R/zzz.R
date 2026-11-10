.onLoad <- function(libname, pkgname) {
  # ---- systemfonts ----
  
  font_dir <- system.file("fonts", package = pkgname)
  
  system_fonts <- systemfonts::system_fonts()$family
  registered_fonts <- systemfonts::registry_fonts()$family
  
  available_fonts <- union(
    system_fonts,
    registered_fonts
  )
  
  if (!"Public Sans" %in% available_fonts) {
    systemfonts::register_font(
      "Public Sans",
      plain = file.path(font_dir, "PublicSans-Regular.ttf"),
      bold = file.path(font_dir, "PublicSans-Bold.ttf"),
      italic = file.path(font_dir, "PublicSans-Italic.ttf"),
      bolditalic = file.path(font_dir, "PublicSans-BoldItalic.ttf")
    )
  }
  
  
  if (!"Public Sans SemiBold" %in% available_fonts) {
    systemfonts::register_font(
      "Public Sans SemiBold",
      plain = file.path(font_dir, "PublicSans-SemiBold.ttf"),
      italic = file.path(font_dir, "PublicSans-SemiBoldItalic.ttf")
    )
  }
  
  # ---- extrafont ----
  
  # First try whatever is already in the user's extrafont database
  extrafont::loadfonts(quiet = TRUE)
  
  required_fonts <- c(
    "Public Sans",
    "Public Sans SemiBold"
  )
  
  pdf_fonts <- names(grDevices::pdfFonts())
  
  # extrafont::font_addpackage() fails when a dev user does not have
  # Public Sans registered with extrafont yet.
  is_dev_package <-
    requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package(pkgname)
  
  # If required fonts aren't available, add nswtheme's bundled
  # Type 1 fonts to extrafont
  if (!all(required_fonts %in% pdf_fonts) && !is_dev_package) {
    
    extrafont::font_addpackage(pkgname)
    
    # Register newly-added fonts with supported graphics devices
    extrafont::loadfonts(quiet = TRUE)
  }
  
  pdf_fonts2 <- names(grDevices::pdfFonts())
  
  # Warn if registration still failed
  
  if (!all(required_fonts %in% pdf_fonts2) && !is_dev_package) {
    warning(
      "Public Sans could not be registered with R's graphics devices. ",
      "Your extrafont database may contain conflicting Public Sans entries. ",
      "If the problem persists, try resetting extrafontdb by running:\n\n",
      "  remove.packages(\"extrafontdb\")\n",
      "  install.packages(\"extrafontdb\")\n\n",
      "Then restart R and reload nswtheme. ",
      "You do not need to run extrafont::font_import(); ",
      "nswtheme supplies its own Public Sans fonts.",
      call. = FALSE
    )
  }
}