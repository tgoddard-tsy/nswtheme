#' Set NSW Government visual defaults
#'
#' Configures the current R session to use NSW Government themes,
#' colour palettes, typography and geom defaults for data visualisation.
#'
#' `set_defaults_nsw()` sets the default ggplot2 theme and colour scales,
#' updates defaults for commonly used geoms and stats, and configures
#' flextable defaults. Preset styles can be used for particular NSW
#' Government brands, or individual theme settings can be customised.
#'
#' @param style Optional character string selecting a preset style.
#'   Available styles are `"Core"`, `"Health"`, `"Treasury"` and
#'   `"Budget"`. If `NULL`, the default NSW Government style is used.
#'   Style names are not case-sensitive.
#' @param textbox Logical. If `TRUE`, use ggtext textbox elements for
#'   supported plot text.
#' @param base_size Base font size, in points.
#' @param base_line_size Base line width. If `NULL`, calculated from
#'   `base_size`.
#' @param base_family Font family used for general text.
#' @param header_family Font family used for headings and text geoms.
#' @param ink Foreground colour used for text, axes and reference lines.
#' @param ink_geom Default colour or fill used for common data geoms.
#' @param paper Plot background colour.
#' @param accent Accent colour used for grid lines and other theme elements.
#'
#' @section Preset styles:
#' `style` provides predefined palette and theme settings:
#'
#' * `NULL` or `"Default"`: standard NSW Government defaults.
#' * `"Health"`: NSW Health palette defaults.
#' * `"Treasury"`: NSW Treasury palette defaults.
#' * `"Budget"`: Budget publication defaults.
#' * `"Core"`: NSW Government core colours (Blue 01 and Red 02 only).
#'
#' Explicit theme arguments can be used to customise the selected preset.
#'
#' @section What this function changes:
#' Calling `set_defaults_nsw()` changes several defaults for the current
#' R session:
#'
#' * the default ggplot2 theme;
#' * discrete and continuous colour and fill scales;
#' * defaults for common ggplot2 geoms and stats;
#' * supported optional ggplot2 extension geoms; and
#' * flextable formatting defaults.
#'
#' Palette settings used by nswtheme are also stored in the
#' `nswtheme.defaults` option.
#'
#' @section Optional extensions:
#' Defaults are applied to supported optional ggplot2 extension geoms when
#' those geoms are available. If an extension package is loaded after
#' calling `set_defaults_nsw()`, call this function again to apply its
#' defaults.
#'
#' @examples
#' library(nswtheme)
#' library(ggplot2)
#'
#' # Standard NSW Government defaults
#' set_defaults_nsw()
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point()
#'
#' # NSW Treasury preset
#' set_defaults_nsw("Treasury")
#'
#' # Override part of a preset
#' set_defaults_nsw(
#'   "Treasury",
#'   base_size = 10,
#'   ink_geom = red_02
#' )
#'
#' @seealso
#' [theme_nsw()],
#' [scale_colour_nsw()],
#' [scale_fill_nsw()],
#' [theme_nsw_flextable()]
#' @name set_defaults
NULL

#' @rdname set_defaults
#' @export
set_defaults_nsw <- function(
    style = NULL,
    textbox = FALSE,
    base_size = NULL,
    base_line_size = NULL,
    base_family = "Public Sans",
    header_family = "Public Sans SemiBold",
    ink = NULL,
    paper = NULL,
    accent = NULL,
    ink_geom = NULL
) {
  
  # Standardise style input
  if (is.null(style)) style <- "DEFAULT"
  style <- toupper(style)
  
  # General NSW Government defaults
  colours <- NULL
  colour_cols <- c(
    "Blue", "Orange", "Grey", "Red", "Green", "Teal",
    "Purple", "Fuchsia", "Yellow", "Brown"
  )
  ramp_colours <- c(blue_03, blue_01)
  gradient_colours <- c(blue_03, blue_01)
  budget <- FALSE
  
  # Style-specific defaults
  if (style == "CORE") {
    
    colours <- c(blue_01, red_02)
    colour_cols <- NULL
    
    if (is.null(ink_geom)) ink_geom <- red_02
    
  } else if (style %in% c("HEALTH", "MOH")) {
    
    colour_cols <- c(
      "Blue", "Red", "Grey", "Purple", "Orange", "Green", "Teal",
      "Yellow", "Fuchsia", "Brown"
    )
    ramp_colours <- c(purple_03, purple_01)
    gradient_colours <- c(purple_03, purple_01)
    
    if (is.null(ink_geom)) ink_geom <- purple_02
    
  } else if (style %in% c("TREASURY", "TSY", "TRSY")) {
    
    colour_cols <- c(
      "Teal", "Orange", "Green", "Grey", "Blue",
      "Yellow", "Purple", "Fuchsia", "Brown", "Red"
    )
    ramp_colours <- c(green_03, teal_01)
    gradient_colours <- c(green_03, teal_01)
    
    if (is.null(ink_geom)) ink_geom <- teal_02
    
  } else if (style == "BUDGET") {
    
    ramp_colours <- c(grey_03, grey_01)
    gradient_colours <- c(grey_03, grey_01)
    
    if (is.null(base_size)) base_size <- 7
    if (is.null(ink))       ink       <- "black"
    if (is.null(ink_geom))  ink_geom  <- grey_02
    if (is.null(paper))     paper     <- "transparent"
    if (is.null(accent))    accent    <- "#DFDCDA"
    
    budget <- TRUE
    
  } else if (style != "DEFAULT") {
    
    stop(
      "Unknown style: ", style,
      ". Valid styles are 'Default', 'Core', 'Health', 'Treasury', and 'Budget'."
    )
  }
  
  # General theme defaults
  if (is.null(base_size)) base_size <- 9
  if (is.null(ink))       ink       <- grey_01
  if (is.null(ink_geom))  ink_geom  <- blue_02
  if (is.null(paper))     paper     <- "white"
  if (is.null(accent))    accent    <- grey_04
  
  if (is.null(base_line_size)) base_line_size <- base_size / 22
  
  # Store nswtheme palette defaults
  options(
    nswtheme.defaults = list(
      colours = colours,
      colour_cols = colour_cols,
      ramp_colours = ramp_colours,
      gradient_colours = gradient_colours
    )
  )
  
  # Note: Use try() functions for elements relating to packages that the user may not have loaded.
  # Load fonts...
  try(extrafont::loadfonts(device = "win", quiet = TRUE))
  
  # if(is.null(extrafont::fonttable()$FamilyName)){
  #   if(packageVersion("Rttf2pt1") != "1.3.8"){
  #     remotes::install_version("Rttf2pt1", version = "1.3.8")
  #   }
  #   extrafont::font_import(paths = NULL, prompt = FALSE, pattern = "PublicSans")
  #   try(extrafont::loadfonts(device = "win", quiet = TRUE))
  # }
  
  # Update geom defaults...
  # Text geoms
  ggplot2::update_geom_defaults("text", list(colour = ink, family = header_family, fontface = 'plain', size = base_size / .pt, lineheight = 0.8))
  ggplot2::update_geom_defaults("label", list(colour = ink, family = header_family, fontface = 'plain', size = base_size / .pt, lineheight = 0.8))
  try(ggplot2::update_geom_defaults("text_repel", list(colour = ink, family = header_family, fontface = 'plain', size = base_size / .pt, lineheight = 0.8)), silent = TRUE)
  try(ggplot2::update_geom_defaults("label_repel", list(colour = ink, family = header_family, fontface = 'plain', size = base_size / .pt, lineheight = 0.8)), silent = TRUE)
  try(ggplot2::update_geom_defaults("textpath", list(colour = ink, family = header_family, fontface = 'plain', size = base_size / .pt, lineheight = 0.8)), silent = TRUE)
  try(ggplot2::update_geom_defaults("labelpath", list(colour = ink, family = header_family, fontface = 'plain', size = base_size / .pt, lineheight = 0.8)), silent = TRUE)
  
  # Line geoms
  # base_line_size = base_size/22 matches "theme_nsw" linewidth
  ggplot2::update_geom_defaults("abline", list(colour = ink, linewidth = base_line_size))
  ggplot2::update_geom_defaults("hline", list(colour = ink, linewidth = base_line_size))
  ggplot2::update_geom_defaults("vline", list(colour = ink, linewidth = base_line_size))
  ggplot2::update_geom_defaults("line", list(colour = ink_geom))
  ggplot2::update_geom_defaults("path", list(colour = ink_geom))
  ggplot2::update_geom_defaults("step", list(colour = ink_geom))
  ggplot2::update_geom_defaults("point", list(colour = ink_geom))
  
  # Poly geoms
  ggplot2::update_geom_defaults("rect", list(fill = "grey70"))
  ggplot2::update_geom_defaults("bar", list(fill = ink_geom))
  ggplot2::update_geom_defaults("col", list(fill = ink_geom))
  ggplot2::update_geom_defaults("boxplot", list(fill = ink_geom, colour = ink))
  ggplot2::update_geom_defaults("violin", list(fill = ink_geom, colour = ink))
  ggplot2::update_geom_defaults("density", list(fill = ink_geom, colour = ink_geom, alpha = 0.5))
  ggplot2::update_geom_defaults("smooth", list(fill = "grey70", colour = blue_02))
  ggplot2::update_geom_defaults("ribbon", list(fill = "grey70", colour = NA, alpha = 0.3))
  try(ggplot2::update_geom_defaults("sf", list(fill = accent, colour = grey_02)), silent = TRUE)
  
  # ggpattern geoms
  # Setting up a function to combine try and update_geom_defaults:
  try_update_geom_defaults <- function(...) {
    try(ggplot2::update_geom_defaults(...), silent = TRUE)
  }
  try({
    ggpattern_geoms <- ls(pattern = '^geom_', env = as.environment('package:ggpattern'))
    ggpattern_names <- gsub("geom_", "", ggpattern_geoms)
    lapply(ggpattern_names, try_update_geom_defaults, 
           list(
             pattern_colour = NA, 
             pattern_fill = ink, 
             pattern_density = 0.15, 
             pattern_spacing = 0.03, 
             pattern_angle = 45, 
             pattern_key_scale_factor = 0.55
           )
    ) 
  }, silent = TRUE)
  
  ggplot2::update_stat_defaults("count", list(fill = ink_geom))
  ggplot2::update_stat_defaults("boxplot", list(fill = ink_geom))
  ggplot2::update_stat_defaults("density", list(fill = ink_geom))
  ggplot2::update_stat_defaults("ydensity", list(fill = ink_geom))
  
  # Update palette and themes
    # Colour palettes
    options(ggplot2.discrete.colour = scale_colour_nsw)
    options(ggplot2.discrete.fill = scale_fill_nsw)
    options(ggplot2.continuous.colour = scale_colour_nsw_gradient)
    options(ggplot2.continuous.fill = scale_fill_nsw_gradient)
    # try(assign("scale_pattern_fill_discrete", scale_pattern_fill_nsw, envir = as.environment("package:ggpattern")))
    
    # Themes (ggplot and flextable)...
    ggplot2::theme_set(
      theme_nsw(
        budget = budget, textbox = textbox, 
        base_size = base_size, base_line_size = base_line_size,
        base_family = base_family, header_family = header_family, 
        ink = ink, paper = paper, accent = accent
      )
    )
    
    flextable::set_flextable_defaults(
      font.size = base_size, font.family = base_family, font.color = ink,
      theme_fun = theme_nsw_flextable,
      padding = 6, 
      big.mark = ",", 
      digits = 1)
}
