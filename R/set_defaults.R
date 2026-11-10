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
#' @param style Optional character string selecting a preset style.
#'   It is not case-sensitive.
#'   Available styles are `"Default"`, `"Core"`, `"Health"`, `"Treasury"`,
#'   `"Budget"` and `"Aboriginal"`. 
#'   If `NULL`, the default NSW Government style is used. 
#' @param palette_variant Character string selecting the colour-grid variant.
#'   Currently `"base"` or `"aboriginal"`. The selected variant is used by
#'   default discrete colour and fill palettes. 
#'   It is not case-sensitive.
#' @param textbox Logical. If `TRUE`, use ggtext textbox elements for
#'   supported plot text.
#' @param gridlines Character string specifying which major grid lines to
#'   display. One of `"both"`, `"y"`, `"x"` or `"none"`. 
#'   If `NULL`, the selected style determines the default.
#' @param base_size Base font size, in points. 
#'   If `NULL`, determined by the selected style.
#' @param base_line_size Base line width. 
#'   If `NULL`, calculated from `base_size`.
#' @param base_family Font family used for general text. 
#'   If `NULL`, determined by the selected style.
#' @param header_family Font family used for headings and text geoms. 
#'   If `NULL`, determined by the selected style.
#' @param ink Foreground colour used for text, axes and reference lines. 
#'   If `NULL`, determined by the selected style.
#' @param ink_geom Default colour or fill used for common data geoms. 
#'   If `NULL`, determined by the selected style and palette variant.
#' @param paper Plot background colour. 
#'   If `NULL`, determined by the selected style.
#' @param accent Accent colour used for grid lines and other theme elements.
#'   If `NULL`, determined by the selected style.
#' @param strip_background Background colour used for facet strips. 
#'   If `NULL`, determined by the selected style.
#' @param strip_text Text colour used for facet strips. 
#'   If `NULL`, determined by the selected style.
#'
#' @section Preset styles:
#' `style` provides predefined palette and theme settings:
#'
#' * `NULL` or `"Default"`: standard NSW Government defaults.
#' * `"Health"`: NSW Health palette defaults.
#' * `"Treasury"`: NSW Treasury palette defaults.
#' * `"Budget"`: Budget publication defaults.
#' * `"Core"`: NSW Government core colours (Blue 01 and Red 02 only).
#' * `"Aboriginal"`: standard NSW Government defaults using the Aboriginal
#'   colour-grid variant.
#'
#' Explicit theme arguments override values supplied by the selected preset.
#' Arguments left as `NULL` use the corresponding style default.
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
#' Resolved style, palette and theme settings are also stored in the
#' `nswtheme.defaults` option so they can be reused by other nswtheme
#' functions.
#'
#' @section Optional extensions:
#' Defaults are applied to supported optional ggplot2 extension geoms when
#' those geoms are available. If an extension package is loaded after
#' calling `set_defaults_nsw()`, call this function again to apply its
#' defaults.
#'
#' @returns Invisibly returns the resolved `nswtheme.defaults` list.
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
#'   ink_geom = red_02,
#'   gridlines = "both"
#' )
#' 
#'
#' # Use the Aboriginal colour grid as the session default
#' set_defaults_nsw(palette_variant = "aboriginal")
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
    palette_variant = "base",
    textbox = FALSE,
    gridlines = NULL,
    base_size = NULL,
    base_line_size = NULL,
    base_family = NULL,
    header_family = NULL,
    ink = NULL,
    paper = NULL,
    accent = NULL,
    ink_geom = NULL,
    strip_background = NULL,
    strip_text = NULL
) {
  
  # ---------------------------------------------------------------------------
  # Standardise and validate inputs
  # ---------------------------------------------------------------------------
  
  if (is.null(style)) style <- "DEFAULT"
  
  if (
    length(style) != 1L ||
    is.na(style) ||
    !is.character(style) ||
    !nzchar(style)
  ) {
    stop(
      "`style` must be a single non-empty character string or `NULL`.",
      call. = FALSE
    )
  }
  
  style <- toupper(style)
  
  style <- switch(
    style,
    MOH = "HEALTH",
    TSY = "TREASURY",
    TRSY = "TREASURY",
    style
  )
  
  valid_styles <- c(
    "DEFAULT",
    "CORE",
    "HEALTH",
    "TREASURY",
    "BUDGET",
    "ABORIGINAL"
  )
  
  if (!style %in% valid_styles) {
    stop(
      "Unknown style: ", style,
      ". Valid styles are 'Default', 'Core', 'Health', ",
      "'Treasury', 'Budget', and 'Aboriginal'.",
      call. = FALSE
    )
  }
  
  # "Aboriginal" is a convenience alias for the default style using the
  # Aboriginal palette variant.
  if (style == "ABORIGINAL") {
    style <- "DEFAULT"
    palette_variant <- "aboriginal"
  }
  
  palette_variant <- validate_palette_variant(palette_variant)
  
  
  # ---------------------------------------------------------------------------
  # General NSW Government defaults
  # ---------------------------------------------------------------------------
  
  colours <- NULL
  
  colour_cols <- c(
    "Blue", "Orange", "Grey", "Red", "Green", "Teal",
    "Purple", "Fuchsia", "Yellow", "Brown"
  )
  
  gradient_colours <- nswgov_palette[c("blue_03", "blue_01")]
  
  style_gridlines <- "both"
  style_base_size <- 9
  style_base_family <- "Public Sans"
  style_header_family <- "Public Sans SemiBold"
  style_ink <- grey_01
  style_paper <- "white"
  style_accent <- grey_04
  style_ink_geom <- blue_02
  style_strip_background <- blue_01
  style_strip_text <- "white"

  # ---------------------------------------------------------------------------
  # Style-specific defaults
  # ---------------------------------------------------------------------------
  
  if (style == "CORE") {
    
    # Core always uses the fixed NSW Government core colours.
    if (palette_variant != "base") {
      warning(
        "`palette_variant` is ignored when `style = \"Core\"`. ",
        "The Core style always uses the fixed NSW Government core colours.",
        call. = FALSE
      )
    }
    
    palette_variant <- "base"
    
    colours <- nswgov_palette[c("blue_01", "red_02")]
    colour_cols <- NULL
    gradient_colours <- nswgov_palette[c("red_02", "blue_01")]
    style_ink_geom <- nswgov_palette["red_02"]
    
  } else if (style == "HEALTH") {
    
    colour_cols <- c(
      "Blue", "Red", "Grey", "Purple", "Orange", "Green",
      "Teal", "Yellow", "Fuchsia", "Brown"
    )
    
    gradient_colours <- nswgov_palette[c("purple_03", "purple_01")]
    style_ink_geom <- nswgov_palette["purple_02"]
    style_gridlines <- "both"
    
  } else if (style == "TREASURY") {
    
    colour_cols <- c(
      "Teal", "Orange", "Green", "Grey", "Blue",
      "Yellow", "Purple", "Fuchsia", "Brown", "Red"
    )
    
    gradient_colours <- nswgov_palette[c("green_03", "teal_01")]
    style_ink_geom <- nswgov_palette["teal_02"]
    style_gridlines <- "y"
    
  } else if (style == "BUDGET") {
    
    gradient_colours <- nswgov_palette[c("grey_03", "grey_01")]
    style_base_size <- 7
    style_ink <- "black"
    style_paper <- "transparent"
    style_accent <- "#DFDCDA"
    style_ink_geom <- nswgov_palette["grey_02"]
    style_strip_background <- style_accent
    style_strip_text <- style_ink
    style_gridlines <- "y"
  }
  
  
  # ---------------------------------------------------------------------------
  # Palette-variant-specific defaults
  # ---------------------------------------------------------------------------
  
  # The style controls the preferred colour-column ordering, while the
  # palette variant controls the grid from which those columns are selected.
  #
  # Core is excluded because it always uses its fixed exact palette.
  if (palette_variant == "aboriginal" && style != "CORE") {
    gradient_colours <- nswgov_palette[
      c("light_water_blue", "billabong_blue")
    ]
    
    style_ink_geom <- nswgov_palette["saltwater_blue"]
  }
  
  
  # ---------------------------------------------------------------------------
  # General theme defaults
  # ---------------------------------------------------------------------------
  
  if (is.null(base_size))        base_size        <- style_base_size
  if (is.null(base_family))      base_family      <- style_base_family
  if (is.null(header_family))    header_family    <- style_header_family
  if (is.null(ink))              ink              <- style_ink
  if (is.null(paper))            paper            <- style_paper
  if (is.null(accent))           accent           <- style_accent
  if (is.null(ink_geom))         ink_geom         <- unname(style_ink_geom)
  if (is.null(strip_background)) strip_background <- style_strip_background
  if (is.null(strip_text))       strip_text       <- style_strip_text
  if (is.null(gridlines))        gridlines        <- style_gridlines
  if (is.null(base_line_size))   base_line_size   <- base_size / 22
  
  gridlines <- match.arg(
    gridlines,
    c("both", "y", "x", "none")
  )
  
  
  # ---------------------------------------------------------------------------
  # Align colour columns with palette variant
  # ---------------------------------------------------------------------------
  
  if (!is.null(colour_cols)) {
    
    requested_colour_cols <- colour_cols
    
    available_colour_cols <- unique(
      nswgov_palette_df$colour_col[
        nswgov_palette_df$variant == palette_variant
      ]
    )
    
    # Preserve the style-specific order while omitting unavailable columns.
    colour_cols <- requested_colour_cols[
      requested_colour_cols %in% available_colour_cols
    ]
    
    unavailable_colour_cols <- setdiff(
      requested_colour_cols,
      available_colour_cols
    )
    
    if (length(unavailable_colour_cols)) {
      warning(
        "The following colour columns are not available in the ",
        palette_variant,
        " variant and have been omitted: ",
        paste(unavailable_colour_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    if (!length(colour_cols)) {
      stop(
        "None of the colour columns requested by style `",
        tools::toTitleCase(style),
        "` are available in the `",
        palette_variant,
        "` palette variant.",
        call. = FALSE
      )
    }
  }
  
  if (!is.null(colours)) colours <- unname(colours)
  gradient_colours <- unname(gradient_colours)
  
  
  # ---------------------------------------------------------------------------
  # Store nswtheme defaults
  # ---------------------------------------------------------------------------
  
  options(
    nswtheme.defaults = list(
      style = tolower(style),
      variant = palette_variant,
      colours = colours,
      colour_cols = colour_cols,
      gradient_colours = gradient_colours,
      gridlines = gridlines,
      base_size = base_size,
      base_line_size = base_line_size,
      base_family = base_family,
      header_family = header_family,
      ink = ink,
      paper = paper,
      accent = accent,
      ink_geom = ink_geom,
      strip_background = strip_background,
      strip_text = strip_text
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Update geom defaults
  # ---------------------------------------------------------------------------
  
  # Text geoms
  ggplot2::update_geom_defaults("text", list(colour = ink, lineheight = 0.8))
  ggplot2::update_geom_defaults("label", list(colour = ink, lineheight = 0.8, fill = accent))
  try(ggplot2::update_geom_defaults("text_repel", list(colour = ink, lineheight = 0.8)), silent = TRUE)
  try(ggplot2::update_geom_defaults("label_repel", list(colour = ink, lineheight = 0.8, fill = accent)), silent = TRUE)
  try(ggplot2::update_geom_defaults("textpath", list(colour = ink, lineheight = 0.8)), silent = TRUE)
  try(ggplot2::update_geom_defaults("labelpath", list(colour = ink, lineheight = 0.8, fill = accent)), silent = TRUE)
  
  # Reference-line geoms
  # ggplot2::update_geom_defaults("abline", list(colour = ink, linewidth = base_line_size))
  # ggplot2::update_geom_defaults("hline", list(colour = ink, linewidth = base_line_size))
  # ggplot2::update_geom_defaults("vline", list(colour = ink, linewidth = base_line_size))
  
  # Other geoms
  # ggplot2::update_geom_defaults("boxplot", list(fill = ink_geom, colour = ink))
  # ggplot2::update_geom_defaults("violin", list(fill = ink_geom, colour = ink))
  # ggplot2::update_geom_defaults("density", list(alpha = 0.5))
  
  # Retain fixed contrasting inks for standard ggplot2 and sf behaviour.
  # ggplot2::update_geom_defaults("smooth", list(fill = "grey70", colour = blue_02))
  ggplot2::update_geom_defaults("ribbon", list(fill = "grey70", colour = NA, alpha = 0.3))
  try(ggplot2::update_geom_defaults("sf", list(fill = accent, colour = grey_02)), silent = TRUE)
  
  
  # ---------------------------------------------------------------------------
  # Update optional extension defaults
  # ---------------------------------------------------------------------------
  
  try_update_geom_defaults <- function(...) {
    try(ggplot2::update_geom_defaults(...), silent = TRUE)
  }
  
  try({
    
    ggpattern_geoms <- ls(
      pattern = "^geom_",
      env = as.environment("package:ggpattern")
    )
    
    ggpattern_names <- sub("^geom_", "", ggpattern_geoms)
    
    lapply(
      ggpattern_names,
      try_update_geom_defaults,
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
  
  
  # ---------------------------------------------------------------------------
  # Set colour scale defaults
  # ---------------------------------------------------------------------------
  
  options(ggplot2.continuous.colour = scale_colour_nsw_gradient)
  options(ggplot2.continuous.fill = scale_fill_nsw_gradient)
  
  
  # ---------------------------------------------------------------------------
  # Set ggplot2 theme
  # ---------------------------------------------------------------------------
  
  ggplot2::theme_set(
    theme_nsw(
      textbox = textbox,
      gridlines = gridlines,
      palette_variant = palette_variant,
      base_size = base_size,
      base_line_size = base_line_size,
      base_family = base_family,
      header_family = header_family,
      ink = ink,
      paper = paper,
      accent = accent,
      ink_geom = ink_geom,
      strip_background = strip_background,
      strip_text = strip_text
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Set flextable defaults
  # ---------------------------------------------------------------------------
  
  flextable::set_flextable_defaults(
    font.size = base_size,
    font.family = base_family,
    font.color = ink,
    theme_fun = theme_nsw_flextable,
    padding = 6,
    big.mark = ",",
    digits = 1
  )
  
  invisible(getOption("nswtheme.defaults"))
}