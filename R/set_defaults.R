#' Set NSW Treasury theme and palette as defaults
#'
#' Load fonts (including \emph{Public Sans} if installed) and set the default themes and colour scales. Run this function after loading packages. 
#' 
#' The function will also attempt to set defaults for optional ggplot extensions such as ggpattern, ggrepel and geomtextpath. 
#' If these packages have not been loaded prior to setting defaults, the function won't be able to find the geoms - so it will skip them.
#' Therefore, if any of these packages are loaded later in your session, you'll need to set defaults again so the function can access the geoms.
#' @param budget a logical determining whether to use budget theme settings.
#' @examples 
#' library(nswtrsy)
#' 
#' set_defaults_nsw()
#' @name set_defaults
NULL

#' @rdname set_defaults
#' @export
set_defaults_nsw <- function(budget = FALSE, textbox = FALSE, 
                             base_size = NULL, base_family = "Public Sans", header_family = "Public Sans SemiBold", base_line_size = base_size/22, 
                             ink = NULL, paper = NULL, accent = NULL,
                             ink_geom = teal_02){
  # Note: Use try() functions for elements relating to packages that the user may not have loaded.
  # Load fonts...
  try(extrafont::loadfonts(device = "win", quiet = TRUE))
  
  if(is.null(extrafont::fonttable()$FamilyName)){
    if(packageVersion("Rttf2pt1") != "1.3.8"){
      remotes::install_version("Rttf2pt1", version = "1.3.8")
    }
    extrafont::font_import(paths = NULL, prompt = FALSE, pattern = "PublicSans")
    try(extrafont::loadfonts(device = "win", quiet = TRUE))
  }
  
  # Default theme parameters unless overridden
  if (is.null(base_size)) base_size <- if (budget) 7 else 9
  if (is.null(ink))       ink       <- if (budget) "black" else grey_01
  if (is.null(paper))     paper     <- if (budget) "transparent" else "white"
  if (is.null(accent))    accent    <- if (budget) "#DFDCDA" else grey_04

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
  
  # NSWGOV defaults
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
