#' NSW Government ggplot theme
#'
#' A ggplot2 theme based on NSW Government brand guidelines.
#'
#' `theme_nsw()` provides the standard theme. `theme_nsw_map()` adapts the
#' theme for maps by removing axes and grid lines and optionally displaying
#' a water background.
#'
#' @param budget Logical. If `TRUE`, use the Budget variant of the theme.
#' @param textbox Logical. If `TRUE`, use ggtext textbox elements for the
#'   plot title and caption.
#' @param base_size Base font size, in points.
#' @param base_line_size Base line width.
#' @param base_family Font family used for general text.
#' @param header_family Font family used for headings.
#' @param ink Foreground colour used for text, axes and other theme elements.
#' @param paper Background colour used for the plot.
#' @param accent Accent colour used for grid lines and other theme elements.
#' @param legend.position Position of the legend. Defaults to `"inside"` for
#'   `theme_nsw()`.
#' @param legend.position.inside Two-element numeric vector specifying the
#'   position of an inside legend. Defaults to `c(0.02, 0.98)`.
#' @param legend.justification Justification of the legend. Defaults to
#'   `c(0, 1)` for `theme_nsw()`.
#' @param water_background Logical. For maps, whether to use a blue panel
#'   background to represent water.
#' @param ... Additional arguments passed to [ggplot2::theme()].
#'
#' @returns A [ggplot2::theme()] object.
#'
#' @references
#' NSW Government masterbrand guidelines:
#' \url{https://www.nsw.gov.au/sites/default/files/_static/nsw-gov-masterbrand-guidelines.pdf}
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   theme_nsw() +
#'   scale_colour_nsw()
#'
#' # Facets
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_jitter() +
#'   facet_wrap(~manufacturer, nrow = 3) +
#'   theme_nsw() +
#'   scale_colour_nsw()
#'
#' @name theme_nsw
NULL

#' @rdname theme_nsw
#' @export
theme_nsw <- function(budget = FALSE, textbox = FALSE, 
                      base_size = 9, base_line_size = base_size/22, 
                      base_family = "Public Sans", header_family = "Public Sans SemiBold", 
                      ink = grey_01, paper = "white", accent = grey_04,
                      legend.position = "inside", 
                      legend.position.inside = c(0.02,0.98), 
                      legend.justification = c(0,1), ...){
  half_line <- base_size / 2
  half_line_mm <- half_line / .pt
  
  if(budget){
    strip_background <- accent
    strip_text <- ink
  } else{
    strip_background <- blue_01
    strip_text <- paper
  }
  
  if(textbox){
    plot_title <- ggtext::element_textbox(family = header_family, colour = blue_01, size = rel(11/9), hjust = 0.5, vjust = 1, halign = 0.5, margin = margin(t = 0, r = 0, b = 2, l = 0, unit = 'mm'), width = grid::unit(1, "npc"))
    plot_caption <- ggtext::element_textbox_simple(size = base_size, hjust = 0, margin = margin(t = 3-half_line_mm, r = 0, b = 0, l = 0, unit = 'mm'))
  } else{
    plot_title <- element_text(family = header_family, colour = blue_01, size = rel(11/9), hjust = 0.5, vjust = 1, margin = margin(t = 0, r = 0, b = 2, l = 0, unit = 'mm'))
    plot_caption <- element_text(size = base_size, hjust = 0, margin = margin(t = 3-half_line_mm, r = 0, b = 0, l = 0, unit = 'mm'))
  }
  
  ggplot2::theme_grey(base_size = base_size, base_family = base_family, header_family = header_family, base_line_size = base_line_size, base_rect_size = base_line_size, ink = ink, paper = paper, accent = accent) %+replace% 
    ggplot2::theme(
      # Main elements
      text = element_text(family = base_family, colour = ink, size = base_size), 
      line = element_line(colour = ink, linewidth = base_line_size),
      rect = element_rect(fill = NA, colour = NA, linewidth = base_line_size),
      
      # Geom elements (verison 4.0.0 onwards)
      geom = element_geom(
        # Line width used across hline/vline/abline/path/line/etc.
        # linewidth = base_line_size,
        family = header_family
        ),

      # Plot elements
      plot.background = element_rect(fill = paper),
      plot.title = plot_title,
      plot.title.position = 'plot',
      plot.subtitle = element_text(colour = blue_01, size = base_size, hjust = 0.5, vjust = 1, margin = margin(t = 0, r = 0, b = 3-half_line_mm, l = 0, unit = 'mm')),
      
      plot.caption = plot_caption,
      plot.caption.position = 'plot',
      
      plot.tag = element_text(family = header_family, colour = blue_01, size = base_size),
      plot.tag.position = 'top',
      
      # Axis elements
      axis.text = element_text(size = base_size),
      axis.title = element_text(family = base_family),
      # axis.text.x = element_text(margin = margin(0.75,0.75,0.75,0.75, unit = "mm")),
      # axis.text.y = element_text(margin = margin(0.75,0.75,0.75,0.75, unit = "mm")),
      # axis.title.x = element_text(margin = margin(2,0,0,0, unit = "mm")),
      # axis.title.x.top = element_text(margin = margin(0,0,2,0, unit = "mm")),
      # axis.title.y = element_text(margin = margin(0,2,0,0, unit = "mm")),
      # axis.title.y.right = element_text(margin = margin(0,0,0,2, unit = "mm")),
      
      axis.line = element_line(colour = ink, lineend = "square"),
      axis.ticks = element_line(colour = ink),
      axis.ticks.length = unit(1, units = 'mm'),
      axis.minor.ticks.length = rel(1),
      
      # Legend elements
      legend.position = legend.position,
      legend.position.inside = legend.position.inside,
      legend.justification = legend.justification,
      legend.text = element_text(size = base_size),
      legend.margin = margin(0, half_line, 0, half_line),
      legend.key.size = unit(0.75, 'lines'),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.spacing = unit(-1, 'mm'),
      
      # Panel elements (borders, background and gridlines)
      panel.border = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linetype = 1, colour = accent),
      panel.grid.minor.y = element_blank(),
      
      # Strip (facet label) elements
      strip.background = element_rect(fill = strip_background, colour = strip_background, linewidth = base_line_size),
      strip.text = element_text(family = header_family, colour = strip_text, margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = 'mm')),
      ...
    )
}

#' @rdname theme_nsw
#' @export
theme_nsw_map <- function(budget = FALSE, water_background = FALSE, textbox = FALSE, base_size = 9, base_line_size = base_size/22, base_family = "Public Sans", header_family = "Public Sans SemiBold", 
                          ink = grey_01, paper = "white", accent = grey_04,
                          legend.position = c(0.97,0.03), legend.justification = c(1,0), ...){
  if(water_background) {
    water.bg <- blue_04
  } else {
    water.bg <- NA
  }
  
  theme_nsw(
    budget = budget, 
    textbox = textbox,
    base_size = base_size, 
    base_family = base_family, 
    base_line_size = base_line_size, 
    ink = ink, 
    paper = paper, 
    accent = accent,
    legend.position = legend.position, 
    legend.justification = legend.justification
  ) %+replace% 
    ggplot2::theme(
      panel.background = element_rect(fill = water.bg), 
      panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      plot.margin = margin(0,0,0,0),
      ...)
}


#' Adjust the plot margin
#'
#' Increase one or more margins around a plot. For example, increase `right`
#' when additional space is needed for an x-axis label.
#'
#' @param right,left,top,bottom Amount, in points, to add to each plot
#'   margin. Defaults to `0`.
#'
#' @returns A [ggplot2::theme()] object that modifies the plot margin.
#'
#' @export
adjust_margin <- function(right = 0, left = 0, top = 0, bottom = 0){
  base_size <- 9
  half_line <- base_size/2
  theme(plot.margin = margin(t = half_line+top, r = half_line+right, b = half_line+bottom, l = half_line+left))
}


#' Plot labels with useful defaults
#'
#' A wrapper around [ggplot2::labs()] with defaults used by nswtheme.
#'
#' Compared with [ggplot2::labs()], `labs_nsw()`:
#'
#' * combines `tag` and `title` on the same line;
#' * defaults to `x = NULL`, which is useful for untitled date axes; and
#' * can convert the combined tag and title to uppercase.
#'
#' @param title Plot title.
#' @param x X-axis title. Defaults to `NULL`.
#' @param ... Other arguments passed to [ggplot2::labs()].
#' @param tag Plot tag. When supplied, it is combined with `title`.
#' @param uppercase Logical. If `TRUE`, convert the tag and title to
#'   uppercase. Defaults to `FALSE`.
#'
#' @returns A ggplot2 labels object.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(economics, aes(date, uempmed)) +
#'   geom_line() +
#'   labs_nsw(
#'     tag = "Figure 0.0:",
#'     title = "Median duration of unemployment",
#'     y = "Weeks"
#'   )
#'
#' @export
labs_nsw <- function(title = NULL, x = NULL, ..., tag = NULL, uppercase = FALSE) {
  if (!is.null(tag)) {
    if (uppercase) {
      tag <- paste0("**", tag, "**")
    }
      title <- paste(tag, title)
  }
  if (uppercase & !is.null(title)){
    title <- toupper(title)
  }
  labs(title = title, x = x, ...)
}


#' Save a plot with NSW Government defaults
#'
#' A wrapper around [ggplot2::ggsave()] with convenient default dimensions
#' and output devices.
#'
#' `ggsave_nsw()`:
#'
#' * defaults to a size of 15.24 cm by 9.98 cm;
#' * slightly increases the default height when the plot has a title; and
#' * selects a Cairo device automatically for PDF and EPS output.
#'
#' @param filename File name for the saved plot.
#' @param plot Plot or grid object to save. Defaults to
#'   [ggplot2::last_plot()].
#' @param width,height Plot dimensions. Defaults to `15.24` cm wide and
#'   `9.98` cm high.
#' @param units Units for `width` and `height`. Defaults to `"cm"`.
#' @param dpi Plot resolution. Defaults to `700`.
#' @param device Graphics device. If `NULL`, an appropriate device is
#'   selected from the file extension where supported.
#' @param ... Other arguments passed to [ggplot2::ggsave()].
#'
#' @returns Invisibly returns the value returned by [ggplot2::ggsave()].
#'
#' @references
#' \url{https://nswgov.sharepoint.com/sites/inside-treasury/SitePages/Brand-Hub.aspx}
#'
#' @export
ggsave_nsw <- function(filename, plot = last_plot(), width = 15.24, height = 9.98, units = 'cm', 
                       dpi = 700, device = NULL, ...){
  p <- plot
  wid <- width
  hgt <- height
  # If the plot has a title element AND height is at the default...
  if(!is.null(p$labels$title) & hgt == 9.98){
    # Then increase the height a little bit...
    hgt <- hgt * 1.05
  }
  
  # Guess the optimal cairo device... 
  dev <- device
  if(is.null(dev)){
    if(endsWith(tolower(filename), ".pdf")){
      dev <- cairo_pdf
    } else if(endsWith(tolower(filename), ".eps")){
      dev <- cairo_ps
    } else if(endsWith(tolower(filename), ".png")){
      dev <- png
    } else{NULL}
  }
  
  ggplot2::ggsave(
    filename = filename,
    plot = p,
    width = wid,
    height = hgt,
    units = units,
    dpi = dpi,
    device = dev,
    # type = type,
    # bg = bg,
    ...
  )
}
