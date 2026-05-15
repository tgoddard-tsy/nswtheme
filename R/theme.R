#' ggplot theme based on \strong{NSW Government} brand guidelines
#'
#' A theme object for the style of \emph{New South Wales Government}.
#' @param ... Arguments passed on to \code{\link[ggplot2]{theme}()}.
#' @param legend.position The position of legends ("none", "left", "right", "bottom" or "top" for positioning legend outside plot) OR (two-element numeric vector for positioning legend inside plot). Defaults to \code{c(0.02,0.98)} (near the top-left corner of the plot area).
#' @param legend.justification anchor point for positioning legend inside plot ("center" or two-element numeric vector) or the justification according to the plot area when positioned outside the plot. Defaults to \code{c(0,1)} (anchored to top-left corner of the legend box).
#' @param water_background For maps, logical determining whether to use a blue \code{panel.background} to represent the presence of the oceans.
#' @returns An object of class \code{\link[ggplot2]{theme}()}
#' @references \url{https://www.nsw.gov.au/sites/default/files/_static/nsw-gov-masterbrand-guidelines.pdf}
#' @examples 
#' library("ggplot2")
#' 
#' p <- ggplot(data = filter(economics_long, variable %in% c('uempmed', 'psavert'))) + 
#'   geom_line(mapping = aes(x = date, y = value, colour = variable)) +
#'   theme_nsw() +
#'   scale_colour_nsw()
#'
#' # Facets:
#' p <- ggplot(data = mpg) +
#'   geom_jitter(mapping = aes(x = displ, y = hwy, colour = class)) +
#'   facet_wrap(~ manufacturer, nrow = 3)
#'   theme_nsw() +
#'   scale_colour_nsw()
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


#' Adjust the margin around plot
#' 
#' Increase \code{right} margin if you need more space for your x-axis label.
#' @param right,left,top,bottom Number of pts to increase margin by.
#' @export
adjust_margin <- function(right = 0, left = 0, top = 0, bottom = 0){
  base_size <- 9
  half_line <- base_size/2
  theme(plot.margin = margin(t = half_line+top, r = half_line+right, b = half_line+bottom, l = half_line+left))
}


#' A wrapper for labs() with useful defaults
#'
#' This is the same as using \code{\link[ggplot2]{labs}()}, except \code{labs_nsw()}:
#' \itemize{
#'  \item{Puts the \code{tag} and \code{title} together on the same line.}
#'  \item{Defaults to \code{x = NULL}; since it is usually a date axis which doesn't need to be titled (\code{labs()} default is \code{x = waiver()}).}
#'  \item{Provides an option to capitalise the \code{tag} and \code{title}.}
#' }
#' @param ... Arguments passed on to \code{\link[ggplot2]{labs}()}.
#' @param uppercase If \code{TRUE} (default), will capitalise \code{tag} and \code{title} text.
#' @examples 
#' library(ggplot2)
#' 
#' ggplot(economics, aes(x = date, y = uempmed)) +
#'   geom_line() +
#'   labs_nsw(tag = "Figure 0.0:",
#'            title = "Median duration of unemployment",
#'            y = "Weeks")
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


#' A wrapper for ggsave() with default dimensions
#'
#' Save a ggplot (or other grid object) with useful defaults. This is the same as \code{\link[ggplot2]{ggsave}()}, but with the following features: 
#' \itemize{
#'  \item{Default size of 15.24cm x 9.98cm (width x height)}
#'  \item{Selects the best \code{device} option, when saving file as ".pdf" (\code{cairo_pdf}) or ".eps" (\code{cario_ps}) extensions.}
#' }
#' @param ... Arguments passed on to \code{\link[ggplot2]{ggsave}()}.
#' @references \url{https://nswgov.sharepoint.com/sites/inside-treasury/SitePages/Brand-Hub.aspx}
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
