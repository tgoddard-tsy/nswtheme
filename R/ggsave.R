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
