#' Coordinate systems with NSW Government theme adjustments
#'
#' Coordinate-system wrappers that apply corresponding adjustments to the
#' NSW Government plot theme.
#'
#' `coord_flip_nsw()` flips Cartesian coordinates and adjusts the major grid
#' lines to suit the flipped axes.
#'
#' `coord_polar_nsw()` transforms Cartesian coordinates to polar coordinates.
#' It is intended primarily for pie and donut charts, where a bar chart is
#' transformed into a circular layout. It also removes axis lines, ticks,
#' labels, titles and grid lines that are generally unnecessary for these
#' charts.
#'
#' @param ... Arguments passed to [ggplot2::coord_flip()] or
#'   [ggplot2::coord_polar()].
#'
#' @returns A list containing a ggplot2 coordinate system and theme
#'   adjustments that can be added to a plot.
#'
#' @examples
#' library(ggplot2)
#'
#' # Flipped coordinates
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot() +
#'   coord_flip_nsw()
#'
#' # Pie chart
#' ggplot(mpg, aes(x = "", fill = drv)) +
#'   geom_bar(width = 1) +
#'   coord_polar_nsw(theta = "y")
#'
#' @seealso
#' [ggplot2::coord_flip()], [ggplot2::coord_polar()]
#'
#' @name coord_nsw
NULL

#' @rdname coord_nsw
#' @export
coord_flip_nsw <- function(...){
  list(
    ggplot2::coord_flip(...), 
    ggplot2::theme(
      panel.grid.major.x = element_line(linetype = 1, colour = grey_04),
      panel.grid.major.y = element_blank()
    )
  )
}

#' @rdname coord_nsw
#' @export
coord_polar_nsw <- function(...){
  list(
    ggplot2::coord_polar(...), 
    ggplot2::theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  )
}
