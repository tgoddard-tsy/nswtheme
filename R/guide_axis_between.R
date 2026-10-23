# Internal axis guide for labels positioned between ticks.
#
# Breaks are expected to alternate between:
#
#   true tick, midpoint label position, true tick, midpoint label position, ...
#
# The guide retains only the true tick positions when drawing tick marks.
# Labels are still built from the complete set of breaks.
#
# @noRd
GuideAxisBetween <- ggplot2::ggproto(
  "GuideAxisBetween",
  ggplot2::GuideAxis,
  
  build_ticks = function(
    key,
    elements,
    params,
    position = params$opposite
  ) {
    
    if (is.null(key) || nrow(key) == 0L) {
      return(
        ggplot2::GuideAxis$build_ticks(
          key = key,
          elements = elements,
          params = params,
          position = position
        )
      )
    }
    
    # Break positions alternate between:
    #
    #   true tick, midpoint, true tick, midpoint, ...
    #
    # Retain only the true tick positions.
    keep <- seq.int(
      from = 1L,
      to = nrow(key),
      by = 2L
    )
    
    tick_key <- key[
      keep,
      ,
      drop = FALSE
    ]
    
    # Delegate the actual drawing to ggplot2's standard axis guide.
    #
    # This means tick colour, linewidth, linetype and other properties
    # continue to come from the active plot theme.
    ggplot2::GuideAxis$build_ticks(
      key = tick_key,
      elements = elements,
      params = params,
      position = position
    )
  }
)


#' Axis guide for labels positioned between ticks
#'
##' An axis guide for scales whose breaks alternate between true tick positions
#' and intermediate label positions. Tick marks are drawn only at the true
#' tick positions, allowing labels supplied at the intermediate positions to
#' appear centred between ticks.
#'
#' Breaks are expected to alternate in the form
#' `tick, label position, tick, label position, ...`. The first break and every
#' second break thereafter are treated as true tick positions. The guide does
#' not calculate intermediate positions or labels; these must be supplied by
#' the scale.
#' 
#' @inheritParams ggplot2::guide_axis
#'
#' @returns A ggplot2 axis guide.
#'
#' @examples
#' library(ggplot2)
#'
#' # Position labels between ticks on a standard ggplot2 date scale
#' breaks <- as.Date(c(
#'   "2000-01-01", "2000-07-01",
#'   "2001-01-01", "2001-07-01",
#'   "2002-01-01", "2002-07-01",
#'   "2003-01-01"
#' ))
#'
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line() +
#'   scale_x_date(
#'     breaks = breaks,
#'     labels = c("", "2000", "", "2001", "", "2002", ""),
#'     limits = as.Date(c("2000-01-01", "2003-01-01")),
#'     guide = guide_axis_between()
#'   )
#'
#' # The guide can also be supplied separately with guides()
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   scale_y_continuous(
#'     breaks = c(10, 15, 20, 25, 30),
#'     labels = c("", "10-20", "", "20-30", "")
#'   ) +
#'   guides(y = guide_axis_between())
#'   
#' @seealso [ggplot2::guide_axis()]
#'
#' @export
guide_axis_between <- function(
    title = ggplot2::waiver(),
    theme = NULL,
    check.overlap = FALSE,
    angle = ggplot2::waiver(),
    n.dodge = 1,
    order = 0,
    position = ggplot2::waiver()
) {
  ggplot2::new_guide(
    title = title,
    theme = theme,
    check.overlap = check.overlap,
    angle = angle,
    n.dodge = n.dodge,
    minor.ticks = FALSE,
    cap = "none",
    order = order,
    position = position,
    available_aes = c("x", "y"),
    name = "axis_between",
    super = GuideAxisBetween
  )
}
