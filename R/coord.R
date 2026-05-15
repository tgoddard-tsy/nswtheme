#' Coordinate transformations with corresponding changes to the ggplot theme
#' 
#' Wrapper functions for \code{coord_flip()} and \code{coord_polar()}. 
#' These functions transform your coordinate space; the \code{coord_*_nsw} equivalent does this \emph{AND} makes corresponding amendments to the NSW Government Brand theme.
#' \itemize{
#'  \item{flip: Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal.}
#'  \item{polar: The polar coordinate system is most commonly used for pie charts, which are a stacked bar chart in polar coordinates.}
#' }
#' @param ... Optional arguments passed on to \code{\link[ggplot2]{coord_flip}()} or \code{\link[ggplot2]{coord_polar}()}.
#' @references \url{https://nswgov.sharepoint.com/sites/inside-treasury/SitePages/Brand-Hub.aspx}
#' @examples 
#' ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
#'   geom_boxplot() +
#'   coord_flip_nsw()
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
