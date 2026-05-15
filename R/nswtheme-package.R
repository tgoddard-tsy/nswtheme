#' @section How to use with \code{ggplot2}:
#' \itemize{
#'  \item{Run \code{\link{set_defaults_nsw}()} to update defaults and load fonts}
#'  \item{Once you've created a chart with \code{ggplot2}, you can edit the theme elements to suit your needs with \code{\link[ggplot2]{theme}()}. For example, \code{legend.position}.}
#'  \item{You can change the colour scheme by using \code{\link{scale_colour}_nsw()} or similar, depending on the mapping/aesthetics used.}
#'  \item{Save a plot using \code{\link{ggsave_nsw}()}.}
#' }
#' 
#' @section Colour palette:
#' \if{html}{\figure{nswgov_palette.png}{options: width=700 alt="NSW Government palette"}}
#' @references Masterbrand quick reference guide: \url{https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf}
#' @keywords internal
"_PACKAGE"