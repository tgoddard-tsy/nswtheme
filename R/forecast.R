#' Distinguish forecast and historical periods
#'
#' Add a vertical line or shaded region to distinguish forecast observations
#' from historical data.
#'
#' `forecast_line()` adds a vertical line at `date`, while
#' `forecast_shading()` shades the region after `date`. Both functions add
#' a text annotation, which defaults to `"Forecast"`.
#'
#' In faceted plots, `facet` and `facet_variable` can be used to display the
#' annotation only in selected facets.
#'
#' @param date Date marking the end of the historical period and the start
#'   of the forecast period.
#' @param label Character string used to annotate the forecast period.
#'   Defaults to `"Forecast"`.
#' @param fill Fill colour used by `forecast_shading()`. Defaults to
#'   `grey_02`.
#' @param alpha Transparency of the shading added by `forecast_shading()`.
#'   Defaults to `0.1`.
#' @param linetype Line type used by `forecast_line()`. Defaults to `1`.
#' @param colour Line colour used by `forecast_line()`. Defaults to
#'   `grey_01`.
#' @param facet Facet value or values in which to display `label`. If
#'   `NULL`, the annotation is displayed without restricting it to particular
#'   facets.
#' @param facet_variable Character string giving the name of the faceting
#'   variable when `facet` is supplied. Defaults to `"category"`.
#'
#' @returns A list of ggplot2 layers that can be added to a plot.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(economics, aes(date, unemploy)) +
#'   geom_line()
#'
#' # Mark the forecast period with a line
#' p + forecast_line(as.Date("2010-01-01"))
#'
#' # Shade the forecast period
#' p + forecast_shading(as.Date("2010-01-01"))
#'
#' # Change the annotation and line type
#' p + forecast_line(
#'   as.Date("2010-01-01"),
#'   label = "Projection",
#'   linetype = "dashed"
#' )
#'
#' @name forecast
NULL

#' @rdname forecast
#' @export
forecast_shading <- function(date, label = "Forecast", fill = grey_02, alpha = 0.1, facet = NULL, facet_variable = "category"){
  if(is.null(facet)) {
    df <- data.frame(x = date, y = Inf, label = label)}
  else{
    df <- data.frame(x = date, y = Inf, label = label, facet_temp = factor(facet))
    colnames(df)[which(names(df) == "facet_temp")] <- facet_variable
  }
  
  list(
    annotate("rect", xmin = as.Date(date), xmax = as.Date(Inf), ymin = -Inf, ymax = Inf, fill = fill, alpha = alpha),
    geom_text(data = df, aes(x = x, y = y, label = label), inherit.aes = FALSE, family = 'Public Sans', fontface = 'plain', hjust = -0.05, vjust = 1.2))
}

#' @rdname forecast
#' @export
forecast_line <- function(date, label = "Forecast", linetype = 1, colour = grey_01, facet = NULL, facet_variable = "category"){
  if(is.null(facet)) {
    df <- data.frame(x = date, y = Inf, label = label)}
  else{
    df <- data.frame(x = date, y = Inf, label = label, facet_temp = factor(facet))
    colnames(df)[which(names(df) == "facet_temp")] <- facet_variable
  }
  
  list(
    geom_vline(xintercept = as.Date(date), linetype = linetype, colour = colour, mapping = NULL),
    geom_text(data = df, aes(x = x, y = y, label = label), inherit.aes = FALSE, family = 'Public Sans', fontface = 'plain', hjust = -0.05, vjust = 1))
}
