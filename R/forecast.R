#' Forecast line or shading
#' 
#' Distinguish forecasts from historical data with either shading or a vertical line.
#' @param date The latest historical date (before forecasts start).
#' @param label A character string to annotate the forecasts with. Defaults to "Forecast".
#' @param facet If used in a faceted ggplot, specify the facet(s) you want the \code{label} to appear on.
#' @param facet.variable If used in a faceted ggplot, the variable name used in your \code{facet_wrap()}. Defaults to "category", i.e. \code{facet_wrap(~category)}.
#' @name forecast
NULL

#' @rdname forecast
#' @export
forecast_shading <- function(date, label = "Forecast", facet = NULL, facet.variable = "category", fill = grey_02, alpha = 0.1){
  if(is.null(facet)) {
    df <- data.frame(x = date, y = Inf, label = label)}
  else{
    df <- data.frame(x = date, y = Inf, label = label, facet_temp = factor(facet))
    colnames(df)[which(names(df) == "facet_temp")] <- facet.variable
  }
  
  list(
    annotate("rect", xmin = as.Date(date), xmax = as.Date(Inf), ymin = -Inf, ymax = Inf, fill = fill, alpha = alpha),
    geom_text(data = df, aes(x = x, y = y, label = label), inherit.aes = FALSE, family = 'Public Sans', fontface = 'plain', hjust = -0.05, vjust = 1.2))
}

#' @rdname forecast
#' @export
forecast_line <- function(date, label = "Forecast", facet = NULL, facet.variable = "facet", linetype = 1, colour = grey_01){
  if(is.null(facet)) {
    df <- data.frame(x = date, y = Inf, label = label)}
  else{
    df <- data.frame(x = date, y = Inf, label = label, facet_temp = factor(facet))
    colnames(df)[which(names(df) == "facet_temp")] <- facet.variable
  }
  
  list(
    geom_vline(xintercept = as.Date(date), linetype = linetype, colour = colour, mapping = NULL),
    geom_text(data = df, aes(x = x, y = y, label = label), inherit.aes = FALSE, family = 'Public Sans', fontface = 'plain', hjust = -0.05, vjust = 1))
}
