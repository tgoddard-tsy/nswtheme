#' Inflation target lines or shading
#' 
#' Highlight the RBA inflation target with either shading or horizontal lines.
#' @param min,max Values to use in the plot. Defaults to 2 and 3, respectively. 
#' @param official A logical determining whether to use the start date of only official, mandated targeting. Inflation targeting was adopted by the RBA in 1993, but was not mandated as an official target until 1996.
#' @param start.date A date, which if specified will override the default start date of inflation targeting. If used, \code{official} will be ignored.
#' @param colour,fill,alpha Colour/fill of lines/shading. Defaults to \code{grey_01}.
#' @param label A character string to annotate the target with. Defaults to "RBA Inflation Target".
#' @param label.x,label.y x- and y- coordinates for where to position the \code{label}.
#' @param label.size Size of \code{label} text (in pts). Defaults to 7, which is slightly smaller than other text (9).
#' @param label.colour Colour of \code{label} text. Defaults to be the same as \code{colour} or \code{fill}.
#' @param facet If used in a faceted ggplot, specify the facet(s) you want the inflation target to appear on.
#' @param facet.name The variable name used in your \code{facet_wrap()}. Defaults to "facet", i.e. \code{facet_wrap(~facet)}.
#' @references Statement on the Conduct of Monetary Policy, Reserve Bank of Australia, September 1996, \url{https://www.rba.gov.au/monetary-policy/framework/stmt-conduct-mp-1-14081996.html}
#' @examples 
#' # Data setup:
#' library(tidyverse)
#' library(readabs)
#' cpi <- read_abs(series_id = c("A3604506F", "A3604509L", "A3604503X"))
#' infl <- select(cpi, date, variable = series, value) |> 
#'   growth(4) |> 
#'   mutate(variable = str_remove(variable, "Index Numbers ;  "),
#'   variable = str_remove(variable, " ;  Australia ;")) |> 
#'   filter(year(date) >= 1990)
#' 
#' # Plot:
#' plot <- ggplot(infl, mapping = aes(x = date, y = value, col = variable)) +
#' geom_line() +
#' geom_hline(yintercept = 0) +
#' scale_colour_tsy(values = c(teal_01, fuchsia_02, fuchsia_03)) +
#' labs_tsy(y = "%, Annual change", caption = "Source: ABS, NSW Treasury", title = "Headline and Underlying inflation") +
#' theme(legend.position = c(0.5,0.95))
#' 
#' plot + inflation_target_shading()
#' plot + inflation_target_lines()
#' @name inflation
NULL

#' @rdname inflation
#' @export
inflation_target_lines <- function(min = 2, max = 3, official = FALSE, start.date = NULL, linetype = "dashed", colour = grey_01, label = "RBA Inflation Target", label.x = "2017-08-01", label.y = 3, label.size = 7, label.colour = NULL, facet = NULL, facet.name = "facet", ...){
  if(is.null(label.colour)) {
    label.col <- colour
  }
  else {
    label.col <- label.colour
  }
  
  if(is.null(start.date)){
    if(official) {
      start_date <- as.Date("1996-08-14")}
    else{
      start_date <- as.Date("1993-06-01")
    }
  } else{
    start_date <- start.date
  }
  
  if(is.null(facet)) {  
    df <- data.frame(xmin = start_date, xmax = as.Date(Inf), ymin = min, ymax = max,
                     x = as.Date(label.x), y = label.y, label = label)
  }
  else {
    df <- data.frame(xmin = start_date, xmax = as.Date(Inf), ymin = min, ymax = max,
                     x = as.Date(label.x), y = label.y, label = label, facet_temp = factor(facet))
    colnames(df)[which(names(df) == "facet_temp")] <- facet.name
  }
  
  list(
    geom_linerange(
      data = df, mapping = aes(xmin = xmin, xmax = xmax, y = min), 
      colour = colour, linetype = linetype, inherit.aes = FALSE, ...
    ),
    geom_linerange(
      data = df, mapping = aes(xmin = xmin, xmax = xmax, y = max), 
      colour = colour, linetype = linetype, inherit.aes = FALSE, ...
    ),
    geom_text(
      data = df, aes(x = x, y = y, label = label), 
      colour = label.col, inherit.aes = FALSE, family = 'Public Sans', fontface = 'plain', vjust = -0.2,
      size = label.size / .pt
    )
  )
}

#' @rdname inflation
#' @export
inflation_target_shading <- function(min = 2, max = 3, official = FALSE, start.date = NULL, fill = grey_01, alpha = 0.1, label = "RBA Inflation Target", label.x = "2017-08-01", label.y = 3, label.size = 7, label.colour = NULL, facet = NULL, facet.name = "facet", ...){
  if(is.null(label.colour)) {
    label.col <- fill
  }
  else {
    label.col <- label.colour
  }
  
  if(is.null(start.date)){
    if(official) {
      start_date <- as.Date("1996-08-14")}
    else{
      start_date <- as.Date("1993-06-01")
    }
  } else{
    start_date <- start.date
  }
  
  if(is.null(facet)) {
    df <- data.frame(xmin = start_date, xmax = as.Date(Inf), ymin = min, ymax = max,
                     x = as.Date(label.x), y = label.y, label = label)
  }
  else {
    df <- data.frame(xmin = start_date, xmax = as.Date(Inf), ymin = min, ymax = max,
                     x = as.Date(label.x), y = label.y, label = label, facet_temp = factor(facet))
    colnames(df)[which(names(df) == "facet_temp")] <- facet.name
  }
  
  list(
    geom_rect(
      data = df, 
      mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      fill = fill, alpha = alpha, inherit.aes = FALSE, ...),
    geom_text(
      data = df, aes(x = x, y = y, label = label), 
      colour = label.col, inherit.aes = FALSE, family = 'Public Sans', fontface = 'plain', vjust = -0.2,
      size = label.size / .pt
    )
  )
}
