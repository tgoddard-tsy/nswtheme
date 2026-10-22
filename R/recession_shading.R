#' Add recession shading to a plot
#'
#' Add shaded regions representing periods of economic recession.
#'
#' `recession_shading()` is a wrapper around [ggplot2::geom_rect()] with
#' defaults for plotting recession periods. By default, it uses
#' [recessions_aus], with `peak` and `trough` defining the beginning and end
#' of each shaded period.
#'
#' @param data Data frame containing recession periods. Defaults to
#'   [recessions_aus].
#' @param mapping Aesthetic mapping passed to [ggplot2::geom_rect()]. By
#'   default, `xmin = peak`, `xmax = trough`, `ymin = -Inf` and
#'   `ymax = Inf`.
#' @param fill Fill colour for the shaded regions. Defaults to `grey_01`.
#' @param alpha Transparency of the shaded regions. Defaults to `0.1`.
#' @param show.legend Logical indicating whether this layer should be included
#'   in plot legends. Defaults to `FALSE`.
#' @param ... Other arguments passed to [ggplot2::geom_rect()].
#'
#' @returns A ggplot2 layer that can be added to a plot.
#'
#' @seealso [lockdown_shading()], [recessions_aus], [recessions_nsw]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(economics, aes(date, unemploy)) +
#'   recession_shading() +
#'   geom_line()
#'
#' # Use NSW recession periods
#' ggplot(economics, aes(date, unemploy)) +
#'   recession_shading(data = recessions_nsw) +
#'   geom_line()
#'
#' @export
recession_shading <- function(data = recessions_aus, mapping = aes(x = NULL, y = NULL, colour = NULL, group = NULL, linetype = NULL,
                                                                   xmin = peak, xmax = trough, ymin = -Inf, ymax = Inf), 
                              fill = grey_01, alpha = 0.1, show.legend = FALSE, ...){
  geom_rect(data = data, 
            mapping = mapping, 
            fill = fill, alpha = alpha, 
            show.legend = show.legend, ...)
}


#' Add COVID-19 lockdown shading to a plot
#'
#' Add shaded regions representing periods of COVID-19 lockdown.
#'
#' `lockdown_shading()` is a wrapper around [ggplot2::geom_rect()] with
#' defaults for plotting lockdown periods. By default, it uses
#' [lockdowns_nsw], with `start` and `end` defining each shaded period.
#'
#' The `freq` argument adjusts lockdown start and end dates to the boundaries
#' of the requested period before the shading is added.
#'
#' @param freq Frequency at which to display lockdown periods. Defaults to
#'   `"d"`. Supported values include:
#'
#'   * `"d"` for daily;
#'   * `"w"` for weekly;
#'   * `"m"` for monthly;
#'   * `"q"` for quarterly;
#'   * `"y"` for calendar year; and
#'   * `"fy"` for financial year.
#' @param data Data frame containing lockdown periods. Defaults to
#'   [lockdowns_nsw].
#' @param mapping Aesthetic mapping passed to [ggplot2::geom_rect()]. By
#'   default, `xmin = start`, `xmax = end`, `ymin = -Inf` and
#'   `ymax = Inf`.
#' @param fill Fill colour for the shaded regions. Defaults to `grey_01`.
#' @param alpha Transparency of the shaded regions. Defaults to `0.1`.
#' @param show.legend Logical indicating whether this layer should be included
#'   in plot legends. Defaults to `FALSE`.
#' @param ... Other arguments passed to [ggplot2::geom_rect()].
#'
#' @returns A ggplot2 layer that can be added to a plot.
#'
#' @seealso [recession_shading()], [lockdowns_nsw], [lockdowns_vic],
#'   [lockdowns_qld], [lockdowns_wa]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(economics, aes(date, unemploy)) +
#'   lockdown_shading() +
#'   geom_line()
#'
#' # Use Victorian lockdown periods
#' ggplot(economics, aes(date, unemploy)) +
#'   lockdown_shading(data = lockdowns_vic) +
#'   geom_line()
#'
#' @export
lockdown_shading <- function(freq = "d", data = lockdowns_nsw, mapping = aes(x = NULL, y = NULL, colour = NULL, group = NULL, linetype = NULL,
                                                                             xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
                             fill = grey_01, alpha = 0.1, show.legend = FALSE, ...){
  df <- data
  f <- tolower(freq)
  # Week:
  if(startsWith(f, "w")){
    wday(df$start) <- 1
    wday(df$end) <- 7
  } 
  # Month:
  else if(startsWith(f, "m")){
    mday(df$start) <- 1
    mday(df$end) <- days_in_month(df$end)
  } 
  # Quarter:
  else if(startsWith(f, "q")){
    month(df$start) <- quarter(df$start) * 3 - 2
    month(df$end) <- quarter(df$end) * 3
    mday(df$start) <- 1
    mday(df$end) <- days_in_month(df$end)
  } 
  # Financial Year:
  else if(startsWith(f, "fy") | endsWith(f, "fy")){
    year(df$start) <- ifelse(month(df$start) <= 6, year(df$start) - 1, year(df$start))
    year(df$end) <- ifelse(month(df$end) <= 6, year(df$end), year(df$end) + 1)
    day(df$start) <- 1
    month(df$start) <- 7
    day(df$end) <- 30
    month(df$end) <- 6
  } 
  # Calendar Year:
  else if(startsWith(f, "cy") | endsWith(f, "cy") | startsWith(f, "y") | startsWith(f, "a")){
    day(df$start) <- 1
    month(df$start) <- 1
    day(df$end) <- 31
    month(df$end) <- 12
  }
  
  df <- df |> 
    dplyr::group_by(start, end) |> 
    dplyr::summarise(days = sum(days),
                     n_lockdowns = dplyr::n()
                     )
  
  geom_rect(data = df, 
            mapping = mapping, 
            fill = fill, alpha = alpha, 
            show.legend = show.legend, ...)
}
