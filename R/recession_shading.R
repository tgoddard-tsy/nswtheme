#' Add a layer of recession shading
#'
#' This is a wrapper for \code{\link[ggplot2]{geom_rect}()}, with convenient defaults for recession shading. Defaults to recessions in Australia dataset (\code{\link{recessions_aus}}).
#' @param ... Arguments passed to \code{geom_rect()}.
#' @seealso \code{\link{lockdown_shading}()}
#' @export
recession_shading <- function(data = recessions_aus, mapping = aes(x = NULL, y = NULL, colour = NULL, group = NULL, linetype = NULL,
                                                                   xmin = peak, xmax = trough, ymin = -Inf, ymax = Inf), 
                              fill = grey_01, alpha = 0.1, show.legend = FALSE, ...){
  geom_rect(data = data, 
            mapping = mapping, 
            fill = fill, alpha = alpha, 
            show.legend = show.legend, ...)
}

#' Add a layer of COVID-19 lockdown shading
#'
#' This is a wrapper for \code{\link[ggplot2]{geom_rect}()}, with convenient defaults for COVID-19 lockdown shading. Defaults to using the NSW lockdown dataset (\code{\link{lockdowns_nsw}}).
#' @param freq Frequency at which to show lockdown data. Defaults to "d". Options:\itemize{
#'   \item{Daily [default]: \code{freq = "d"}}
#'   \item{Weekly: \code{freq = "w"}}
#'   \item{Monthly: \code{freq = "m"}}
#'   \item{Quarterly: \code{freq = "q"}}
#'   \item{Yearly: \code{freq = "y"}}
#'   \item{Yearly (Financial): \code{freq = "fy"}}
#'   }
#' @param ... Arguments passed to \code{geom_rect()}.
#' @seealso \code{\link{recession_shading}()}
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
    group_by(start, end) |> 
    summarise(days = sum(days),
              n_lockdowns = n())
  
  geom_rect(data = df, 
            mapping = mapping, 
            fill = fill, alpha = alpha, 
            show.legend = show.legend, ...)
}
