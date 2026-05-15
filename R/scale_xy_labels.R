#' Label functions with sensible defaults
#' 
#' \itemize{
#'   \item{\code{label_comma_nsw} function behaves like \code{\link[scales]{label_comma}()}, but defaults to \code{drop0trailing = TRUE}.}
#'   \item{\code{fy} takes a vector of dates as inputs, and returns financial years as a \code{character} vector.}
#'   }
#' @param ... Arguments passed on to \code{\link[scales]{number}()}.
#' @param x Dates to translate into financial years.
#' @name label_nsw
NULL

#' @rdname label_nsw
#' @export
en_dash <- "\u2013"

#' @rdname label_nsw
#' @export
em_dash <- "\u2014"

#' @rdname label_nsw
#' @export
label_comma_nsw <- function(accuracy = NULL, scale = 1, prefix = "", suffix = "", 
                            big.mark = ",", decimal.mark = ".", trim = TRUE, drop0trailing = TRUE, ...) {
  function(x) scales::number(x, accuracy = accuracy, scale = scale, 
                             prefix = prefix, suffix = suffix, big.mark = big.mark, 
                             decimal.mark = decimal.mark, trim = trim, drop0trailing = drop0trailing, ...)
}

#' @rdname label_nsw
#' @export
fy <- Vectorize(function(x, sep = en_dash){
  if(lubridate::month(x) <= 6){
    # If date is Jan-Jun, then use the year x-1 and x
    fy_label <- paste0(year(x) - 1, sep, sprintf('%02d', (year(x)) %% 100))
  } else{
    # If date is Jul-Dec, then use the year x and x+1
    fy_label <- paste0(year(x), sep, sprintf('%02d', (year(x) + 1) %% 100))
  }
  fy_label
})

#' A wrapper for scale_*_date() that makes axis labelling easier
#' 
#' Specify your desired range of dates (or years), and formatting for \code{date} labels and ticks including whether or not to display in financial year notation.
#' @param limits A vector of dates (of length 2), specifying [1] the minimum date and [2] the maximum date for the axis. Also accepts year values (numerics); these are converted to dates at the start [1] and end [2] of the given years, respectively (depends on \code{FY}).
#' @param by A string giving the distance between breaks/ticks (e.g. "2 weeks", or "10 years") - similar to date_breaks in . Defaults to '1 year'.
#' @param major Number of periods between date labels (Defaults to 1, i.e. label every period/year).
#' @param format \code{character} string of date format. Passed to \code{\link[base]{format.Date}()}. This argument is ignored if \code{FY = TRUE}. Example \code{2020-03-01}: \itemize{
#'   \item{`format = "\%Y"` results in "2020"}
#'   \item{`format = "\%y"` results in "20"}
#'   \item{`format = "\%B"` results in "March"}
#'   \item{`format = "\%d-\%b"` results in "01-Mar"}}
#' @param FY If \code{TRUE}, axis labels will be in the format "2011-12".
#' @param label.between.ticks If \code{TRUE}, will position the axis labels between tick marks - useful for showing the span of years more clearly.
#' @param skip How many periods to skip before date labels appear. Defaults to 0.
#' @param expand.mult,expand.add Values passed to \code{\link[ggplot2]{expansion}()}. These can be used to expand the axis beyond the \code{limits} supplied, in one- or both- directions.
#' @param oob Function that handles limits outside of the scale limits (out of bounds). Defaults to \code{scales::\link[scales]{oob_keep}} to keep out of bounds data.
#' @param ... Other arguments passed on to scale date function.
#' @seealso \code{\link[ggplot2]{scale_x_date}()} and \code{\link[ggplot2]{scale_y_date}()}
#' @examples 
#' date_range <- c(lubridate::dmy('1-1-2000'), lubridate::dmy('31-12-2020'))
#' 
#' scale_x_date_nsw(xlim = date_range, label.between.ticks = TRUE, major = 2)
#' @name scale_date_nsw
NULL

#' @rdname scale_date_nsw
#' @export
scale_x_date_nsw <- function(limits, by = '1 year', major = 1, format = '%Y', FY = FALSE, label.between.ticks = FALSE, skip = 0, expand.mult = c(0,0), expand.add = c(0,0), oob = scales::oob_keep, ...){
  lim <- limits
  # If limits are given as year numbers, turn them into dates: 
  if(is.numeric(lim[1])){
    # Make start limit at the start of year
    if(FY){
      lim[1] <- lubridate::make_date(year = lim[1] - 1, month = 7, day = 1)
    } else{
      lim[1] <- lubridate::make_date(year = lim[1], month = 1, day = 1)
    }
  }
  if(is.numeric(lim[2]) & lim[2] < 3001){
    # Make end limit at the end of year
    if(FY){
      lim[2] <- lubridate::make_date(year = lim[2], month = 6, day = 30)
    } else{
      lim[2] <- lubridate::make_date(year = lim[2], month = 12, day = 31)
    }
  }
  if(is.numeric(lim)){
    lim <- as.Date(lim, origin = lubridate::origin)
  }
  
  ticks <- seq.Date(lim[1], lim[2], by = by)
  
  if(label.between.ticks){
    # Calculate days between ticks and then place new dates in between them...
    ticks_diff <- lead(ticks) - ticks
    ticks_diff[is.na(ticks_diff)] <- max(ticks_diff, na.rm = TRUE)
    ticks_diff_half <- round(ticks_diff / 2)
    
    ticks_double <- append(ticks, ticks + ticks_diff_half)
    ticks_double <- sort(ticks_double)
    
    # Initialise labels from original ticks values (to make labels reflect actual ticks instead of midpoints)
    x_labels <- rep(ticks, each = 2)
    if(FY){
      x_labels <- fy(x_labels)
    } else{
      x_labels <- format.Date(x_labels, format)
    }
    # Replace every second label with blanks (TRUE = remove):
    drop <- rep(TRUE, length(x_labels))
    drop[seq(2 + skip*2, length(drop), major*2)] <- FALSE # start at 2 because that's where the first midpoint is
    x_labels[drop] <- ''
    if(length(x_labels) > length(ticks_double)){
      x_labels <- x_labels[-length(x_labels)] # If there are too many x_labels, then drop the last element
    }
    list(
      ggplot2::scale_x_date(labels = x_labels, breaks = ticks_double, limits = lim, 
                            expand = expansion(mult = expand.mult, add = expand.add), oob = oob, ...),
      theme(axis.ticks.x = element_line(colour = c(grey_01, NA))) # removes midpoint ticks from the theme element
    )
  } 
  else{
    if(FY){
      x_labels <- fy(ticks)
    } 
    else{
      x_labels <- format.Date(ticks, format)
    }
    drop <- rep(TRUE, length(x_labels))
    drop[seq(1 + skip, length(drop), major)] <- FALSE
    x_labels[drop] <- ''
    ggplot2::scale_x_date(labels = x_labels, breaks = ticks, limits = lim, 
                          expand = expansion(mult = expand.mult, add = expand.add), oob = oob, ...)
  }
}

#' @rdname scale_date_nsw
#' @export
scale_y_date_nsw <- function(limits, by = '1 year', major = 1, format = '%Y', FY = FALSE, label.between.ticks = FALSE, skip = 0, expand.mult = c(0,0), expand.add = c(0,0), oob = scales::oob_keep, ...){
  lim <- limits
  
  # if (missing(limits)){ # limits are missing, then use waiver
  #   if (missing(limits)){
  #     lim <- waiver()
  #   }
  #   if (FY){
  #     x_labels <- fy()
  #   }
  #   ggplot2::scale_y_date(labels = x_labels, date_breaks = by, limits = lim, 
  #                         expand = expansion(mult = expand.mult, add = expand.add), oob = oob, ...)
  #   
  # } 
  # else{                        # If limits are provided, then use seq
    # If limits given are as years, turn them into dates: 
    if(is.numeric(lim[1])){
      if(FY){
        lim[1] <- lubridate::make_date(year = lim[1] - 1, month = 7, day = 1)
      } else{
        lim[1] <- lubridate::make_date(year = lim[1], month = 1, day = 1)
      }
    }
    if(is.numeric(lim[2]) & lim[2] < 3001){
    if(FY){
      lim[2] <- lubridate::make_date(year = lim[2], month = 6, day = 30)
    } else{
      lim[2] <- lubridate::make_date(year = lim[2], month = 12, day = 31)
      }
    }
    if(is.numeric(lim)){
      lim <- as.Date(lim, origin = lubridate::origin)
    }
    
    ticks <- seq.Date(lim[1], lim[2], by = by)
  # }
  if(label.between.ticks){
    # Calculate days between ticks and then place new dates in between them...
    ticks_diff <- lead(ticks) - ticks
    ticks_diff[is.na(ticks_diff)] <- max(ticks_diff, na.rm = TRUE)
    ticks_diff_half <- round(ticks_diff / 2)
    
    ticks_double <- append(ticks, ticks + ticks_diff_half)
    ticks_double <- sort(ticks_double)
    
    # Initialise labels from original ticks values (to make labels reflect actual ticks instead of midpoints)
    x_labels <- rep(ticks, each = 2)
    if(FY){
      x_labels <- fy(x_labels)
    } else{
      x_labels <- format.Date(x_labels, format)
    }
    # Replace every second label with blanks (TRUE = remove):
    drop <- rep(TRUE, length(x_labels))
    drop[seq(2 + skip*2, length(drop), major*2)] <- FALSE # start at 2 because that's where the first midpoint is
    x_labels[drop] <- ''
    if(length(x_labels) > length(ticks_double)){
      x_labels <- x_labels[-length(x_labels)] # If there are too many x_labels, then drop the last element
    }
    list(
      ggplot2::scale_y_date(labels = x_labels, breaks = ticks_double, limits = lim, 
                            expand = expansion(mult = expand.mult, add = expand.add), oob = oob, ...),
      theme(axis.ticks.y = element_line(colour = c(grey_01, NA))) # removes midpoint ticks from the theme element
    )
  } 
  else{
    if(FY){
      x_labels <- fy(ticks)
    } 
    else{
      x_labels <- format.Date(ticks, format)
    }
    drop <- rep(TRUE, length(x_labels))
    drop[seq(1 + skip, length(drop), major)] <- FALSE
    x_labels[drop] <- ''
    ggplot2::scale_y_date(labels = x_labels, breaks = ticks, limits = lim, 
                          expand = expansion(mult = expand.mult, add = expand.add), oob = oob, ...)
  }
}



#' A wrapper for scale_*_continuous() that makes axis labelling easier
#' 
#' Specify your desired range for the axis, and formatting of labels.
#' @param limits A vector of values (of length 2), specifying [1] the minimum and [2] the maximum for the axis.
#' @param by Distance between ticks/labels.
#' @param label.prefix A character string to prefix the label with (e.g. "$").
#' @param label.suffix A character string to suffix the label with (e.g. "\%").
#' @param limits.secondary,by.secondary,name.secondary Respective arguments on a secondary axis. If \code{by.secondary} not provided, will use the same magnitudes as the primary axis. Supply an axis title with \code{name.secondary}. Note: secondary axes are cosmetic only, so you will need to transform the variables you want on the axis beforehand (the function will calculate and return a warning with the coefficients you need for this).
#' @param expand.mult,expand.add Values passed to \code{\link[ggplot2]{expansion}()}. These can be used to expand the axis beyond the \code{limits} supplied, in one- or both- directions.
#' @param oob Function that handles limits outside of the scale limits (out of bounds). Defaults to \code{scales::\link[scales]{oob_keep}} to keep out of bounds data.
#' @param ... Other arguments passed on to scale continuous function.
#' @seealso \code{\link[ggplot2]{scale_x_continuous}()} and \code{\link[ggplot2]{scale_y_continuous}()}
#' @name scale_continuous_nsw
NULL

#' @rdname scale_continuous_nsw
#' @export
scale_y_continuous_nsw <- function(limits, by, limits.secondary = NULL, by.secondary = NULL, name.secondary = NULL, expand.mult = c(0,0), expand.add = c(0,0), oob = scales::oob_keep, labels = label_comma_nsw(), labels.secondary = label_comma_nsw(), ... ){
  lim <- limits
  lim.sec <- limits.secondary
  if (missing(by)){
    ticks <- waiver()
  } else{
    ticks <- seq(lim[1], lim[2], by = by)
  }
  
  # Secondary axis:
  if(! is.null(lim.sec)){
    if(is.null(by.secondary)) {
      # If no secondary increments are supplied, use same proportion as primary axis:
      # by.secondary <- (by / (lim[2] - lim[1])) * (lim.sec[2] - lim.sec[1])
      ticks2 <- waiver()
    } else{
      ticks2 <- seq(lim.sec[1], lim.sec[2], by = by.secondary)
    }
    b1 <- diff(lim)/diff(lim.sec)
    b0 <- lim[1] - b1 * lim.sec[1]
    trans <- ~ (. - b0) / b1
    warning(paste0('\nSecondary axis is cosmetic only. Make sure the following transformation has been made to the variables you want the secondary axis to apply to:\n    ', 
                   '* ', ifelse(b1 !=1, paste0("Multiply by ", b1), "No multiplication needed"), ' and;\n    ', 
                   '* ', ifelse(b0 !=0, paste0("Add ", b0), "No addition needed"), '\n'))
    scale_y_continuous(labels = labels, breaks = ticks, limits = lim, 
                       expand = expansion(mult = expand.mult, add = expand.add), oob = oob, 
                       sec.axis = sec_axis(trans = trans, labels = labels.secondary, breaks = ticks2, name = name.secondary), ...)
  } else{
    scale_y_continuous(labels = labels, breaks = ticks, limits = lim, 
                       expand = expansion(mult = expand.mult, add = expand.add), oob = oob, ...)
  }
}

#' @rdname scale_continuous_nsw
#' @export
scale_x_continuous_nsw <- function(limits, by, limits.secondary = NULL, by.secondary = NULL, name.secondary = NULL, expand.mult = c(0,0), expand.add = c(0,0), oob = scales::oob_keep, labels = label_comma_nsw(), labels.secondary = label_comma_nsw(), ... ){
  lim <- limits
  lim.sec <- limits.secondary
  if (missing(by)){
      ticks <- waiver()
    } else{
      ticks <- seq(lim[1], lim[2], by = by)
  }
  # Secondary axis:
  if(! is.null(lim.sec)){
    # If no secondary increments are supplied, use same proportion as primary axis:
    if(is.null(by.secondary)) {
      by.secondary <- (by / (lim[2] - lim[1])) * (lim.sec[2] - lim.sec[1])
    }
    ticks2 <- seq(lim.sec[1], lim.sec[2], by = by.secondary)
    b1 <- diff(lim)/diff(lim.sec)
    b0 <- lim[1] - b1 * lim.sec[1]
    trans <- ~ (. - b0) / b1
    warning(paste0('\nSecondary axis is cosmetic only. Make sure the following transformation has been made to the variables you want the secondary axis to apply to:\n    ', 
                   '* ', ifelse(b1 !=1, paste0("Multiply by ", b1), "No multiplication needed"), ' and;\n    ', 
                   '* ', ifelse(b0 !=0, paste0("Add ", b0), "No addition needed"), '\n'))
    scale_x_continuous(labels = labels, breaks = ticks, limits = lim, 
                       expand = expansion(mult = expand.mult, add = expand.add), oob = oob, 
                       sec.axis = sec_axis(trans = trans, labels = labels.secondary, breaks = ticks2, name = name.secondary), ...)
  } else{
    scale_x_continuous(labels = labels, breaks = ticks, limits = lim, 
                       expand = expansion(mult = expand.mult, add = expand.add), oob = oob, ...)
  }
}
