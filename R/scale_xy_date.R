#' Label functions with sensible defaults
#'
#' * `label_comma_nsw()` behaves like [scales::label_comma()], but defaults
#'   to `drop0trailing = TRUE`.
#' * `fy()` takes a vector of dates and returns financial years as a
#'   character vector.
#'
#' @param ... Arguments passed on to [scales::number()].
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
fy <- Vectorize(function(x, sep = en_dash) {
  
  if (lubridate::month(x) <= 6) {
    
    # If date is Jan-Jun, use the previous and current years.
    fy_label <- paste0(
      lubridate::year(x) - 1,
      sep,
      sprintf("%02d", lubridate::year(x) %% 100)
    )
    
  } else {
    
    # If date is Jul-Dec, use the current and following years.
    fy_label <- paste0(
      lubridate::year(x),
      sep,
      sprintf("%02d", (lubridate::year(x) + 1) %% 100)
    )
  }
  
  fy_label
})


# Convert date-scale limits to Date values.
#
# Numeric values of 3000 or below are interpreted as years. For calendar
# years, numeric limits span 1 January to 31 December. For financial years,
# they span 1 July of the preceding year to 30 June of the supplied year.
date_limits_nsw <- function(limits, FY = FALSE) {
  
  if (is.null(limits)) {
    return(NULL)
  }
  
  if (length(limits) != 2L) {
    stop(
      "`limits` must be `NULL` or a vector containing two values.",
      call. = FALSE
    )
  }
  
  start <- limits[[1]]
  end <- limits[[2]]
  
  start_is_year <-
    is.numeric(start) &&
    length(start) == 1L &&
    !is.na(start) &&
    start <= 3000
  
  end_is_year <-
    is.numeric(end) &&
    length(end) == 1L &&
    !is.na(end) &&
    end <= 3000
  
  if (start_is_year) {
    start <- if (FY) {
      lubridate::make_date(
        year = start - 1,
        month = 7,
        day = 1
      )
    } else {
      lubridate::make_date(
        year = start,
        month = 1,
        day = 1
      )
    }
  } else if (is.numeric(start)) {
    start <- as.Date(
      start,
      origin = lubridate::origin
    )
  } else {
    start <- as.Date(start)
  }
  
  if (end_is_year) {
    end <- if (FY) {
      lubridate::make_date(
        year = end,
        month = 6,
        day = 30
      )
    } else {
      lubridate::make_date(
        year = end,
        month = 12,
        day = 31
      )
    }
  } else if (is.numeric(end)) {
    end <- as.Date(
      end,
      origin = lubridate::origin
    )
  } else {
    end <- as.Date(end)
  }
  
  limits_out <- c(start, end)
  
  if (anyNA(limits_out)) {
    stop(
      "`limits` could not be converted to valid dates.",
      call. = FALSE
    )
  }
  
  if (limits_out[1] >= limits_out[2]) {
    stop(
      "`limits` must be in increasing order.",
      call. = FALSE
    )
  }
  
  limits_out
}

# Construct the break function used by the NSW date scales.
date_breaks_nsw <- function(
    by,
    limits = NULL,
    label_between_ticks = FALSE
) {
  
  # With explicit limits, anchor the break sequence at the lower limit.
  # Otherwise, let scales determine a sequence over the trained range.
  base_breaks <- if (is.null(limits)) {
    
    scales::breaks_width(by)
    
  } else {
    
    function(x) {
      seq.Date(
        from = limits[1],
        to = limits[2],
        by = by
      )
    }
  }
  
  function(x) {
    
    range <- as.Date(x)
    
    ticks <- base_breaks(range)
    ticks <- as.Date(ticks)
    
    # Remove invalid breaks and breaks outside the trained scale range.
    #
    # This is important for between-tick labels because scales::breaks_width()
    # may generate breaks immediately outside the requested range. If those
    # are left in place, ggplot2 may replace them with NA after the labels
    # function has assumed an alternating tick/midpoint sequence.
    ticks <- ticks[
      !is.na(ticks) &
        is.finite(as.numeric(ticks)) &
        ticks >= range[1] &
        ticks <= range[2]
    ]
    
    if (
      !label_between_ticks ||
      length(ticks) == 0L
    ) {
      return(ticks)
    }
    
    # Generate an actual additional tick after the final visible tick.
    # This gives the final visible tick a complete period over which its
    # label can be centred.
    next_tick <- seq.Date(
      from = tail(ticks, 1),
      by = by,
      length.out = 2L
    )[2]
    
    ticks_extended <- c(
      ticks,
      next_tick
    )
    
    intervals <- as.numeric(
      diff(ticks_extended),
      units = "days"
    )
    
    midpoints <- ticks + round(intervals / 2)
    
    # Keep only midpoint positions that fall within the scale range.
    midpoints <- midpoints[
      midpoints >= range[1] &
        midpoints <= range[2]
    ]
    
    # Preserve explicit tick/midpoint ordering rather than relying on sorting.
    #
    # There should normally be one midpoint for each tick. If the final
    # midpoint lies outside the range, the final tick remains unpaired.
    n_pairs <- min(
      length(ticks),
      length(midpoints)
    )
    
    breaks <- as.vector(
      rbind(
        ticks[seq_len(n_pairs)],
        midpoints[seq_len(n_pairs)]
      )
    )
    
    if (length(ticks) > n_pairs) {
      breaks <- c(
        breaks,
        ticks[(n_pairs + 1L):length(ticks)]
      )
    }
    
    as.Date(
      breaks,
      origin = "1970-01-01"
    )
  }
}


# Construct the label function used by the NSW date scales.
date_labels_nsw <- function(
    label_every = 1,
    label_skip = 0,
    label_between_ticks = FALSE,
    format = "%Y",
    FY = FALSE
) {
  function(x) {
    
    x <- as.Date(x)
    
    if (length(x) == 0L) {
      return(character())
    }
    
    missing <- is.na(x)
    
    if (label_between_ticks) {
      
      labels <- rep("", length(x))
      
      # This should normally be unnecessary because date_breaks_nsw() now
      # removes out-of-range breaks. Retain it defensively so an NA break can
      # never become the literal label "NA".
      valid_positions <- which(!missing)
      
      if (length(valid_positions) < 2L) {
        return(labels)
      }
      
      tick_positions <- valid_positions[
        seq.int(
          from = 1L,
          to = length(valid_positions),
          by = 2L
        )
      ]
      
      midpoint_positions <- valid_positions[
        seq.int(
          from = 2L,
          to = length(valid_positions),
          by = 2L
        )
      ]
      
      if (length(midpoint_positions) == 0L) {
        return(labels)
      }
      
      # Each midpoint is labelled using the preceding period-start tick.
      period_dates <- x[tick_positions]
      
      n_periods <- min(
        length(period_dates),
        length(midpoint_positions)
      )
      
      period_dates <- period_dates[seq_len(n_periods)]
      midpoint_positions <- midpoint_positions[seq_len(n_periods)]
      
      period_labels <- if (FY) {
        fy(period_dates)
      } else {
        format.Date(period_dates, format)
      }
      
      keep_periods <- seq.int(
        from = 1L + label_skip,
        to = n_periods,
        by = label_every
      )
      
      keep_periods <- keep_periods[
        keep_periods <= n_periods
      ]
      
      if (length(keep_periods) > 0L) {
        labels[midpoint_positions[keep_periods]] <-
          period_labels[keep_periods]
      }
      
      return(labels)
    }

    if (label_between_ticks) {
      
      labels <- rep("", length(x))
      
      # This should normally be unnecessary because date_breaks_nsw() now
      # removes out-of-range breaks. Retain it defensively so an NA break can
      # never become the literal label "NA".
      valid_positions <- which(!missing)
      
      if (length(valid_positions) < 2L) {
        return(labels)
      }
      
      tick_positions <- valid_positions[
        seq.int(
          from = 1L,
          to = length(valid_positions),
          by = 2L
        )
      ]
      
      midpoint_positions <- valid_positions[
        seq.int(
          from = 2L,
          to = length(valid_positions),
          by = 2L
        )
      ]
      
      if (length(midpoint_positions) == 0L) {
        return(labels)
      }
      
      # Each midpoint is labelled using the preceding period-start tick.
      period_dates <- x[tick_positions]
      
      n_periods <- min(
        length(period_dates),
        length(midpoint_positions)
      )
      
      period_dates <- period_dates[seq_len(n_periods)]
      midpoint_positions <- midpoint_positions[seq_len(n_periods)]
      
      period_labels <- if (FY) {
        fy(period_dates)
      } else {
        format.Date(period_dates, format)
      }
      
      keep_periods <- seq.int(
        from = 1L + label_skip,
        to = n_periods,
        by = label_every
      )
      
      keep_periods <- keep_periods[
        keep_periods <= n_periods
      ]
      
      if (length(keep_periods) > 0L) {
        labels[midpoint_positions[keep_periods]] <-
          period_labels[keep_periods]
      }
      
      return(labels)
    }
    
    labels <- rep("", length(x))
    
    valid_positions <- which(!missing)
    
    labels[valid_positions] <- if (FY) {
      fy(x[valid_positions])
    } else {
      format.Date(x[valid_positions], format)
    }
    
    keep <- valid_positions[
      seq.int(
        from = 1L + label_skip,
        to = length(valid_positions),
        by = label_every
      )
    ]
    
    labels[setdiff(valid_positions, keep)] <- ""
    
    labels
  }
}


# Internal implementation shared by the X and Y date scales.
scale_date_nsw <- function(
    axis,
    limits, by, 
    label_every, label_skip, label_between_ticks, 
    format, FY, expand_mult, expand_add, oob,
    ...
) {
  axis <- match.arg(axis, c("x", "y"))
  
  scale_fun <- switch(
    axis,
    x = ggplot2::scale_x_date,
    y = ggplot2::scale_y_date
  )
  
  if (
    length(by) != 1L ||
    is.na(by) ||
    !is.character(by) ||
    !nzchar(by)
  ) {
    stop(
      "`by` must be a single non-empty character string.",
      call. = FALSE
    )
  }
  
  if (
    length(label_every) != 1L ||
    is.na(label_every) ||
    label_every < 1 ||
    label_every != as.integer(label_every)
  ) {
    stop(
      "`label_every` must be a single positive integer.",
      call. = FALSE
    )
  }
  
  if (
    length(label_skip) != 1L ||
    is.na(label_skip) ||
    label_skip < 0 ||
    label_skip != as.integer(label_skip)
  ) {
    stop(
      "`label_skip` must be a single non-negative integer.",
      call. = FALSE
    )
  }
  
  if (
    length(label_between_ticks) != 1L ||
    is.na(label_between_ticks) ||
    !is.logical(label_between_ticks)
  ) {
    stop(
      "`label_between_ticks` must be `TRUE` or `FALSE`.",
      call. = FALSE
    )
  }
  
  if (
    length(FY) != 1L ||
    is.na(FY) ||
    !is.logical(FY)
  ) {
    stop(
      "`FY` must be `TRUE` or `FALSE`.",
      call. = FALSE
    )
  }
  
  lim <- date_limits_nsw(
    limits = limits,
    FY = FY
  )
  
  # Use explicit expansion when supplied by the user. Otherwise, explicit
  # limits use zero expansion and automatically trained limits retain
  # ggplot2's default expansion.
  if (!is.null(expand_mult) || !is.null(expand_add)) {
    expand <- ggplot2::expansion(
      mult = if (is.null(expand_mult)) 0 else expand_mult,
      add = if (is.null(expand_add)) 0 else expand_add
    )
  } else if (!is.null(lim)) {
    expand <- ggplot2::expansion(
      mult = 0,
      add = 0
    )
  } else {
    expand <- ggplot2::waiver()
  }
  
  breaks <- date_breaks_nsw(
    by = by,
    limits = lim,
    label_between_ticks = label_between_ticks
  )
  
  labels <- date_labels_nsw(
    label_every = label_every,
    label_skip = label_skip,
    label_between_ticks = label_between_ticks,
    format = format,
    FY = FY
  )
  
  dots <- list(...)
  
  if (label_between_ticks) {
    
    if (!is.null(dots$guide)) {
      warning(
        paste0(
          "`guide` is ignored when `label_between_ticks = TRUE`, ",
          "because `guide_axis_between()` is required to suppress ",
          "the midpoint ticks."
        ),
        call. = FALSE
      )
    }
    
    dots$guide <- guide_axis_between()
  }
  
  date_scale <- do.call(scale_fun,
                        c(list(breaks = breaks, labels = labels, limits = lim, expand = expand, oob = oob), 
                          dots)
  )
  
  date_scale
}


#' Date scales with convenient axis labelling
#'
#' Wrappers for [ggplot2::scale_x_date()] and [ggplot2::scale_y_date()]
#' that make it easier to specify date ranges, breaks and labels, including
#' financial-year labels and labels positioned between period-start ticks.
#'
#' @param limits A vector of two dates specifying the minimum and maximum
#'   dates for the axis. Numeric year values are also accepted. Calendar-year
#'   limits are converted to 1 January and 31 December, while financial-year
#'   limits are converted to 1 July and 30 June. If `NULL`, the limits are
#'   determined automatically by ggplot2.
#' @param by A string giving the distance between breaks or ticks, such as
#'   `"2 weeks"`, `"3 months"` or `"1 year"`. Defaults to `"1 year"`.
#' @param label_every Number of periods between displayed labels. Defaults to
#'   `2`, so every second period is labelled.
#' @param label_skip Number of periods to skip before the first displayed
#'   label. Defaults to `0`.
#' @param label_between_ticks If `TRUE`, labels are centred over the periods
#'   beginning at each tick rather than being positioned directly at ticks.
#' @param format Character string specifying the date format, passed to
#'   [base::format.Date()]. Ignored when `FY = TRUE`. For example, for
#'   `2020-03-01`:
#'
#'   * `format = "%Y"` gives `"2020"`;
#'   * `format = "%y"` gives `"20"`;
#'   * `format = "%B"` gives `"March"`; and
#'   * `format = "%d-%b"` gives `"01-Mar"`.
#' @param FY If `TRUE`, display labels as financial years.
#' @param expand_mult,expand_add Values passed to [ggplot2::expansion()].
#'   If neither is supplied, explicit `limits` use zero expansion, while an
#'   automatically determined axis uses ggplot2's default expansion. These
#'   arguments can be supplied to override that behaviour.
#' @param oob Function used to handle values outside the scale limits.
#'   Defaults to [scales::oob_keep()], which retains out-of-bounds data.
#' @param ... Other arguments passed on to the corresponding ggplot2 date
#'   scale.
#'
#' @details
#' If `limits` are supplied, they define the exact axis range and no expansion
#' is added by default. If `limits` are `NULL`, the range and expansion are
#' determined automatically by ggplot2.
#'
#' When `label_between_ticks = TRUE`, each ordinary tick is treated as the
#' beginning of a period. Its label is placed halfway between that tick and
#' the following tick. An additional break is generated internally after the
#' final tick so that the final visible period can also be labelled.
#'
#' `label_every`, `label_skip`, and `label_between_ticks` can be used with or
#' without explicit `limits`.
#'
#' @seealso [ggplot2::scale_x_date()], [ggplot2::scale_y_date()]
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(economics, aes(date, unemploy)) +
#'   geom_line()
#'
#' # Let ggplot2 determine the range and expansion
#' p + scale_x_date_nsw()
#'
#' # Automatic range with breaks every five years
#' p + scale_x_date_nsw(by = "5 years")
#'
#' # Automatic range with annual ticks and labels every five years
#' p + scale_x_date_nsw(
#'   by = "1 year",
#'   label_every = 5
#' )
#'
#' # Labels centred over annual periods without explicit limits
#' p + scale_x_date_nsw(
#'   by = "1 year",
#'   label_every = 5,
#'   label_between_ticks = TRUE
#' )
#'
#' # Exact calendar-year limits with no expansion
#' p + scale_x_date_nsw(
#'   limits = c(2000, 2015),
#'   by = "1 year",
#'   label_every = 2
#' )
#'
#' # Exact limits with labels centred over each two-year-labelled period
#' p + scale_x_date_nsw(
#'   limits = c(2000, 2015),
#'   by = "1 year",
#'   label_every = 2,
#'   label_between_ticks = TRUE
#' )
#'
#' @name scale_date_nsw
NULL


#' @rdname scale_date_nsw
#' @export
scale_x_date_nsw <- function(
    limits = NULL,
    by = "1 year",
    label_every = 2,
    label_skip = 0,
    label_between_ticks = FALSE,
    format = "%Y",
    FY = FALSE,
    expand_mult = NULL,
    expand_add = NULL,
    oob = scales::oob_keep,
    ...
) {
  scale_date_nsw(
    axis = "x",
    limits = limits,
    by = by,
    label_every = label_every,
    label_skip = label_skip,
    label_between_ticks = label_between_ticks,
    format = format,
    FY = FY,
    expand_mult = expand_mult,
    expand_add = expand_add,
    oob = oob,
    ...
  )
}


#' @rdname scale_date_nsw
#' @export
scale_y_date_nsw <- function(
    limits = NULL,
    by = "1 year",
    label_every = 2,
    label_skip = 0,
    label_between_ticks = FALSE,
    format = "%Y",
    FY = FALSE,
    expand_mult = NULL,
    expand_add = NULL,
    oob = scales::oob_keep,
    ...
) {
  scale_date_nsw(
    axis = "y",
    limits = limits,
    by = by,
    label_every = label_every,
    label_skip = label_skip,
    label_between_ticks = label_between_ticks,
    format = format,
    FY = FY,
    expand_mult = expand_mult,
    expand_add = expand_add,
    oob = oob,
    ...
  )
}