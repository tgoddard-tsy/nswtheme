#' @rdname label_nsw
#' @export
label_comma_nsw <- function(accuracy = NULL, scale = 1, prefix = "", suffix = "", 
                            big.mark = ",", decimal.mark = ".", trim = TRUE, drop0trailing = TRUE, ...) {
  function(x) scales::number(x, accuracy = accuracy, scale = scale, 
                             prefix = prefix, suffix = suffix, big.mark = big.mark, 
                             decimal.mark = decimal.mark, trim = trim, drop0trailing = drop0trailing, ...)
}

#' Continuous scales with convenient axis labelling
#'
#' Wrappers for [ggplot2::scale_x_continuous()] and
#' [ggplot2::scale_y_continuous()] that make it easier to specify axis
#' ranges, breaks, labels and secondary axes.
#'
#' @param limits A numeric vector of two values specifying the minimum and
#'   maximum values for the axis. If `NULL`, the limits are determined
#'   automatically by ggplot2. When limits are supplied, the axis has no
#'   expansion by default.
#' @param by Distance between axis breaks. If `NULL`, breaks are determined
#'   automatically by ggplot2. When supplied without `limits`, breaks are
#'   generated at this interval over the automatically determined axis range.
#' @param labels Label function for the primary axis. Defaults to
#'   [label_comma_nsw()].
#' @param limits_secondary,by_secondary,labels_secondary Corresponding
#'   arguments for the secondary axis. `limits_secondary` requires `limits`
#'   to be supplied. If `by_secondary` is `NULL`, secondary-axis breaks are
#'   determined automatically by ggplot2. `labels_secondary` defaults to
#'   [label_comma_nsw()].
#' @param name_secondary Title for the secondary axis.
#' @param expand_mult,expand_add Values passed to [ggplot2::expansion()].
#'   If neither is supplied, explicit `limits` use zero expansion, while an
#'   automatically determined axis uses ggplot2's default expansion. These
#'   arguments can be supplied to override that behaviour.
#' @param oob Function used to handle values outside the scale limits.
#'   Defaults to [scales::oob_keep()], which retains out-of-bounds data.
#' @param ... Other arguments passed on to the corresponding ggplot2
#'   continuous scale.
#'
#' @details
#' If `limits` are supplied, they define the exact axis range and no expansion
#' is added by default. If `limits` are `NULL`, the range and expansion are
#' determined automatically by ggplot2.
#'
#' `by` controls the spacing between breaks without requiring `limits` to be
#' specified.
#'
#' Secondary axes are cosmetic only. Variables displayed against a secondary
#' axis must be transformed to the primary-axis scale beforehand.
#' 
#' @seealso [ggplot2::scale_x_continuous()],
#'   [ggplot2::scale_y_continuous()]
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Let ggplot2 determine the range, breaks and expansion
#' p + scale_y_continuous_nsw()
#'
#' # Automatic range with breaks every 10 units
#' p + scale_y_continuous_nsw(by = 10)
#'
#' # Exact limits with no expansion
#' p + scale_y_continuous_nsw(limits = c(0, 50))
#'
#' # Exact limits and breaks
#' p + scale_y_continuous_nsw(
#'   limits = c(0, 50),
#'   by = 10
#' )
#' 
#' @name scale_continuous_nsw
NULL

#' @rdname scale_continuous_nsw
#' @export
scale_y_continuous_nsw <- function(
    limits = NULL, by = NULL, labels = label_comma_nsw(),
    limits_secondary = NULL, by_secondary = NULL, name_secondary = NULL, labels_secondary = label_comma_nsw(),
    expand_mult = NULL, expand_add = NULL,
    oob = scales::oob_keep,
    ...
) {
  
  # Primary-axis breaks
  if (is.null(by)) {
    # by not specified:
    ticks <- ggplot2::waiver()
  } else if (is.null(limits)) { 
    # Limits not specified:
    ticks <- scales::breaks_width(by)
  } else { 
    # Limits specified:
    ticks <- seq(limits[1], limits[2], by = by)
  }
  
  # Use zero expansion when explicit limits are supplied.
  # Otherwise leave expansion to ggplot2, unless the user overrides it.
  if (!is.null(expand_mult) || !is.null(expand_add)) {
    expand <- ggplot2::expansion(
      mult = if (is.null(expand_mult)) 0 else expand_mult,
      add = if (is.null(expand_add)) 0 else expand_add
    )
  } else if (!is.null(limits)) {
    expand <- ggplot2::expansion(mult = 0, add = 0)
  } else {
    expand <- ggplot2::waiver()
  }
  
  # No secondary axis
  if (is.null(limits_secondary)) {
    return(
      ggplot2::scale_y_continuous(
        labels = labels,
        breaks = ticks,
        limits = limits,
        expand = expand,
        oob = oob,
        ...
      )
    )
  }
  
  # A secondary axis needs explicit primary limits to define
  # the transformation between the two scales.
  if (is.null(limits)) {
    stop(
      "`limits` must be supplied when `limits_secondary` is used.",
      call. = FALSE
    )
  }
  
  # Secondary-axis breaks
  if (is.null(by_secondary)) {
    ticks_secondary <- ggplot2::waiver()
  } else {
    ticks_secondary <- seq(
      limits_secondary[1],
      limits_secondary[2],
      by = by_secondary
    )
  }
  
  # Transformation between primary and secondary scales
  b1 <- diff(limits) / diff(limits_secondary)
  b0 <- limits[1] - b1 * limits_secondary[1]
  transform <- ~ (. - b0) / b1
  
  warning(
    paste0(
      "\nSecondary axis is cosmetic only. Make sure the following ",
      "transformation has been made to the variables you want the ",
      "secondary axis to apply to:\n    ",
      "* ", ifelse(
        b1 != 1,
        paste0("Multiply by ", b1),
        "No multiplication needed"
      ),
      " and;\n    ",
      "* ", ifelse(
        b0 != 0,
        paste0("Add ", b0),
        "No addition needed"
      ),
      "\n"
    )
  )
  
  ggplot2::scale_y_continuous(
    labels = labels,
    breaks = ticks,
    limits = limits,
    expand = expand,
    oob = oob,
    sec.axis = ggplot2::sec_axis(
      transform = transform,
      labels = labels_secondary,
      breaks = ticks_secondary,
      name = name_secondary
    ),
    ...
  )
}

#' @rdname scale_continuous_nsw
#' @export
scale_x_continuous_nsw <- function(
    limits = NULL, by = NULL, labels = label_comma_nsw(),
    limits_secondary = NULL, by_secondary = NULL, name_secondary = NULL, labels_secondary = label_comma_nsw(),
    expand_mult = NULL, expand_add = NULL,
    oob = scales::oob_keep,
    ...
) {
  
  # Primary-axis breaks
  if (is.null(by)) {
    # by not specified:
    ticks <- ggplot2::waiver()
  } else if (is.null(limits)) { 
    # Limits not specified:
    ticks <- scales::breaks_width(by)
  } else { 
    # Limits specified:
    ticks <- seq(limits[1], limits[2], by = by)
  }
  
  # Use zero expansion when explicit limits are supplied.
  # Otherwise leave expansion to ggplot2, unless the user overrides it.
  if (!is.null(expand_mult) || !is.null(expand_add)) {
    expand <- ggplot2::expansion(
      mult = if (is.null(expand_mult)) 0 else expand_mult,
      add = if (is.null(expand_add)) 0 else expand_add
    )
  } else if (!is.null(limits)) {
    expand <- ggplot2::expansion(mult = 0, add = 0)
  } else {
    expand <- ggplot2::waiver()
  }
  
  # No secondary axis
  if (is.null(limits_secondary)) {
    return(
      ggplot2::scale_x_continuous(
        labels = labels,
        breaks = ticks,
        limits = limits,
        expand = expand,
        oob = oob,
        ...
      )
    )
  }
  
  # A secondary axis needs explicit primary limits to define
  # the transformation between the two scales.
  if (is.null(limits)) {
    stop(
      "`limits` must be supplied when `limits_secondary` is used.",
      call. = FALSE
    )
  }
  
  # Secondary-axis breaks
  if (is.null(by_secondary)) {
    ticks_secondary <- ggplot2::waiver()
  } else {
    ticks_secondary <- seq(
      limits_secondary[1],
      limits_secondary[2],
      by = by_secondary
    )
  }
  
  # Transformation between primary and secondary scales
  b1 <- diff(limits) / diff(limits_secondary)
  b0 <- limits[1] - b1 * limits_secondary[1]
  transform <- ~ (. - b0) / b1
  
  warning(
    paste0(
      "\nSecondary axis is cosmetic only. Make sure the following ",
      "transformation has been made to the variables you want the ",
      "secondary axis to apply to:\n    ",
      "* ", ifelse(
        b1 != 1,
        paste0("Multiply by ", b1),
        "No multiplication needed"
      ),
      " and;\n    ",
      "* ", ifelse(
        b0 != 0,
        paste0("Add ", b0),
        "No addition needed"
      ),
      "\n"
    )
  )
  
  ggplot2::scale_x_continuous(
    labels = labels,
    breaks = ticks,
    limits = limits,
    expand = expand,
    oob = oob,
    sec.axis = ggplot2::sec_axis(
      transform = transform,
      labels = labels_secondary,
      breaks = ticks_secondary,
      name = name_secondary
    ),
    ...
  )
}
