#' NSW Government colour values
#'
#' Individual colours from the NSW Government colour palette, provided as
#' character constants containing hexadecimal colour values.
#'
#' Colour names combine a colour family with a two-digit tonal row.
#' For example, `blue_01`, `blue_02`, `blue_03` and `blue_04` correspond
#' to the four Blue colours in the NSW Government palette.
#'
#' @format Each colour is a character string containing a hexadecimal
#'   colour value.
#'
#' @section Illustration:
#' \if{html}{\figure{nswgov_palette.png}{
#'   options: width=750 alt="NSW Government palette"
#' }}
#'
#' @examples
#' blue_01
#' red_02
#'
#' # Use individual colours directly
#' ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
#'   ggplot2::geom_point(colour = blue_02)
#'
#' # Specify individual colours for a scale
#' ggplot2::ggplot(
#'   ggplot2::mpg,
#'   ggplot2::aes(displ, hwy, colour = drv)
#' ) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_colour_manual(
#'     values = c(fuchsia_01, red_02, purple_03)
#'   )
#'
#' @references
#' Masterbrand quick reference guide:
#' \url{https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf}
#'
#' @seealso
#' [nswgov_palette] for the complete palette as a named vector.
#'
#' [nswgov_palette_df] for the palette with additional metadata.
#'
#' [nswgov_pal()] for selecting colours from the palette.
#'
#' [scale_colour_nsw()] and [scale_fill_nsw()] for applying the palette
#' to ggplot2 scales.
#'
#' @name nswgov_colours
NULL

#' @rdname nswgov_colours
#' @export
grey_01 <- "#22272b"
#' @rdname nswgov_colours
#' @export
grey_02 <- "#495054"
#' @rdname nswgov_colours
#' @export
grey_03 <- "#cdd3d6"
#' @rdname nswgov_colours
#' @export
grey_04 <- "#ebebeb"

#' @rdname nswgov_colours
#' @export
green_01 <- "#004000"
#' @rdname nswgov_colours
#' @export
green_02 <- "#00aa45"
#' @rdname nswgov_colours
#' @export
green_03 <- "#a8edb3"
#' @rdname nswgov_colours
#' @export
green_04 <- "#dbfadf"

#' @rdname nswgov_colours
#' @export
teal_01 <- "#0b3f47"
#' @rdname nswgov_colours
#' @export
teal_02 <- "#2e808e"
#' @rdname nswgov_colours
#' @export
teal_03 <- "#8cdbe5"
#' @rdname nswgov_colours
#' @export
teal_04 <- "#d1eeea"

#' @rdname nswgov_colours
#' @export
blue_01 <- "#002664"
#' @rdname nswgov_colours
#' @export
blue_02 <- "#146cfd"
#' @rdname nswgov_colours
#' @export
blue_03 <- "#8ce0ff"
#' @rdname nswgov_colours
#' @export
blue_04 <- "#cbedfd"

#' @rdname nswgov_colours
#' @export
purple_01 <- "#441170"
#' @rdname nswgov_colours
#' @export
purple_02 <- "#8055f1"
#' @rdname nswgov_colours
#' @export
purple_03 <- "#cebfff"
#' @rdname nswgov_colours
#' @export
purple_04 <- "#e6e1fd"

#' @rdname nswgov_colours
#' @export
fuchsia_01 <- "#65004d"
#' @rdname nswgov_colours
#' @export
fuchsia_02 <- "#d912ae"
#' @rdname nswgov_colours
#' @export
fuchsia_03 <- "#f4b5e6"
#' @rdname nswgov_colours
#' @export
fuchsia_04 <- "#fddef2"

#' @rdname nswgov_colours
#' @export
red_01 <- "#630019"
#' @rdname nswgov_colours
#' @export
red_02 <- "#d7153a"
#' @rdname nswgov_colours
#' @export
red_03 <- "#ffb8c1"
#' @rdname nswgov_colours
#' @export
red_04 <- "#ffe6ea"

#' @rdname nswgov_colours
#' @export
orange_01 <- "#941b00"
#' @rdname nswgov_colours
#' @export
orange_02 <- "#f3631b"
#' @rdname nswgov_colours
#' @export
orange_03 <- "#ffce99"
#' @rdname nswgov_colours
#' @export
orange_04 <- "#fdeddf"

#' @rdname nswgov_colours
#' @export
yellow_01 <- "#694800"
#' @rdname nswgov_colours
#' @export
yellow_02 <- "#faaf05"
#' @rdname nswgov_colours
#' @export
yellow_03 <- "#fde79a"
#' @rdname nswgov_colours
#' @export
yellow_04 <- "#fff4cf"

#' @rdname nswgov_colours
#' @export
brown_01 <- "#523719"
#' @rdname nswgov_colours
#' @export
brown_02 <- "#b68d5d"
#' @rdname nswgov_colours
#' @export
brown_03 <- "#e8d0b5"
#' @rdname nswgov_colours
#' @export
brown_04 <- "#ede3d7"


#' NSW Government colour palette function
#' 
#' A palette function for selecting colours from the  \emph{NSW Government} palette. \code{nswgov_pal()} returns a function that generates the requested number of colours.
#' @param colour_cols Character vector specifying the "colour columns" to use from the palette. For example, \code{c("Blue", "Green", "Red")}.
#' @param tonal_rows Numeric or character vector specifying the "tonal rows" to use from the palette. Values must correspond to tonal rows 1 to 4;
#'   for example, `1:3` or `c("01", "02", "03")`. If `NULL`, tonal rows are selected automatically based on the number of values needed.
#' @param hierarchy Character vector specifying how to sort/cycle through the colour palette. Defaults to \code{c("colour_col", "tonal_row")} which will sort by \code{colour_col} first and then \code{tonal_row} - e.g. Blue 01, ..., Blue 03, Grey 01, ... Grey 03, etc.
#' @param colours Character vector specifying the colour values to use, e.g. c(green_03, red_02, teal_01). If used, this will override the \code{colour_cols} and \code{tonal_rows} algorithm.
#' @param reverse Logical. If \code{TRUE}, reverses the colours in the selected palette. \code{FALSE} by default.
#' @param pattern Logical. Whether to use the tonal-row defaults for pattern fills. Used by `scale_pattern_fill_nsw()`. 
#' 
#' @section Automatic tonal-row selection:
#' If `tonal_rows = NULL`, the palette automatically selects tonal rows according to the number of colours required. 
#' This is intended to provide a suitable default while allowing tonal-row selection to be overridden explicitly when required.
#' 
#' @section Palette selection:
#' Colours can be selected by specifying `colour_cols` and `tonal_rows`.
#' Alternatively, `colours` can be used to supply colour values directly,
#' overriding colour-column and tonal-row selection.
#'
#' If no colour selection is supplied, palette defaults set by `set_defaults_nsw()` are used where available.
#' 
#' @returns A palette function that takes the number of colours required, `n`, and returns a character vector of colour values.
#' 
#' @section Illustration:
#' \if{html}{\figure{nswgov_palette.png}{options: width=750 alt="NSW Government palette"}}
#' 
#' @references Masterbrand quick reference guide: \url{https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf}
#' 
#' @examples
#' # E.g. Six colours using the default palette
#' nswgov_pal()(6)
#'
#' # Select particular colour columns and tonal rows
#' nswgov_pal(
#'   colour_cols = c("Blue", "Yellow"),
#'   tonal_rows = 1:3
#' )(6)
#'
#' # Visualise the resulting palette
#' scales::show_col(
#'   nswgov_pal(
#'     colour_cols = c("Blue", "Yellow"),
#'     tonal_rows = 1:3
#'   )(6)
#' )
#' 
#' @seealso
#' `scale_colour_nsw()` and `scale_fill_nsw()` for applying the palette to
#' ggplot2 scales.
#'
#' `nswgov_palette_df` for the underlying NSW Government palette data.
#' 
#' @name nswgov_pal
#' @export
NULL

#' @rdname nswgov_pal
#' @export
nswgov_pal <- function(
    colour_cols = NULL,
    tonal_rows = NULL,
    hierarchy = c("colour_col", "tonal_row"),
    colours = NULL,
    reverse = FALSE,
    pattern = FALSE
) {
  
  function(n) {
    
    defaults <- getOption("nswtheme.defaults", list())
    
    # 1. Exact colours supplied directly
    if (!is.null(colours)) {
      colours_out <- colours
      
      # 2. Exact palette supplied by set_defaults_nsw()
    } else if (is.null(colour_cols) && !is.null(defaults$colours)) {
      colours_out <- defaults$colours
      
      # 3. Build a palette from colour columns and tonal rows
    } else {
      
      # Use preset or standard colour columns
      if (is.null(colour_cols)) {
        colour_cols <- defaults$colour_cols
        
        if (is.null(colour_cols)) {
          colour_cols <- c(
            "Grey", "Blue", "Red", "Green", "Teal",
            "Purple", "Fuchsia", "Orange", "Yellow", "Brown"
          )
        }
      }
      
      # Select default tonal rows if none were supplied
      if (is.null(tonal_rows)) {
        if (pattern) {
          tonal_rows <- if (n %in% c(1L, 2L, 4L)) {
            c(4, 1)
          } else {
            c(3, 4, 1)
          }
        } else {
          tonal_rows <- if (n %in% c(1L, 2L, 4L)) {
            2:3
          } else {
            1:3
          }
        }
      }
      
      # Convert numeric or character tonal rows to integers
      tonal_rows_int <- suppressWarnings(as.integer(tonal_rows))
      
      # Validate tonal rows
      if (anyNA(tonal_rows_int) || any(!tonal_rows_int %in% 1:4)) {
        stop(
          "`tonal_rows` must contain values from 1 to 4.",
          call. = FALSE
        )
      }
      
      # Match formatting used in nswgov_palette_df
      colour_cols_formatted <- tools::toTitleCase(colour_cols)
      tonal_rows_formatted <- sprintf("%02d", tonal_rows_int)
      
      # Filter palette
      keep <-
        nswgov_palette_df$colour_col %in% colour_cols_formatted &
        nswgov_palette_df$tonal_row %in% tonal_rows_formatted
      
      palette <- nswgov_palette_df[keep, , drop = FALSE]
      
      # Preserve requested ordering
      palette$colour_col <- factor(
        palette$colour_col,
        levels = colour_cols_formatted
      )
      
      palette$tonal_row <- factor(
        palette$tonal_row,
        levels = tonal_rows_formatted
      )
      
      palette <- palette[
        do.call(base::order, palette[hierarchy]),
        ,
        drop = FALSE
      ]
      
      colours_out <- stats::setNames(
        palette$value,
        palette$name
      )
      
      colours_out <- head(colours_out, n)
    }
    
    if (n > length(colours_out)) {
      warning(
        "Your selection from the NSW Gov palette provides ",
        length(colours_out),
        " colours, but ",
        n,
        " are required."
      )
    }
    
    if (reverse) {
      colours_out <- rev(colours_out)
    }
    
    unname(colours_out[seq_len(n)])
  }
}

#' @rdname nswgov_pal
#' @export
nswgov_ramp_pal <- function(colours = NULL, na.colour = grey_04, reverse = FALSE, alpha = TRUE) {

  # If colour_cols not specified by user, try preset defaults:
  if (is.null(colours)) {
    
    defaults <- getOption("nswtheme.defaults")
    
    if (!is.null(defaults$ramp_colours)) {
      colours <- defaults$ramp_colours
    } else {
      colours <- c(red_03,blue_01)
    }
  }
  
  if (length(colours) < 2) {
    stop("`colours` must contain at least two colours.", call. = FALSE)
  }
  
  if (reverse) colours <- rev(colours)
  
  pal <- scales::colour_ramp(colours, na.color = na.colour, alpha = alpha)
  
  function(n) {
    pal(seq(0, 1, length.out = n))
  }
}


#' NSW Government colour scales
#'
#' Colour scales using values from the \emph{NSW Government} palette. 
#' The \emph{NSW Government} palette is organised into colour columns and tonal rows. 
#' colour_cols selects the colour families, while tonal_rows selects the shades within those families.
#' @section Types of colour scales:
#' There are three types of colour and fill scales provided:
#' \itemize{
#'   \item{\code{scale_colour_nsw()} and \code{scale_fill_nsw()} use discrete colours from the NSW Government palette.}
#'   \item{\code{scale_colour_nsw_ramp()} and \code{scale_fill_nsw_ramp()} interpolate between colours to create a gradient-like colour palette that can be applied to discrete data.}
#'   \item{\code{scale_colour_nsw_gradient()} and \code{scale_fill_nsw_gradient()} provide continuous colour scales.}
#' }
#' @section Illustration:
#' \if{html}{\figure{nswgov_palette.png}{options: width=750 alt="NSW Government palette"}}
#' @inheritParams nswgov_pal
#' @param binned Logical. For continuous scales (gradient), deciding whether to bin continuous data into groups. \code{FALSE} by default.
#' @param ... Arguments passed on to \code{\link[ggplot2]{discrete_scale}()}, \code{\link[ggplot2]{scale_colour_gradientn}()} or \code{\link[ggplot2]{scale_fill_gradientn}()}.
#' @seealso
#' `nswgov_pal()` for details of palette selection and automatic tonal-row selection.
#' @examples
#' library(ggplot2)
#' 
#' p <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
#'   geom_point()
#' 
#' # Automatic tonal-row selection
#' p + scale_colour_nsw()
#' 
#' # Use tonal rows 01 to 03
#' p + scale_colour_nsw(tonal_rows = 1:3)
#' 
#' # Select colour columns and tonal rows
#' p + scale_colour_nsw(
#'   colour_cols = c("Blue", "Purple"),
#'   tonal_rows = 1:3
#' )
#' 
#' # Cycle through colour columns before moving to the next tonal row
#' p + scale_colour_nsw(
#'   colour_cols = c("Blue", "Purple"),
#'   tonal_rows = 1:3,
#'   hierarchy = c("tonal_row", "colour_col")
#' )
#' @references Masterbrand quick reference guide: \url{https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf}
#' @name scale_colour
NULL

# Discrete scales:
#' @rdname scale_colour
#' @export
scale_colour_nsw <- function (colour_cols = NULL, tonal_rows = NULL, hierarchy = c("colour_col", "tonal_row"), colours = NULL, reverse = FALSE, ...){
  ggplot2::discrete_scale(aesthetics = "colour", palette = nswgov_pal(colour_cols, tonal_rows, hierarchy, colours, reverse), ...)
}

#' @rdname scale_colour
#' @export
scale_fill_nsw <- function (colour_cols = NULL, tonal_rows = NULL, hierarchy = c("colour_col", "tonal_row"), colours = NULL, reverse = FALSE, ...){
  ggplot2::discrete_scale(aesthetics = "fill", palette = nswgov_pal(colour_cols, tonal_rows, hierarchy, colours, reverse), ...)
}

#' @rdname scale_colour
#' @export
scale_colour_nsw_ramp <- function (colours = NULL, na.colour = grey_04, reverse = FALSE, ...) {
  ggplot2::discrete_scale(aesthetics = "colour", palette = nswgov_ramp_pal(colours, na.colour, reverse), ...)
}
#' @rdname scale_colour
#' @export
scale_fill_nsw_ramp <- function (colours = NULL, na.colour = grey_04, reverse = FALSE, ...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = nswgov_ramp_pal(colours, na.colour, reverse), ...)
}


# Continuous scales:
#' @rdname scale_colour
#' @export
scale_colour_nsw_gradient <- function (colours = NULL, na.value = grey_04, binned = FALSE, reverse = FALSE, ...){
  
  if (is.null(colours)) {
    defaults <- getOption("nswtheme.defaults")
    
    if (!is.null(defaults$gradient_colours)) {
      colours <- defaults$gradient_colours
    } else {
      colours <- c(blue_03, blue_01)
    }
  }
  
  if (reverse) {colours <- rev(colours)}
  
  if (binned) {
    ggplot2::scale_colour_stepsn(colours = colours, na.value = na.value, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = colours, na.value = na.value, ...)
  }
}

#' @rdname scale_colour
#' @export
scale_fill_nsw_gradient <- function (colours = NULL, na.value = grey_04, binned = FALSE, reverse = FALSE, ...){
  
  if (is.null(colours)) {
    defaults <- getOption("nswtheme.defaults")
    
    if (!is.null(defaults$gradient_colours)) {
      colours <- defaults$gradient_colours
    } else {
      colours <- c(blue_03, blue_01)
    }
  }
  
  if (reverse) {colours <- rev(colours)}
  
  if (binned) {
    ggplot2::scale_fill_stepsn(colours = colours, na.value = na.value, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = colours, na.value = na.value, ...)
  }
}


# Pattern scales:
#' @rdname scale_colour
#' @export
scale_pattern_fill_nsw <- function (colour_cols = NULL, tonal_rows = NULL, hierarchy = c("colour_col", "tonal_row"), colours = NULL, reverse = FALSE, patterns = NULL, ...){
  if (is.null(patterns)) {
    patterns <- c("none", "stripe", "circle", "crosshatch", "wave")
  }
  
  list(
      ggplot2::discrete_scale(aesthetics = "pattern_fill", palette = nswgov_pal(colour_cols, tonal_rows, hierarchy, reverse, pattern = TRUE), ...),
      ggpattern::scale_pattern_manual(values = patterns)
    )
}

