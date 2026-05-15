#' NSW Government colours
#' 
#' A vector of character strings containing HEX codes of all 10 colours in the \emph{NSW Treasury} palette.
#' @seealso 
#' \code{\link{nswgov_palette_simple}}
#' @name nsw_colour
NULL

#' @rdname nsw_colour
#' @export
grey_01 <- "#22272b"
#' @rdname nsw_colour
#' @export
grey_02 <- "#495054"
#' @rdname nsw_colour
#' @export
grey_03 <- "#cdd3d6"
#' @rdname nsw_colour
#' @export
grey_04 <- "#ebebeb"

#' @rdname nsw_colour
#' @export
green_01 <- "#004000"
#' @rdname nsw_colour
#' @export
green_02 <- "#00aa45"
#' @rdname nsw_colour
#' @export
green_03 <- "#a8edb3"
#' @rdname nsw_colour
#' @export
green_04 <- "#dbfadf"

#' @rdname nsw_colour
#' @export
teal_01 <- "#0b3f47"
#' @rdname nsw_colour
#' @export
teal_02 <- "#2e808e"
#' @rdname nsw_colour
#' @export
teal_03 <- "#8cdbe5"
#' @rdname nsw_colour
#' @export
teal_04 <- "#d1eeea"

#' @rdname nsw_colour
#' @export
blue_01 <- "#002664"
#' @rdname nsw_colour
#' @export
blue_02 <- "#146cfd"
#' @rdname nsw_colour
#' @export
blue_03 <- "#8ce0ff"
#' @rdname nsw_colour
#' @export
blue_04 <- "#cbedfd"

#' @rdname nsw_colour
#' @export
purple_01 <- "#441170"
#' @rdname nsw_colour
#' @export
purple_02 <- "#8055f1"
#' @rdname nsw_colour
#' @export
purple_03 <- "#cebfff"
#' @rdname nsw_colour
#' @export
purple_04 <- "#e6e1fd"

#' @rdname nsw_colour
#' @export
fuchsia_01 <- "#65004d"
#' @rdname nsw_colour
#' @export
fuchsia_02 <- "#d912ae"
#' @rdname nsw_colour
#' @export
fuchsia_03 <- "#f4b5e6"
#' @rdname nsw_colour
#' @export
fuchsia_04 <- "#fddef2"

#' @rdname nsw_colour
#' @export
red_01 <- "#630019"
#' @rdname nsw_colour
#' @export
red_02 <- "#d7153a"
#' @rdname nsw_colour
#' @export
red_03 <- "#ffb8c1"
#' @rdname nsw_colour
#' @export
red_04 <- "#ffe6ea"

#' @rdname nsw_colour
#' @export
orange_01 <- "#941b00"
#' @rdname nsw_colour
#' @export
orange_02 <- "#f3631b"
#' @rdname nsw_colour
#' @export
orange_03 <- "#ffce99"
#' @rdname nsw_colour
#' @export
orange_04 <- "#fdeddf"

#' @rdname nsw_colour
#' @export
yellow_01 <- "#694800"
#' @rdname nsw_colour
#' @export
yellow_02 <- "#faaf05"
#' @rdname nsw_colour
#' @export
yellow_03 <- "#fde79a"
#' @rdname nsw_colour
#' @export
yellow_04 <- "#fff4cf"

#' @rdname nsw_colour
#' @export
brown_01 <- "#523719"
#' @rdname nsw_colour
#' @export
brown_02 <- "#b68d5d"
#' @rdname nsw_colour
#' @export
brown_03 <- "#e8d0b5"
#' @rdname nsw_colour
#' @export
brown_04 <- "#ede3d7"


#' NSW Government colour palette function
#' 
#' A utility function for filtering and sorting the \emph{NSW Government} palette. 
#' Can include different decisions based on the number of colours needed.
#' @section Illustration:
#' \if{html}{\figure{nswgov_palette.png}{options: width=750 alt="NSW Government palette"}}
#' @references Masterbrand quick reference guide: \url{https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf}
#' @examples
#' nswgov_pal()(6)
#' 
#' library("scales")
#' show_col(nswgov_pal()(6))
#' @name nswgov_pal
#' @export
NULL

#' @rdname nswgov_pal
#' @export
nswgov_pal <- function (colour_cols = c("Grey", "Blue", "Red", "Green", "Teal", "Purple", "Fuchsia", "Orange", "Yellow", "Brown"), 
                        tonal_rows = NULL,
                        order = c("colour_col", "tonal_row"),
                        reverse = FALSE,
                        pattern = FALSE){
  f <- function(n) {
    # If tonal_rows not specified by user, defaults are as follows:
    if (is.null(tonal_rows)) {
      # If NOT a pattern:
      if (!pattern) {
        if (n == 1L | n == 2L | n == 4L) {
          tonal_rows <- 2:3
        } else {
          tonal_rows <- 1:3
        }
      } else { # If palette is being used for pattern overlay: Aim for high contrasting patterns (inverse of above tones)
        if (n == 1L | n == 2L | n == 4L) {
          tonal_rows <- c(4,1)
        } else {
          tonal_rows <- c(3,4,1)
        }
      }
    }
    
    # Translating colour_cols to sentence casing:
    colour_cols2 <- tools::toTitleCase(colour_cols)
    
    # Translating tonal_rows to e.g. "01":
    if(!"0" %in% substr(tonal_rows,1,1) ){
      tonal_rows2 <- paste0("0", tonal_rows)
    } else {
      tonal_rows2 <- tonal_rows
    }
    
    # Selecting colours from nswgov_palette (data object)
    colours <- dplyr::filter(nswgov_palette, 
                             colour_col %in% colour_cols2,
                             tonal_row %in% tonal_rows2
      )  |> 
      dplyr::mutate(colour_col = factor(colour_col, levels = colour_cols2),
                    tonal_row = factor(tonal_row, levels = tonal_rows2)) |> 
      # Sort according to the ordering preference specified in parameters (e.g. sorted by Colour then Tone):
      dplyr::arrange(!!as.symbol(order[1]), !!as.symbol(order[2])) |> 
      dplyr::select(name, value) |> 
      # Ensure that the palette only carries the required number of colours, n:
      head(n) |> 
      tibble::deframe()

    if (reverse) colours <- rev(colours)
    
    unname(colours[seq_len(n)])
    }
  # max_n <- length(colours)
  # attr(f, "max_n") <- max_n
  f
}

#' @rdname nswgov_pal
#' @export
nswgov_ramp_pal <- function(colours = c(grey_03, blue_02, blue_01), na.colour = grey_04, reverse = FALSE, alpha = TRUE) {
  cols <- colours
  if (reverse) cols <- rev(cols)
  
  pal <- scales::colour_ramp(cols, na.color = na.colour, alpha = alpha)
  
  function(n) {
    pal(seq(0, 1, length.out = n))
  }
}

#' NSW Government colour scales
#'
#' Colour scales using values from the \emph{NSW Government} palette. Use \code{scale_colour} prefix for colour aesthetics and \code{scale_fill} prefix for fill aesthetics.
#' @section Illustration:
#' \if{html}{\figure{nswgov_palette.png}{options: width=750 alt="NSW Government palette"}}
#' @param colour_cols "Colour columns" to use from the palette.
#' @param tonal_rows "Tonal rows" to use from the palette. If \code{NULL}: will default to \code{1:3}, unless exactly 1,2 or 4 colours are needed, in which case \code{2:3} will be used.
#' @param order Hierarchy in which to sort/cycle through colour library. Defaults to \code{c("colour_col", "tonal_row")} which will sort by \code{colour_col} first and then \code{tonal_row} - e.g. Blue 01, ..., Blue 03, Grey 01, ... Grey 03, etc.
#' @param reverse Logical to reverse order of existing colours in the palette. \code{FALSE} by default.
#' @param values Colour values (e.g. \code{grey_01} or \code{"#22272b"}) to select specific colours from the palette. If used, this will override \code{colour_cols}, \code{tonal_rows} and \code{order}.
#' @param binned For continuous scales, logical deciding whether to bin continuous data into discrete groups. \code{FALSE} by default.
#' @param colours For continuous scales, colour values to use.
#' @param ... Arguments passed on to the used scale function.
#' @seealso Discrete scales use the \code{\link[ggplot2]{discrete_scale}()} scale function. The main palette function uses is \code{\link{nswgov_pal}()}.
#' 
#' Continuous scales use \code{\link[ggplot2]{scale_colour_gradientn}()} and \code{\link[ggplot2]{scale_fill_gradientn}()}
#' @examples
#' # Use scale_*_nsw with discrete data:
#' # scale_colour_nsw()
#' # scale_fill_nsw()
#' 
#' # Use scale_*_nsw_gradient with continuous data:
#' # scale_colour_nsw_gradient()
#' # scale_fill_nsw_gradient()
#' 
#' # For NSW Government defaults use:
#' # scale_*_nsw()
#' 
#' # For ONE Treasury defaults use:
#' # scale_*_tsy()
#' @references Masterbrand quick reference guide: \url{https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf}
#' @name scale_colour
NULL

# Discrete:
### NSW Government
#' @rdname scale_colour
#' @export
scale_colour_nsw <- function (colour_cols = c("Blue", "Grey", "Red", "Green", "Teal", "Purple", "Fuchsia", "Orange", "Yellow", "Brown"), tonal_rows = NULL, order = c("colour_col", "tonal_row"), values = NULL, reverse = FALSE, ...){
  if(!is.null(values)){
    ggplot2::scale_colour_manual(values = values, ...)
  } else{
    ggplot2::discrete_scale(aesthetics = "colour", palette = nswgov_pal(colour_cols, tonal_rows, order, reverse), ...)
  }
}

#' @rdname scale_colour
#' @export
scale_fill_nsw <- function (colour_cols = c("Blue", "Grey", "Red", "Green", "Teal", "Purple", "Fuchsia", "Orange", "Yellow", "Brown"), tonal_rows = NULL, order = c("colour_col", "tonal_row"), values = NULL, reverse = FALSE, ...){
  if(!is.null(values)){
    ggplot2::scale_fill_manual(values = values, ...)
  } else{
    ggplot2::discrete_scale(aesthetics = "fill", palette = nswgov_pal(colour_cols, tonal_rows, order, reverse), ...)
  }
}

#' @rdname scale_colour
#' @export
scale_colour_nsw_ramp <- function (colours = c(grey_03, blue_02, blue_01), na.colour = grey_04, reverse = FALSE, ...) {
  ggplot2::discrete_scale(aesthetics = "colour", palette = nswgov_ramp_pal(colours, na.colour, reverse), ...)
}
#' @rdname scale_colour
#' @export
scale_fill_nsw_ramp <- function (colours = c(grey_03, blue_02, blue_01), na.colour = grey_04, reverse = FALSE, ...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = nswgov_ramp_pal(colours, na.colour, reverse), ...)
}


### ONE Treasury
#' @rdname scale_colour
#' @export
scale_colour_tsy <- function (colour_cols = c("Teal", "Grey", "Orange", "Green", "Blue", "Yellow", "Purple", "Fuchsia", "Brown", "Red"), tonal_rows = NULL, order = c("colour_col", "tonal_row"), values = NULL, reverse = FALSE, ...){
  if(!is.null(values)){
    ggplot2::scale_colour_manual(values = values, ...)
  } else{
    ggplot2::discrete_scale(aesthetics = "colour", palette = nswgov_pal(colour_cols, tonal_rows, order, reverse), ...)
  }
}

#' @rdname scale_colour
#' @export
scale_fill_tsy <- function (colour_cols = c("Teal", "Grey", "Orange", "Green", "Blue", "Yellow", "Purple", "Fuchsia", "Brown", "Red"), tonal_rows = NULL, order = c("colour_col", "tonal_row"), values = NULL, reverse = FALSE, ...){
  if(!is.null(values)){
    ggplot2::scale_fill_manual(values = values, ...)
  } else{
    ggplot2::discrete_scale(aesthetics = "fill", palette = nswgov_pal(colour_cols, tonal_rows, order, reverse), ...)
  }
}

#' @rdname scale_colour
#' @export
scale_colour_tsy_ramp <- function (colours = c(green_03, teal_01), na.colour = grey_04, reverse = FALSE, ...) {
  ggplot2::discrete_scale(aesthetics = "colour", palette = nswgov_ramp_pal(colours, na.colour, reverse), ...)
}
#' @rdname scale_colour
#' @export
scale_fill_tsy_ramp <- function (colours = c(green_03, teal_01), na.colour = grey_04, reverse = FALSE, ...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = nswgov_ramp_pal(colours, na.colour, reverse), ...)
}




# Continuous:
### NSW Government
#' @rdname scale_colour
#' @export
scale_colour_nsw_gradient <- function (colours = c(blue_03, blue_01), na.value = grey_04, binned = FALSE, reverse = FALSE, ...){
  cols <- colours
  if (reverse) {cols <- rev(cols)}
  if (binned) {
    ggplot2::scale_colour_stepsn(low = low, high = high, na.value = na.value, ...)
  } else {
    ggplot2::scale_colour_gradientn(low = low, high = high, na.value = na.value, ...)
  }
}
#' @rdname scale_colour
#' @export
scale_colour_nsw_c <- function (...){
  scale_colour_nsw_gradient(...)
  warning("scale_colour_nsw_c() HAS BEEN RENAMED scale_colour_nsw_gradient()... PLEASE UPDATE YOUR CODE!")
}

#' @rdname scale_colour
#' @export
scale_fill_nsw_gradient <- function (colours = c(blue_03, blue_01), na.value = grey_04, binned = FALSE, reverse = FALSE, ...){
  cols <- colours
  if (reverse) {cols <- rev(cols)}
  if (binned) {
    ggplot2::scale_fill_stepsn(colours = cols, na.value = na.value, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = cols, na.value = na.value, ...)
  }
}
#' @rdname scale_colour
#' @export
scale_fill_nsw_c <- function (...){
  scale_fill_nsw_gradient(...)
  warning("scale_fill_nsw_c() HAS BEEN RENAMED scale_fill_nsw_gradient()... PLEASE UPDATE YOUR CODE!")
}


### ONE Treasury
#' @rdname scale_colour
#' @export
scale_colour_tsy_gradient <- function (colours = c(green_03, teal_01), na.value = grey_04, binned = FALSE, reverse = FALSE, ...){
  cols <- colours
  if (reverse) {cols <- rev(cols)}
  if (binned) {
    ggplot2::scale_colour_stepsn(colours = cols, na.value = na.value, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = cols, na.value = na.value, ...)
    }
}
#' @rdname scale_colour
#' @export
scale_colour_tsy_c <- function (...){
  scale_colour_tsy_gradient(...)
  warning("scale_colour_tsy_c() HAS BEEN RENAMED scale_colour_tsy_gradient()... PLEASE UPDATE YOUR CODE!")
  }

#' @rdname scale_colour
#' @export
scale_fill_tsy_gradient <- function (colours = c(green_03, teal_01), na.value = grey_04, binned = FALSE, reverse = FALSE, ...){
  cols <- colours
  if (reverse) {cols <- rev(cols)}
  if (binned) {
    ggplot2::scale_fill_stepsn(colours = cols, na.value = na.value, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = cols, na.value = na.value, ...)
    }
}
#' @rdname scale_colour
#' @export
scale_fill_tsy_c <- function (...){
  scale_fill_tsy_gradient(...)
  warning("scale_fill_tsy_c() HAS BEEN RENAMED scale_fill_tsy_gradient()... PLEASE UPDATE YOUR CODE!")
}




# Patterns:
#' @rdname scale_colour
#' @export
scale_pattern_fill_nsw <- function (colour_cols = c("Blue", "Grey", "Red", "Green", "Teal", "Purple", "Fuchsia", "Orange", "Yellow", "Brown"), tonal_rows = NULL, order = c("colour_col", "tonal_row"), values = NULL, reverse = FALSE, ...){
  if(!is.null(values)){
    list(
      ggpattern::scale_pattern_fill_manual(values = values, ...),
      scale_pattern_manual(values = c("none", "stripe", "circle", "crosshatch", "wave"))
    )
  } else{
    list(
      ggplot2::discrete_scale(aesthetics = "pattern_fill", palette = nswgov_pal(colour_cols, tonal_rows, order, reverse, pattern = TRUE), ...),
      scale_pattern_manual(values = c("none", "stripe", "circle", "crosshatch", "wave"))
    )
  }
}
#' @rdname scale_colour
#' @export
scale_pattern_fill_tsy <- function (colour_cols = c("Teal", "Grey", "Orange", "Green", "Blue", "Yellow", "Purple", "Fuchsia", "Brown", "Red"), tonal_rows = NULL, order = c("colour_col", "tonal_row"), values = NULL, reverse = FALSE, ...){
  if(!is.null(values)){
    list(
      ggpattern::scale_pattern_fill_manual(values = values, ...),
      scale_pattern_manual(values = c("none", "stripe", "circle", "crosshatch", "wave"))
    )
  } else{
    list(
      ggplot2::discrete_scale(aesthetics = "pattern_fill", palette = nswgov_pal(colour_cols, tonal_rows, order, reverse, pattern = TRUE), ...),
      scale_pattern_manual(values = c("none", "stripe", "circle", "crosshatch", "wave"))
    )
  }
}
