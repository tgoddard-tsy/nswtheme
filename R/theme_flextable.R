#' NSW Government flextable themes
#'
#' Apply NSW Government styling to a [flextable::flextable()] object.
#'
#' `theme_nsw_flextable()` provides the standard NSW Government table theme.
#' `theme_tsy_flextable()` applies NSW Treasury colours, while
#' `theme_nsw_flextable_budget()` provides defaults for Budget tables.
#'
#' The themes format table fonts, text colours, borders, header and body
#' backgrounds, alignment and numeric values.
#'
#' @param x A [flextable::flextable()] object.
#' @param base_size Base font size, in points.
#' @param digits Number of decimal places used to format numeric columns.
#' @param ink Foreground colour used for body and footer text and table
#'   borders.
#' @param paper Background colour used as the default table background.
#' @param accent Accent colour used by the theme.
#' @param odd_header,even_header Background colours for odd and even header
#'   rows.
#' @param odd_body,even_body Background colours for odd and even body rows.
#'
#' @returns A formatted [flextable::flextable()] object.
#'
#' @examples
#' library(flextable)
#'
#' ft <- flextable(head(airquality))
#'
#' # Standard NSW Government theme
#' theme_nsw_flextable(ft)
#'
#' # NSW Treasury theme
#' theme_tsy_flextable(ft)
#'
#' # Budget theme
#' theme_nsw_flextable_budget(ft)
#'
#' @seealso
#' [flextable::flextable()], [set_defaults_nsw()]
#'
#' @name theme_nsw_flextable
NULL

#' @rdname theme_nsw_flextable
#' @export
theme_nsw_flextable <- function (x, base_size = 9, digits = 1,
                                 ink = grey_01, paper = 'white', accent = blue_04, 
                                 odd_header = blue_01, even_header = blue_01, odd_body = paper, even_body = accent){
  # Adapted from theme_zebra()
  if (!inherits(x, 'flextable')) {
    stop("`theme_nsw_flextable()` supports only flextable objects.")
  }
  h_nrow <- nrow_part(x, 'header')
  f_nrow <- nrow_part(x, 'footer')
  b_nrow <- nrow_part(x, 'body')
  
  x <- border_remove(x)
  
  std_border <- fp_border_default(width = 1, color = ink)
  x <- fontsize(x, size = base_size, part = 'all')
  x <- font(x, fontname = 'Public Sans', part = 'all')
  x <- bold(x, bold = FALSE, part = 'all')
  x <- font(x, fontname = 'Public Sans SemiBold', part = 'header')
  x <- color(x, color = paper, part = 'header')
  x <- color(x, color = ink, part = 'body')
  x <- color(x, color = ink, part = 'footer')
  
  # Borders
  x <- hline_bottom(x, part = 'body', border = std_border )
  
  # Header background  
  x <- bold(x = x, bold = TRUE, part = 'header')
  if (h_nrow > 0) {
    even <- seq_len(h_nrow)%%2 == 0
    odd <- !even
    x <- bg(x = x, i = odd, bg = odd_header, part = 'header')
    x <- bg(x = x, i = even, bg = even_header, part = 'header')
  }
  
  # Body background
  if (b_nrow > 0) {
    even <- seq_len(b_nrow)%%2 == 0
    odd <- !even
    x <- bg(x = x, i = odd, bg = odd_body, part = 'body')
    x <- bg(x = x, i = even, bg = even_body, part = 'body')
  }
  
  x <- align(x = x, align = 'center', part = 'header')
  x <- valign(x, valign = 'top', part = 'footer')
  
  x <- align_text_col(x, align = 'left', header = FALSE)
  x <- align_nottext_col(x, align = 'right', header = FALSE)
  x <- colformat_double(x, big.mark = ",", digits = digits)
  fix_border_issues(x)
}

#' @rdname theme_nsw_flextable
#' @export
theme_tsy_flextable <- function (x, base_size = 9, digits = 1, 
                                 ink = grey_01, paper = 'white', accent = green_04, 
                                 odd_header = teal_01, even_header = teal_01, odd_body = 'transparent', even_body = accent){
  theme_nsw_flextable(x, base_size = base_size, digits = digits, 
                      ink = ink, paper = paper, accent = accent,
                      odd_header = odd_header, even_header = even_header, odd_body = odd_body, even_body = even_body)
}

#' @rdname theme_nsw_flextable
#' @export
theme_nsw_flextable_budget <- function (x, base_size = 8, digits = 1, 
                                        ink = 'black', paper = 'transparent', accent = grey_04, 
                                        odd_header = accent, even_header = accent, odd_body = paper, even_body = paper){
  theme_nsw_flextable(x, base_size = base_size, digits = digits, 
                      ink = ink, paper = paper, accent = accent,
                      odd_header = odd_header, even_header = even_header, odd_body = odd_body, even_body = even_body)
}