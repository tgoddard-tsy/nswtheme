#' flextable theme based on \strong{NSW Treasury} brand template
#'
#' Apply \emph{NSW Treasury} theme to a flextable object.
#' @param x a flextable object
#' @param odd_header,odd_body,even_header,even_body odd/even colors for table header and body
#' @param digits number of decimal places to use for cells of class \code{double}. For select columns/rows/cells, use \code{flextable::\link[flextable]{colformat_double}()}. 
#' @section Illustration:
#' NSW Government theme (default):
#' \if{html}{\figure{ft_nsw.png}{options: width=550 alt="A 'flextable' object with NSW Government theme"}}
#' \cr
#' NSW Treasury theme:
#' \if{html}{\figure{ft_tsy.png}{options: width=550 alt="A 'flextable' object with NSW Treasury theme"}}
#' @seealso This theme has been modified from \code{flextable::\link[flextable]{theme_zebra}()}
#' @examples 
#' library(flextable)
#' 
#' ft <- flextable(head(airquality))
#' ft <- theme_tsy_flextable(ft)
#' ft
#' @name theme_tsy_flextable
NULL

#' @rdname theme_tsy_flextable
#' @export
theme_nsw_flextable <- function (x, base_size = 9, digits = 1,
                                 ink = grey_01, paper = 'white', accent = blue_04, 
                                 odd_header = blue_01, even_header = blue_01, odd_body = paper, even_body = accent){
  # Adapted from theme_zebra()
  if (!inherits(x, 'flextable')) {
    stop("theme_tsy_flextable supports only flextable objects.")
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

#' @rdname theme_tsy_flextable
#' @export
theme_tsy_flextable <- function (x, base_size = 9, digits = 1, 
                                 ink = grey_01, paper = 'white', accent = green_04, 
                                 odd_header = teal_01, even_header = teal_01, odd_body = 'transparent', even_body = accent){
  theme_nsw_flextable(x, base_size = base_size, digits = digits, 
                      ink = ink, paper = paper, accent = accent,
                      odd_header = odd_header, even_header = even_header, odd_body = odd_body, even_body = even_body)
}

#' @rdname theme_tsy_flextable
#' @export
theme_nsw_flextable_budget <- function (x, base_size = 8, digits = 1, 
                                        ink = 'black', paper = 'transparent', accent = grey_04, 
                                        odd_header = accent, even_header = accent, odd_body = paper, even_body = paper){
  theme_nsw_flextable(x, base_size = base_size, digits = digits, 
                      ink = ink, paper = paper, accent = accent,
                      odd_header = odd_header, even_header = even_header, odd_body = odd_body, even_body = even_body)
}