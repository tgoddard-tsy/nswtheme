#' Recession peaks and troughs in Australia
#'
#' A dataset containing periods of economic recession, marked by the peak and trough. A recession is defined as two or more consecutive quarters of negative GDP growth. 
#' GDP sourced from ABS National Accounts (beginning December quarter 1959). For NSW, State Final Demand (SFD) is used (beginning September quarter 1985).
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{peak}{date of peak, the quarter preceding any recession period}
#'   \item{trough}{date of trough, the quarter when the recessionary period reached its trough}
#'   }
#' @source \url{https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/latest-release}
#' @name recessions
NULL

#' @rdname recessions
"recessions_aus"

#' @rdname recessions
"recessions_nsw"


#' Lockdown start and end dates in Australia
#'
#' A dataset containing lockdown periods related to COVID-19, marked by start and end dates. Lockdown is assumed if at least one major city is in lockdown.\cr\cr
#' Lockdowns for:
#' \itemize{
#'   \item{New South Wales (not including the Northern Beaches lockdown)}
#'   \item{Victoria}
#'   \item{Queensland}
#'   \item{Western Australia}
#' }
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{start}{date the lockdown came into effect}
#'   \item{end}{date the lockdown was lifted}
#'   \item{days}{length of lockdown in days}
#'   \item{name}{Name of lockdown (descriptive only)}
#'   }
#' @name lockdowns
NULL

#' @rdname lockdowns
"lockdowns_nsw"

#' @rdname lockdowns
"lockdowns_vic"

#' @rdname lockdowns
"lockdowns_qld"

#' @rdname lockdowns
"lockdowns_wa"


#' NSW Government colour palette
#'
#' A dataset containing the NSW Government palette.
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{name}{Colour name}
#'   \item{value}{HEX value}
#'   \item{colour_col}{Colour column of the palette}
#'   \item{tonal_row}{Tonal row of the palette - one of "01", "02", "03", or "04"}
#'   \item{corporate}{\code{TRUE} if colour is for corporate use}
#'   \item{core}{\code{TRUE} if colour is a core colour}
#'   }
#' @seealso 
#' \code{\link{nswgov_palette_simple}} is a \emph{named vector} equivalent
#' @name palette
NULL

#' @rdname palette
"nswgov_palette"

#' NSW Government colour palette (simple)
#' 
#' NSW Government palette in the form of a named vector.
#' @seealso 
#' \code{\link{nswgov_palette}} is a \emph{dataframe} equivalent with additional information
#' @name palette_simple
NULL

#' @rdname palette_simple
"nswgov_palette_simple"
