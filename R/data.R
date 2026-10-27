#' Recession peaks and troughs in Australia
#'
#' Quarterly recession periods identified from Australian real GDP and
#' NSW State Final Demand (SFD). Each observation records the peak immediately
#' preceding a recession and the trough marking its end.
#'
#' Recessions are identified as two or more consecutive quarters of negative
#' growth in the relevant activity measure. Australian GDP data begin in the
#' December quarter 1959 and NSW SFD data begin in the September quarter 1985.
#'
#' @format A data frame with 2 variables:
#'
#' * `peak`: Date of peak, the quarter preceding any recession period.
#' * `trough`: Date of trough, the quarter when the recessionary period reached
#'   its trough.
#'
#' @source <https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/latest-release>
#'
#' @name recessions
NULL

#' @rdname recessions
"recessions_aus"

#' @rdname recessions
"recessions_nsw"


#' Lockdown start and end dates in Australia
#'
#' A dataset containing lockdown periods related to COVID-19, marked by start
#' and end dates. Lockdown is assumed if at least one major city is in lockdown.
#'
#' Lockdowns for:
#'
#' * New South Wales (not including the Northern Beaches lockdown)
#' * Victoria
#' * Queensland
#' * Western Australia
#'
#' @format A data frame with 4 variables:
#'
#' * `start`: Date the lockdown came into effect.
#' * `end`: Date the lockdown was lifted.
#' * `days`: Length of lockdown in days.
#' * `name`: Descriptive label for the lockdown period.
#'
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


#' NSW Government colour palette data frame
#'
#' A dataset containing the NSW Government palette.
#'
#' @format A data frame with 6 variables:
#'
#' * `name`: colour name.
#' * `value`: HEX value.
#' * `colour_col`: colour column of the palette.
#' * `tonal_row`: tonal row of the palette, one of `"01"`, `"02"`, `"03"`,
#'   or `"04"`.
#' * `corporate`: `TRUE` if the colour is for corporate use.
#' * `core`: `TRUE` if the colour is a core colour.
#'
#' @section Illustration:
#' \if{html}{\figure{nswgov_palette.png}{
#'   options: width=750 alt="NSW Government palette"
#' }}
#'
#' @references
#' Masterbrand quick reference guide:
#' <https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf>
#'
#' @seealso
#' [`nswgov_palette`] is a *named vector* equivalent, which does not include
#' the additional columns.
#'
#' @name palette_df
NULL

#' @rdname palette_df
"nswgov_palette_df"


#' NSW Government colour palette
#'
#' NSW Government palette in the form of a named vector.
#'
#' @section Illustration:
#' \if{html}{\figure{nswgov_palette.png}{
#'   options: width=750 alt="NSW Government palette"
#' }}
#'
#' @references
#' Masterbrand quick reference guide:
#' <https://www.nsw.gov.au/sites/default/files/2022-01/Quick_Reference_Guide_Masterbrand.pdf>
#'
#' @seealso
#' [`nswgov_palette_df`] is a *data frame* equivalent with additional
#' information.
#'
#' @name palette
NULL

#' @rdname palette
"nswgov_palette"
