#' Checks whether or not a particular year is a leap year 
#' according to the Gregorian calendar 
#' 
#' - Years divisible by 4 are leap years,
#' - Except years divisible by 100 are not leap years,
#' - Except years divisible by 400 are leap years.
#' 
#' @param year An integer representing the year (must be positive, non-zero, and numeric).
#'
#' @returns Logical value: TRUE if `year` is a leap year, FALSE otherwise.
#'
#' @examples 
#' is_leap(1992)   # TRUE
#' is_leap(2000)   # TRUE
#' is_leap(1900)   # FALSE
#' is_leap(2021)   # FALSE
#' @export
is_leap <- function(year) {
    # Check if year is missing or not length 1
    if (missing(year) || length(year) != 1) {
        stop("Please provide a single year value.")
    }
    
    # Check for type and validity
    if (!is.numeric(year) || is.na(year)) {
        stop("Year must be a numeric value.")
    }
    if (year == 0) {
        stop("Year 0 does not exist in the Gregorian calendar.")
    }
    if (year < 0) {
        stop("Year must be positive (greater than 0).")
    }
    if (year %% 1 != 0) {
        stop("Year must be an integer.")
    }
    
    # Leap year logic
    if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}