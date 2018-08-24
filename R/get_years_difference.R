#' Gets exact age
#' 
#' Returns the exact age of a player given his birth date and some reference date
#' 
#' @param from The start date. Typically the player's birth date.
#' @param to The reference date. Used for finding a player's draft eligibility, it's typically `2018-09-15`, or whatever the year of the NHL draft is.
#' @examples 
#' get_years_difference("2000-09-15", "2018-09-15")
#' 
#' # My birthday -- don't forget it!
#' get_years_difference("1995-10-22", "2018-08-27")
#' 
#' @export
#' @import dplyr
#' 
get_years_difference <- function(from, to) {
  
  to <- lubridate::as_date(to)
  from <- lubridate::as_date(from)
  
  age <- lubridate::`%--%`(from, to) / lubridate::years(1)
  
  return(age)
  
}