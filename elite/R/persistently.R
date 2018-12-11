#' Capture side effects.
#'
#' Imported from \url{https://github.com/ijlyttle} and his \code{warrenr} package. These functions wrap functions so that instead of generating side effects
#' through printed output, messages, warnings, and errors, they return enhanced
#' output. They are all adverbs because they modify the action of a verb (a
#' function).
#'
#' @param .f A function, formula, or atomic vector.
#' @param quiet Hide errors (\code{TRUE}, the default), or display them
#'   as they occur?
#' @param max_attempts Positive integer. \code{persistent} functions will try
#'   to run this many times before giving up.
#' @param wait_seconds Positive number. Base multiplier for time in seconds to
#'   wait between attempts. The time increases exponentially, with a wait time
#'   randomly chosen from a uniform distribution between \code{0} and
#'   \code{wait_seconds * 2 ^ (i - 1)} seconds, between the \code{i}th and
#'   \code{i + 1}th attempts.
#'
#' @return wrapped function uses a default value (\code{otherwise})
#'   whenever an error occurs max_attempts times.
#' @export
#' @examples
#'
#' # persistently() makes a function repeatedly try to work
#'
#' risky_runif <- function(lo = 0, hi = 1) {
#'   y <- stats::runif(1, lo, hi)
#'   if(y < 0.9) {
#'     stop(y, " is too small")
#'   }
#'   y
#' }
#'
#' persistent_risky_runif <- persistently(
#'   risky_runif, quiet = FALSE, wait_seconds = 0.01)
#'
#' \dontrun{
#'   set.seed(1)
#'   persistent_risky_runif()
#'   set.seed(3)
#'   persistent_risky_runif()
#' }
#'
#'
#' @export
#'
persistently <- function(.f, quiet = TRUE, max_attempts = 5,
                         wait_seconds = 0.1) {
  
  .f <- purrr::as_mapper(.f)
  
  force(quiet)
  force(max_attempts)
  force(wait_seconds)
  
  function(...) {
    for (i in seq_len(max_attempts)) {
      answer <- purrr:::capture_error(.f(...), quiet = quiet)
      if (is.null(answer$error)) {
        return(answer$result)
      }
      if (wait_seconds > 0) {
        actual_wait_seconds <- stats::runif(1, 0, wait_seconds * 2 ^ (i - 1))
        if (!quiet) {
          message(sprintf("Retrying in %.3g seconds.", actual_wait_seconds))
        }
        Sys.sleep(actual_wait_seconds)
      }
    }
    if (!quiet) {
      msg <- sprintf(
        "%s failed after %d tries",
        deparse(match.call()),
        max_attempts
      )
      message(msg)
    }
    stop(answer$error)
  }
}
