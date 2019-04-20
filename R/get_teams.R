#' Gets teams and team URLs for specified league and season
#' 
#' Returns a data frame of teams and their URLs for user supplied leagues & seasons. Bear in mind that there are some cases in which teams that aren't part of the user-supplied league are returned in the data frame. This is normal and is fixed later when using \code{get_player_stats_team()}.
#' 
#' @param league Leagues from which the user wants to scrape data
#' @param season Seasons for which the user wants to scrape data. Must be of the form \code{2018}, \code{1965}, etc. for the 2017-18 and 1964-65 seasons, respectively.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @param ... Allows the user to supply other information to the function. If you don't know what this means, then don't worry about it.
#' @examples 
#' get_teams("ohl", 2013)
#' 
#' get_teams(c("SHL", "allsvenskan", "ncaa iii"), c(1994, 2017:2018), progress = FALSE)
#' 
#' @export
#' @import dplyr
#' 
get_teams <- function(league, season, progress = TRUE, other = "", ...) {
  
  if (any(nchar(season) > 4) | any(!stringr::str_detect(season, "[0-9]{4,4}"))) {
    
    cat("\n")
    
    stop('\n\nMake sure your seasons are all 4-digit numbers
          \rlike 1994 (for 1993-94) and 2017 (for 2016-17)\n\n')
    
  }
  
  else if (any(as.numeric(season) > 1 + lubridate::year(Sys.time()))) {
    
    cat("\n")
    
    stop('\n\nMake sure your seasons are all actual
          \rseasons (not 2025, you silly goose)\n\n')
    
  }
  
  league <- stringr::str_replace_all(league, " ", "-")
  season <- stringr::str_c(as.numeric(season) - 1, as.numeric(season), sep = "-")
  
  leagues <- league %>% 
    tibble::enframe(name = NULL) %>% 
    purrr::set_names("league")
  
  seasons <- season %>%
    tibble::enframe(name = NULL) %>% 
    purrr::set_names("season")
  
  mydata <- tidyr::crossing(leagues, seasons)
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_teams() [:bar] :percent ETA: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    cat("\n")
    
    pb$tick(0)}
  
  .get_teams <- function(league, season, ...) {
    
    if (other == "evan") {
      
      seq(7, 11, by = 0.001) %>%
        sample(1) %>%
        Sys.sleep()
      
    }
    
    else {
      
      seq(20, 35, by = 0.001) %>%
        sample(1) %>%
        Sys.sleep()
      
    }
    
    page <- stringr::str_c("https://www.eliteprospects.com/league/", league, "/", season) %>% xml2::read_html()
    
    league_name <- page %>%
      rvest::html_nodes("small") %>%
      rvest::html_text() %>%
      stringr::str_squish()
    
    team_urls_rosters <- page %>% 
      rvest::html_nodes(".column-4 i+ a") %>% 
      rvest::html_attr("href") %>%
      ifelse(stringr::str_detect(., "[0-9]{4,4}-[0-9]{4,4}"), ., stringr::str_c(., season, sep = "/")) %>%
      ifelse(stringr::str_detect(., "https"), stringr::str_c(., "?tab=stats"), .) %>%
      tibble::enframe(name = NULL) %>% 
      purrr::set_names("team_url") %>%
      mutate(team_url = stringr::str_replace_all(team_url, "\\-\\-", ""))
    
    teams_rosters <- page %>%
      rvest::html_nodes(".column-4 i+ a") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      tibble::enframe(name = NULL) %>% 
      purrr::set_names("team")
    
    team_urls_standings <- page %>%
      rvest::html_nodes("#standings .team a") %>%
      rvest::html_attr("href") %>%
      ifelse(stringr::str_detect(., "[0-9]{4,4}-[0-9]{4,4}"), ., stringr::str_c(., season, sep = "/")) %>%
      ifelse(stringr::str_detect(., "https"), stringr::str_c(., "?tab=stats"), .) %>%
      tibble::enframe(name = NULL) %>% 
      purrr::set_names("team_url") %>%
      mutate(team_url = stringr::str_replace_all(team_url, "\\-\\-", ""))
    
    teams_standings <- page %>%
      rvest::html_nodes("#standings .team a") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      tibble::enframe(name = NULL) %>% 
      purrr::set_names("team")
    
    teams <- teams_standings %>% 
      bind_rows(teams_rosters) %>%
      mutate(team_lower = tolower(team)) %>%
      distinct(team_lower, .keep_all = TRUE) %>%
      select(team)
    
    team_urls <- team_urls_standings %>%
      bind_rows(team_urls_rosters) %>%
      mutate(team_url = stringr::str_replace_all(team_url, c("\\-\\/" = "/"))) %>%
      distinct()
    
    season <- stringr::str_split(season, "-", simplify = TRUE, n = 2)[,2] %>%
      stringr::str_sub(3, 4) %>%
      stringr::str_c(stringr::str_split(season, "-", simplify = TRUE, n = 2)[,1], ., sep = "-")
    
    all_data <- teams %>%
      bind_cols(team_urls) %>% 
      mutate(league = league_name) %>%
      mutate(season = season)
    
    if (progress) {pb$tick()}
    
    return(all_data)
    
  }
  
  insistently_get_teams <- purrr::insistently(.get_teams, rate = purrr::rate_delay(pause = 0.1, max_times = 10))
  
  try_get_teams <- function(league, season, ...) {
    
    tryCatch(insistently_get_teams(league, season, ...), 
             
             error = function(e) {
               cat("\n\nThere's an error:\n\n", sep = "")
               print(e)
               cat("\nHere's where it's from:\n\nLeague:\t", league, "\nSeason:\t", season, sep = "")
               cat("\n")
               tibble()},
             
             warning = function(w) {
               cat("\n\nThere's a warning:\n\n", sep = "")
               print(w)
               cat("\nHere's where it's from:\n\nLeague:\t", league, "\nSeason:\t", season, sep = "")
               cat("\n")
               tibble()})
    
  }
  
  league_team_data <- purrr::map2_dfr(mydata[["league"]], mydata[["season"]], try_get_teams)
  
  cat("\n")
  
  return(league_team_data)
  
}
