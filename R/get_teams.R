#' Gets teams and team URLs for specified league and season
#' 
#' Returns a data frame of teams and their URLs for user supplied leagues & seasons
#' 
#' @param .league Leagues from which the user wants to scrape data
#' @param .season Seasons for which the user wants to scrape data. Must be of the form `2017-2018`, `1964-1965`, etc.
#' @param .progress Sets a Progress Bar. Defaults to `TRUE`.
#' @param ... Allows the user to supply other information to the function. If you don't know what this means, then don't worry about it.
#' @examples 
#' get_teams("ohl", "2012-2013")
#' 
#' get_teams(c("SHL", "allsvenskan", "ncaa iii"), c("2014-2015", "1993-1994"), .progress = TRUE)
#' 
#' @export
#' @import dplyr
#' 
get_teams <- function(.league, .season, .progress = TRUE, ...) {
  
  leagues <- .league %>% 
    as_tibble() %>% 
    purrr::set_names(".league") %>% 
    mutate(.league = stringr::str_replace_all(.league, " ", "-"))
  
  seasons <- .season %>%
    as_tibble() %>%
    purrr::set_names(".season")
  
  mydata <- tidyr::crossing(leagues, seasons)
  
  if (.progress) {
    
    pb <- progress::progress_bar$new(format = "get_teams() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    pb$tick(0)}
  
  .get_teams <- function(.league, .season, ...) {
    
    seq(30, 35, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    page <- stringr::str_c("https://www.eliteprospects.com/league/", .league, "/", .season) %>% xml2::read_html()
    
    team_url <- page %>% 
      rvest::html_nodes("#standings .team a") %>% 
      rvest::html_attr("href") %>%
      stringr::str_c(., "?tab=stats") %>%
      as_tibble() %>%
      purrr::set_names("team_url")
    
    team <- page %>%
      rvest::html_nodes("#standings .team a") %>%
      rvest::html_text() %>%
      stringr::str_trim(side = "both") %>%
      as_tibble() %>%
      purrr::set_names("team")
    
    league <- page %>%
      rvest::html_nodes("small") %>%
      rvest::html_text() %>%
      stringr::str_trim(side = "both")
    
    season <- stringr::str_split(.season, "-", simplify = TRUE, n = 2)[,2] %>%
      stringr::str_sub(3, 4) %>%
      stringr::str_c(stringr::str_split(.season, "-", simplify = TRUE, n = 2)[,1], ., sep = "-")
    
    all_data <- team %>%
      bind_cols(team_url) %>% 
      mutate(league = league) %>%
      mutate(season = season)
    
    if (.progress) {pb$tick()}
    
    return(all_data)}
  
  league_team_data <- purrr::map2_dfr(mydata[[".league"]], mydata[[".season"]], elite::persistently(.get_teams, max_attempts = 10))

  return(league_team_data)
  
}
