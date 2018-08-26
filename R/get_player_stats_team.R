#' Gets players' stats and URLs for specified team
#' 
#' Returns a data frame of players' stats and player URLs for user supplied teams & seasons
#'
#' @param ... Function requires a \code{team_url}, \code{team_name}, \code{league}, and \code{season}. Additional data may be supplied. All of this information comes directly from \code{get_teams()}, if desired.
#' @param .progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @examples 
#' 
#' # The function works in conjunction with get_teams()
#' teams <- get_teams("ohl", "2017-2018")
#' get_player_stats_team(teams)
#' 
#' # All functions are easily pipeable too
#' get_teams(c("shl", "allsvenskan"), c("2008-2009", "2009-2010", "2010-2011")) %>%
#'   get_player_stats_team(.progress = TRUE)
#'   
#' # It's also easy to get player stats for only 1 team   
#' get_teams("ncaa iii", "2017-2018") %>%
#'   filter(team == "Hamilton College") %>%
#'   get_player_stats_team()
#'   
#' @export
#' @import dplyr
#' 
get_player_stats_team <- function(..., .progress = TRUE) {
  
  if (.progress) {
    
    pb <- progress::progress_bar$new(format = "get_player_stats_team() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    
    pb$tick(0)}
  
  .get_player_stats_team <- function(team_url, team_name, league, season, ...) {
    
    seq(30, 35, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    page <- team_url %>% xml2::read_html()
    
    player_stats <- page %>%
      rvest::html_node('[class="table table-striped table-sortable skater-stats highlight-stats"]') %>%
      rvest::html_table() %>%
      purrr::set_names("number", "name", "games_played", "goals", "assists", "points", "penalty_minutes", "plus_minus", "blank", "games_played_playoffs", "goals_playoffs", "assists_playoffs", "points_playoffs", "penalty_minutes_playoffs", "plus_minus_playoffs") %>%
      as_tibble() %>%
      filter(row_number() > 1) %>%
      filter(row_number() < if_else(!any(is.na(number) & name != ""), n(), which(is.na(number) & name != "")[1])) %>%
      filter(!is.na(number) & name != "") %>%
      mutate(position = stringr::str_split(name, "\\(", simplify = TRUE, n = 2)[,2]) %>%
      mutate(position = stringr::str_split(position, "\\)", simplify = TRUE, n = 2)[,1]) %>%
      mutate(name = stringr::str_split(name, "\\(", simplify = TRUE, n = 2)[,1]) %>%
      mutate_all(~na_if(., "-")) %>%
      mutate_all(~na_if(., "")) %>%
      mutate_all(stringr::str_squish) %>%
      select(-c(blank, number)) %>%
      select(name, position, everything())
    
    league_from_team_page <- page %>%
      rvest::html_node(".skater-stats .title a") %>%
      rvest::html_text() %>%
      stringr::str_squish()
    
    league_from_team_page_lower_case <- league_from_team_page %>%
      tolower() %>%
      stringr::str_replace_all(" ", "-")
    
    player_urls <- page %>%
      rvest::html_nodes("tbody+ tbody .txt-blue a") %>%
      rvest::html_attr("href") %>%
      as_tibble() %>%
      purrr::set_names("player_url") %>%
      filter(row_number() < if_else(!any(!stringr::str_detect(player_url, "player")), n(), which(!stringr::str_detect(player_url, "player"))[1]))
    
    all_data <- player_stats %>%
      bind_cols(player_urls) %>% 
      mutate(team = team_name) %>%
      mutate(season = season) %>%
      mutate(team_url = team_url) %>%
      mutate(.league = league) %>%
      mutate(league = league_from_team_page) %>%
      mutate(league_from_team_page_lower_case = league_from_team_page_lower_case) %>%
      filter(league_from_team_page_lower_case == .league) %>%
      select(-c(league_from_team_page_lower_case, .league)) %>%
      mutate_at(vars(-c(name, team, league, season, position, player_url, team_url)), as.numeric) %>%
      select(name, team, league, season, everything())
    
    if (.progress) {pb$tick()}
    
    return(all_data)}
  
  player_stats_team <- purrr::pmap_dfr(..., elite::persistently(.get_player_stats_team, max_attempts = 10))
  
  return(player_stats_team)
  
}