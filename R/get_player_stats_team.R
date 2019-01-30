#' Gets players' stats and URLs for specified team
#' 
#' Returns a data frame of players' stats and player URLs for user supplied teams & seasons
#'
#' @param ... Function requires a \code{team_url}, \code{team}, \code{league}, and \code{season}. Additional data may be supplied. All of this information comes directly from \code{get_teams()}, if desired.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @examples 
#' 
#' # The function works in conjunction with get_teams()
#' teams <- get_teams("ohl", 2018)
#' get_player_stats_team(teams)
#' 
#' # All functions are easily pipeable too
#' get_teams(c("shl", "allsvenskan"), 2009:2011) %>%
#'   get_player_stats_team(progress = TRUE)
#'   
#' # It's also easy to get player stats for only 1 team   
#' get_teams("ncaa iii", 2018) %>%
#'   filter(team == "Hamilton College") %>%
#'   get_player_stats_team()
#'   
#' @export
#' @import dplyr
#' 
get_player_stats_team <- function(..., progress = TRUE, other = "") {
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_player_stats_team() [:bar] :percent ETA: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    
    cat("\n")
    
    pb$tick(0)}
  
  .get_player_stats_team <- function(team_url, team, league, season, ...) {
    
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
    
    page <- team_url %>% xml2::read_html()
    
    player_stats <- page %>%
      rvest::html_node('[class="table table-striped table-sortable skater-stats highlight-stats"]') %>%
      rvest::html_table() %>%
      purrr::set_names("number", "name", "games_played", "goals", "assists", "points", "penalty_minutes", "plus_minus", "blank", "games_played_playoffs", "goals_playoffs", "assists_playoffs", "points_playoffs", "penalty_minutes_playoffs", "plus_minus_playoffs") %>%
      as_tibble() %>%
      mutate(league = ifelse(name != "" & is.na(number), name, NA)) %>% 
      tidyr::fill(league, .direction = "down") %>% 
      filter(!is.na(number)) %>%
      mutate(position = stringr::str_split(name, "\\(", simplify = TRUE, n = 2)[,2]) %>%
      mutate(position = stringr::str_split(position, "\\)", simplify = TRUE, n = 2)[,1]) %>%
      mutate(name = stringr::str_split(name, "\\(", simplify = TRUE, n = 2)[,1]) %>%
      mutate_all(~na_if(., "-")) %>%
      mutate_all(~na_if(., "")) %>%
      mutate(goals_against_average = NA) %>%
      mutate(save_percentage = NA) %>%
      mutate(goals_against_average_playoffs = NA) %>%
      mutate(save_percentage_playoffs = NA) %>%
      select(-c(blank, number)) %>%
      select(name, position, league, everything()) %>%
      mutate_all(stringr::str_squish)
    
    goalie_stats <- page %>%
      rvest::html_node('[class="table table-striped table-sortable goalie-stats highlight-stats"]') %>%
      rvest::html_table() %>%
      purrr::set_names("number", "name", "games_played", "goals_against_average", "save_percentage", "blank", "games_played_playoffs", "goals_against_average_playoffs", "save_percentage_playoffs") %>%
      as_tibble() %>%
      mutate(league = ifelse(name != "" & is.na(number), name, NA)) %>% 
      tidyr::fill(league, .direction = "down") %>% 
      filter(!is.na(number)) %>%
      mutate_all(~na_if(., "-")) %>%
      mutate_all(~na_if(., "")) %>%
      mutate(position = "G") %>%
      mutate(goals = NA) %>%
      mutate(assists = NA) %>%
      mutate(points = NA) %>%
      mutate(penalty_minutes = NA) %>%
      mutate(plus_minus = NA) %>%
      mutate(goals_playoffs = NA) %>%
      mutate(assists_playoffs = NA) %>%
      mutate(points_playoffs = NA) %>%
      mutate(penalty_minutes_playoffs = NA) %>%
      mutate(plus_minus_playoffs = NA) %>%
      select(-c(blank, number)) %>%
      select(name, position, league, everything()) %>%
      mutate_all(stringr::str_squish)
    
    skater_urls <- page %>%
      rvest::html_nodes(".skater-stats td a") %>%
      rvest::html_attr("href") %>%
      as_tibble() %>%
      purrr::set_names("player_url") %>%
      filter(stringr::str_detect(player_url, "https\\:\\/\\/www\\.eliteprospects\\.com\\/player\\/"))

    goalie_urls <- page %>%
      rvest::html_nodes(".goalie-stats td a") %>%
      rvest::html_attr("href") %>%
      as_tibble() %>%
      purrr::set_names("player_url") %>%
      filter(stringr::str_detect(player_url, "https\\:\\/\\/www\\.eliteprospects\\.com\\/player\\/"))

    player_urls <- skater_urls %>% bind_rows(goalie_urls)
    
    all_data <- player_stats %>%
      bind_rows(goalie_stats) %>%
      bind_cols(player_urls) %>% 
      mutate(team = team) %>%
      mutate(season = season) %>%
      mutate(team_url = team_url) %>%
      mutate_at(vars(-c(name, team, league, season, position, player_url, team_url)), as.numeric) %>%
      select(name, team, league, season, everything())
    
    is_this_team_actually_in_this_league <- any(all_data[["league"]] == league)
    
    if (is_this_team_actually_in_this_league == FALSE) {
      
      all_data <- tibble()
      
    }
    
    if (progress) {pb$tick()}
    
    return(all_data)}
  
  player_stats_team <- purrr::pmap_dfr(..., elite::persistently(.get_player_stats_team, max_attempts = 10))
  
  cat("\n")
  
  return(player_stats_team)
  
}
