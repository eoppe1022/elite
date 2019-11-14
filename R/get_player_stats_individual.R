#' Gets bio information and career statistics for specified player
#' 
#' Returns a data frame of players, their bio information (age, birth place, etc.), and career statistics for user supplied player URLs and names. 
#' 
#' @param ... Function requires a \code{player_url} and \code{name}. Additional data may be supplied. All of this information comes directly from \code{get_player_stats_team()} and \code{get_teams()} or \code{get_drafts()}, if desired.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @param strip_redundancy Removes variables \code{name_}, \code{player_url_}, and \code{position_}, as they're the same as \code{name}, \code{player_url}, and \code{position}. Defaults to \code{TRUE}.
#' @examples 
#' 
#' # The function works in conjunction with get_teams() and get_player_stats_team()
#' teams <- get_teams("ohl", 2018)
#' stats_team <- get_player_stats_team(teams)
#' get_player_stats_individual(stats_team)
#' 
#' # The function also works in conjunction with get_drafts()
#' drafts <- get_drafts("nhl entry draft", 2018)
#' get_player_stats_individual(drafts)
#' 
#' # All functions are easily pipeable too
#' get_teams(c("shl", "allsvenskan"), 2009:2011) %>%
#'   get_player_stats_team(progress = TRUE) %>%
#'   get_player_stats_individual(strip_redundancy = FALSE)
#'   
#' # It's also easy to get player stats & bio information for only 1 team   
#' get_teams("ncaa iii", 2018) %>%
#'   filter(team == "Hamilton College") %>%
#'   get_player_stats_team() %>%
#'   get_player_stats_individual()
#'   
#' # Once you have your data, use tidyr::unnest() to view players' career statistics
#' get_teams("ncaa iii", 2015) %>%
#'   filter(team == "Hamilton College") %>%
#'   get_player_stats_team() %>%
#'   get_player_stats_individual() %>%
#'   tidyr::unnest(player_statistics)
#'   
#' @export
#' @import dplyr
#' 
get_player_stats_individual <- function(..., progress = TRUE, strip_redundancy = TRUE, other = "") {
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_player_stats_individual() [:bar] :percent ETA: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    
    cat("\n")
    
    pb$tick(0)}
  
  .get_player_stats_individual <- function(player_url, name, ...) {
    
    if (is.na(player_url)) {
      
      all_data <- tibble(shot_handedness = NA, birth_place = NA, birth_country = NA, birthday = NA, height = NA, weight = NA, age = NA, name_ = NA, position_ = NA, player_url_ = NA)
      
      player_statistics <- NA %>% 
        enframe(name = NULL) %>%
        purrr::set_names("captaincy_") %>% 
        tidyr::nest_legacy()
      
      all_data <- all_data %>% 
        bind_cols(player_statistics) %>% 
        rename(player_statistics = data)
      
    }
    
    else {
      
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
      
      page <- player_url %>% xml2::read_html()
      
      vitals <- page %>%
        rvest::html_nodes('[class="col-xs-8 fac-lbl-dark"]') %>%
        rvest::html_text() %>%
        magrittr::extract(1:9) %>%
        stringr::str_squish() %>%
        purrr::set_names("birthday", "age", "birth_place", "birth_country", "youth_team", "position_", "height", "weight", "shot_handedness") %>%
        t() %>%
        as.data.frame() %>%
        as_tibble() %>%
        mutate(birthday = lubridate::mdy(birthday, quiet = TRUE)) %>%
        mutate(height = stringr::str_split(height, '"', simplify = TRUE, n = 2)[,1]) %>%
        mutate(feet_tall = stringr::str_split(height, "'", simplify = TRUE, n = 2)[,1]) %>%
        mutate(inches_tall = stringr::str_split(height, "'", simplify = TRUE, n = 2)[,2]) %>%
        mutate(height = ifelse(stringr::str_detect(feet_tall, "[0-9]") & stringr::str_detect(inches_tall, "[0-9]"), (as.numeric(feet_tall) * 12) + as.numeric(inches_tall), as.numeric(NA))) %>%
        mutate(weight = ifelse(stringr::str_detect(weight, "[0-9]"), stringr::str_split(weight, "lbs", simplify = TRUE, n = 2)[,1], as.numeric(NA))) %>%
        mutate(name_ = name) %>%
        mutate(player_url_ = player_url) %>%
        mutate_all(~stringr::str_trim(., side = "both")) %>%
        mutate_all(~na_if(., "-")) %>%
        mutate_all(~na_if(., "")) %>%
        select(-c(feet_tall, inches_tall, age, youth_team))
      
      skater_or_goalie <- page %>%
        rvest::html_node('[class="table table-striped table-condensed table-sortable player-stats highlight-stats"]') %>%
        rvest::html_table() 
      
      if ("GAA" %in% colnames(skater_or_goalie)) {
        
        player_statistics <- page %>%
          rvest::html_node('[class="table table-striped table-condensed table-sortable player-stats highlight-stats"]') %>%
          rvest::html_table() %>%
          purrr::set_names("season_", "team_", "league_", "games_played_", "goals_against_average_", "save_percentage_", "blank_", "playoffs_", "games_played_playoffs_", "goals_against_average_playoffs_", "save_percentage_playoffs_") %>%
          as_tibble() %>% 
          mutate_all(~na_if(., "-")) %>%
          mutate(captaincy_ = stringr::str_split(team_, "\U201C", simplify = TRUE, n = 2)[,2]) %>%
          mutate(captaincy_ = stringr::str_split(captaincy_, "\U201D", simplify = TRUE, n = 2)[,1]) %>%
          mutate(team_ = stringr::str_split(team_, "\U201C", simplify = TRUE, n = 2)[,1]) %>%
          mutate(season_ = replace(season_, season_ == "", NA)) %>%
          tidyr::fill(season_) %>%
          mutate(season_short_ = as.numeric(stringr::str_split(season_, "-", simplify = TRUE, n = 2)[,1]) + 1) %>%
          mutate(birthday = vitals[["birthday"]]) %>%
          mutate(draft_eligibility_date_ = stringr::str_c(as.character(season_short_), "09-15", sep = "-")) %>%
          mutate(age_ = elite::get_years_difference(birthday, draft_eligibility_date_)) %>%
          mutate_all(stringr::str_squish) %>%
          mutate_all(as.character) %>%      
          mutate_all(~na_if(., "")) %>%
          mutate(goals_ = NA) %>%
          mutate(assists_ = NA) %>%
          mutate(points_ = NA) %>%
          mutate(penalty_minutes_ = NA) %>%
          mutate(plus_minus_ = NA) %>%
          mutate(goals_playoffs_ = NA) %>%
          mutate(assists_playoffs_ = NA) %>%
          mutate(points_playoffs_ = NA) %>%
          mutate(penalty_minutes_playoffs_ = NA) %>%
          mutate(plus_minus_playoffs_ = NA) %>%
          select(-c(blank_, playoffs_, draft_eligibility_date_, birthday)) %>%
          select(team_, league_, captaincy_, season_, season_short_, age_, games_played_, goals_, assists_, points_, penalty_minutes_, plus_minus_, goals_against_average_, save_percentage_, games_played_playoffs_, goals_playoffs_, assists_playoffs_, points_playoffs_, penalty_minutes_playoffs_, plus_minus_playoffs_, goals_against_average_playoffs_, save_percentage_playoffs_) %>% 
          mutate_at(vars(c(team_, league_, captaincy_, season_)), as.character) %>%
          mutate_at(vars(-c(team_, league_, captaincy_, season_)), as.numeric) %>%
          tidyr::nest_legacy()
          
      }
      
      else {
      
        player_statistics <- page %>%
          rvest::html_node('[class="table table-striped table-condensed table-sortable player-stats highlight-stats"]') %>%
          rvest::html_table() %>%
          purrr::set_names("season_", "team_", "league_", "games_played_", "goals_", "assists_", "points_", "penalty_minutes_", "plus_minus_", "blank_", "playoffs_", "games_played_playoffs_", "goals_playoffs_", "assists_playoffs_", "points_playoffs_", "penalty_minutes_playoffs_", "plus_minus_playoffs_") %>%
          as_tibble() %>%    
          mutate_all(~na_if(., "-")) %>%      
          mutate(captaincy_ = stringr::str_split(team_, "\U201C", simplify = TRUE, n = 2)[,2]) %>%
          mutate(captaincy_ = stringr::str_split(captaincy_, "\U201D", simplify = TRUE, n = 2)[,1]) %>%
          mutate(team_ = stringr::str_split(team_, "\U201C", simplify = TRUE, n = 2)[,1]) %>%
          mutate(season_ = replace(season_, season_ == "", NA)) %>%
          tidyr::fill(season_) %>%
          mutate(season_short_ = as.numeric(stringr::str_split(season_, "-", simplify = TRUE, n = 2)[,1]) + 1) %>%
          mutate(birthday = vitals[["birthday"]]) %>%
          mutate(draft_eligibility_date_ = stringr::str_c(as.character(season_short_), "09-15", sep = "-")) %>%
          mutate(age_ = elite::get_years_difference(birthday, draft_eligibility_date_)) %>%
          mutate_all(stringr::str_squish) %>%
          mutate_all(as.character) %>%      
          mutate_all(~na_if(., "")) %>%
          mutate(goals_against_average_ = NA) %>%
          mutate(save_percentage_ = NA) %>%
          mutate(goals_against_average_playoffs_ = NA) %>%
          mutate(save_percentage_playoffs_ = NA) %>%
          select(-c(blank_, playoffs_, draft_eligibility_date_, birthday)) %>%
          select(team_, league_, captaincy_, season_, season_short_, age_, games_played_, goals_, assists_, points_, penalty_minutes_, plus_minus_, goals_against_average_, save_percentage_, games_played_playoffs_, goals_playoffs_, assists_playoffs_, points_playoffs_, penalty_minutes_playoffs_, plus_minus_playoffs_, goals_against_average_playoffs_, save_percentage_playoffs_) %>% 
          mutate_at(vars(c(team_, league_, captaincy_, season_)), as.character) %>%
          mutate_at(vars(-c(team_, league_, captaincy_, season_)), as.numeric) %>%
          tidyr::nest_legacy()
      
      }
      
      all_data <- vitals %>% 
        bind_cols(player_statistics) %>% 
        rename(player_statistics = data)
      
    }
    
    if (progress) {pb$tick()}
    
    return(all_data)
    
  }
  
  insistently_get_player_stats_individual <- purrr::insistently(.get_player_stats_individual, rate = purrr::rate_delay(pause = 0.1, max_times = 10))
  
  try_get_player_stats_individual <- function(player_url, name, ...) {
    
    tryCatch(insistently_get_player_stats_individual(player_url, name, ...), 
             
             error = function(e) {
               cat("\n\nThere's an error:\n\n", sep = "")
               print(e)
               cat("\nHere's where it's from:\n\nPlayer URL:\t", player_url, "\nName:\t", name, sep = "")
               cat("\n")
               tibble()},
             
             warning = function(w) {
               cat("\n\nThere's a warning:\n\n", sep = "")
               print(w)
               cat("\nHere's where it's from:\n\nPlayer URL:\t", player_url, "\nName:\t", name, sep = "")
               cat("\n")
               tibble()})
    
  }
  
  player_stats_individual <- purrr::pmap_dfr(..., try_get_player_stats_individual)

  mydata <- player_stats_individual %>% 
    bind_cols(...)
  
  if ("pick_number" %in% colnames(mydata)) {
    
    mydata <- mydata %>%
      mutate(draft_eligibility_date = stringr::str_c(draft_year, "09-15", sep = "-")) %>%
      mutate(age = elite::get_years_difference(birthday, draft_eligibility_date)) %>%
      select(draft_league, draft_year, pick_number, round, draft_team, name, position, shot_handedness, birth_place, birth_country, birthday, height, weight, age, player_url, name_, position_, player_url_, player_statistics) %>%
      mutate_at(vars(c(draft_league, draft_year, draft_team, name, position, shot_handedness, birth_place, birth_country, birthday, name_, position_, player_url_, player_url)), as.character) %>%
      mutate_at(vars(-c(draft_league, draft_year, draft_team, name, position, shot_handedness, birth_place, birth_country, birthday, name_, position_, player_url_, player_url, player_statistics)), as.numeric)
    
  }
  
  else if ("season" %in% colnames(mydata)) {
    
    mydata <- mydata %>%
      mutate(season_short = as.numeric(stringr::str_split(season, "-", simplify = TRUE, n = 2)[,1]) + 1) %>%
      mutate(draft_eligibility_date = stringr::str_c(as.character(season_short), "09-15", sep = "-")) %>%
      mutate(age = elite::get_years_difference(birthday, draft_eligibility_date)) %>%
      select(name, team, league, position, shot_handedness, birth_place, birth_country, birthday, height, weight, season, season_short, age, games_played, goals, assists, points, penalty_minutes, plus_minus, games_played_playoffs, goals_playoffs, assists_playoffs, points_playoffs, penalty_minutes_playoffs, plus_minus_playoffs, player_url, team_url, name_, position_, player_url_, player_statistics) %>%
      mutate_at(vars(c(name, team, league, position, shot_handedness, birth_place, birth_country, birthday, season, player_url, team_url, name_, position_, player_url_)), as.character) %>%
      mutate_at(vars(-c(name, team, league, position, shot_handedness, birth_place, birth_country, birthday, season, player_url, team_url, name_, position_, player_url_, player_statistics)), as.numeric)
    
  }
  
  else {
    
    mydata <- mydata %>%
      select(name, position = position_, shot_handedness, birth_place, birth_country, birthday, height, weight, player_url, name_, player_url_, player_statistics) %>%
      mutate_at(vars(c(name, position, shot_handedness, birth_place, birth_country, birthday, player_url, name_, player_url_)), as.character) %>%
      mutate_at(vars(-c(name, position, shot_handedness, birth_place, birth_country, birthday, player_url, name_, player_url_, player_statistics)), as.numeric)
    
  }
  
  if (strip_redundancy & "season" %in% colnames(mydata)) {
    
    mydata <- mydata %>% select(-c(name_, position_, player_url_))
    
    }
  
  else if (strip_redundancy & !c("season" %in% colnames(mydata))) {
    
    mydata <- mydata %>% select(-c(name_, player_url_))
    
    }
  
  cat("\n")
  
  return(mydata)
  
}
