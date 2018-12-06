#' Gets draft information, players' names, and player URLs for specified draft and season
#' 
#' Returns a data frame of draft information, players' names, and player URLs for user supplied drafts (NHL Entry Draft, CHL Import Draft, etc.) & seasons.
#' 
#' @param .draft_type The type of draft for which the user wants to scrape data. Draft types must be typed exactly as they are found on EliteProspects (though case doesn't matter). Draft types include -- but are not limited to -- NHL Entry Draft, NHL Expansion Draft, KHL Draft, NWHL Draft, CWHL Draft, CHL Import Draft, and OHL U18 Priority Selection. Others may be found at the bottom of the page at \url{https://www.eliteprospects.com/draft/nhl-entry-draft}.
#' @param .draft_year Seasons for which the user wants to scrape data. Must be of the form \code{2018}, \code{1996}, etc -- only  a single 4-digit number.
#' @param .progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @param ... Allows the user to supply other information to the function. If you don't know what this means, then don't worry about it.
#' @examples 
#' get_drafts("chl import draft", "2018")
#' 
#' get_drafts(c("nhl entry draft", "khl draft"), c("2018", "2017", "1994"))
#' 
#' @export
#' @import dplyr
#' 
get_drafts <- function(.draft_type, .draft_year, .progress = TRUE, ...) {
  
  draft_types <- .draft_type %>% 
    as_tibble() %>% 
    purrr::set_names(".draft_type") %>% 
    mutate(.draft_type = stringr::str_replace_all(.draft_type, " ", "-"))
  
  draft_years <- .draft_year %>%
    as_tibble() %>%
    purrr::set_names(".draft_year")
  
  mydata <- tidyr::crossing(draft_types, draft_years)
  
  if (.progress) {
    
    pb <- progress::progress_bar$new(format = "get_drafts() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    pb$tick(0)}
  
  .get_drafts <- function(.draft_type, .draft_year, ...) {
    
    if (nrow(mydata) > 1) {
    
      seq(20, 25, by = 0.001) %>%
        sample(1) %>%
        Sys.sleep()
      
    }

    page <- stringr::str_c("https://www.eliteprospects.com/draft/", .draft_type, "/", .draft_year) %>% xml2::read_html()
    
    draft_league <- page %>%
      rvest::html_nodes(".plytitle") %>%
      rvest::html_text() %>%
      stringr::str_replace("[0-9]{4,4}", "") %>%
      stringr::str_squish()
    
    draft_year <- .draft_year
    
    draft_pick_info <- page %>%
      rvest::html_nodes("#drafted-players td:nth-child(1)") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      as_tibble() %>%
      mutate(round = ifelse(stringr::str_detect(value, "ROUND"), value, NA)) %>%
      tidyr::fill(round) %>%
      filter(!stringr::str_detect(value, "ROUND")) %>%
      mutate(value = stringr::str_replace(value, "#", "")) %>%
      mutate(round = stringr::str_replace(round, "ROUND", "")) %>%
      mutate_all(stringr::str_squish) %>%
      rename(round = round, pick_number = value)
    
    draft_team <- page %>%
      rvest::html_nodes(".team a") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      as_tibble() %>%
      purrr::set_names("draft_team")
    
    player_info <- page %>%
      rvest::html_nodes("#drafted-players .txt-blue a") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      as_tibble() %>%
      mutate(position = stringr::str_split(value, "\\(", simplify = TRUE, n = 2)[,2]) %>%
      mutate(position = stringr::str_split(position, "\\)", simplify = TRUE, n = 2)[,1]) %>%
      mutate(name = stringr::str_split(value, "\\(", simplify = TRUE, n = 2)[,1]) %>%
      mutate_all(stringr::str_squish)
    
    player_url <- page %>%
      rvest::html_nodes("#drafted-players .txt-blue a") %>%
      rvest::html_attr("href") %>%
      as_tibble() %>%
      purrr::set_names("player_url")
    
    player_names_with_no_selection <- page %>%
      rvest::html_nodes("#drafted-players td.player") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      as_tibble() %>%
      purrr::set_names("value")

    no_selection_info <- player_names_with_no_selection %>%
      bind_cols(draft_pick_info) %>%
      bind_cols(draft_team) %>%
      anti_join(player_info, by = c("value" = "value"))
      
    everything_but_no_selection_info <- draft_pick_info %>%
      bind_cols(draft_team) %>%
      anti_join(no_selection_info, by = c("pick_number" = "pick_number", "round" = "round")) %>%
      bind_cols(player_info) %>%
      bind_cols(player_url)
      
    all_data <- everything_but_no_selection_info %>%
      bind_rows(no_selection_info) %>%
      mutate(draft_league = draft_league) %>%
      mutate(draft_year = draft_year) %>%
      mutate_at(vars(pick_number, round), as.numeric) %>%
      select(-c(value)) %>%
      select(draft_league, draft_year, pick_number, round, draft_team, name, position, player_url) %>%
      arrange(pick_number)
    
    if (.progress) {pb$tick()}
    
    return(all_data)}
    
  draft_data <- purrr::map2_dfr(mydata[[".draft_type"]], mydata[[".draft_year"]], elite::persistently(.get_drafts, max_attempts = 10))
    
  return(draft_data)
  
}
