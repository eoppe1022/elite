# elite
This is a package for scraping hockey data from EliteProspects in a [fairly] tidy manner for the purpose of analysis

## Installation
You can install `elite` from github with:
````
# install.packages("devtools")
devtools::install_github("eoppe1022/elite")
````

To load the package:
````
library(elite)
````

## The Bare Bones
The `elite` package is built on 4 main functions:
- `get_drafts()`
- `get_teams()`
- `get_player_stats_team()`
- `get_player_stats_individual()`

These 4 functions work in conjunction with one another to allow the user to cleanly and quickly get data from EliteProspects.

### `get_drafts()`
This function allows the user to supply a draft type (NHL Entry Draft, CHL Import Draft, NWHL Draft, etc.) and a season to get draft information from EliteProspects.

````
library(elite)

# to get 2018 nhl draft data
get_drafts("nhl entry draft", "2018")

# to get 2005, 2012, & 2018 nhl and chl import draft data
get_drafts(c("nhl entry draft", "chl import draft"), c("2005", "2012", "2018"))
````

### `get_teams()`
Similarly to `get_drafts()`, this function allows the user to supply a league (NHL, SHL, NCAA III, etc.) and a season to get team names and URLs from EliteProspects. Note that seasons must be in the `YYYY-YYYY` format.

````
library(elite)

# to get 2018 shl teams & team urls
get_teams("shl", "2017-2018")

# to get 1999 and 2014, shl and nhl teams & team urls
get_teams(c("shl", "nhl"), c("1998-1999", "2013-2014"))
````

### `get_player_stats_team()`
This function works in conjunction to `get_teams()`, as it returns the players' stats for each supplied team.

````
library(elite)
library(dplyr)

# get every team & team url for the ohl in 2018
teams <- get_teams("ohl", "2017-2018")

# get every players' stats for the ohl in 2018
get_player_stats_team(teams)

# the same thing as above but for the whl and for the past 2 seasons
get_teams("whl", c("2017-2018", "2016-2017")) %>%
  get_player_stats_team()
````

### `get_player_stats_individual()`
This function works in conjunction with both `get_player_stats_team()` and `get_drafts()`, as it provides the players' career statistics and bio. information (age, height, etc.) for a supplied name and player URL. This data is returned in a variable named `player_statistics` &mdash; a list variable in which each player's career statistics are in a nested tibble. You can use `tidyr::unnest()` to lengthen the data-frame and see the players' career statistics. 

````
library(elite)
library(dplyr)
library(tidyr)

# get nhl draft data from 2018
draft <- get_drafts("nhl", "2018")

# get all the drafted players' career stats and bio. info
stats <- get_player_stats_individual(draft)

# see unnested data frame
stats %>% tidyr::unnest(player_statistics)

# can also be used for season data
# same example as earlier
stats <- get_teams("whl", c("2017-2018", "2016-2017")) %>%
  get_player_stats_team() %>%
  get_player_stats_individual()
  
stats %>% tidyr::unnest(player_statistics)
````

## A Quick (but Important) Note on Patience
EliteProspects is an invaluable resource to the hockey community. By creating this package, I &mdash; by no means &mdash; aim to compete with EliteProspects in any way. The purpose of this package is solely to provide a medium to easily analyze and model data from EliteProspects. Thus, to make sure that no harm comes to EliteProspects from my making of this package, I set a `Sys.sleep()` of 30-35 seconds between each scrape. This means that between each scrape, there will be a mandatory wait time (that I already coded into my functions) of 30-35 seconds so that EliteProspects' servers don't get overloaded with requests. This is important, as &mdash; without this measure &mdash; it's possible that EliteProspects could block the user's IP address or take some other form of action. This all means that some scrapes can take hours or even days long, depdending on what you want to scrape. So, I ask that you be patient with your scrapes and that you view this as more of a delivery service that takes a while than some sort of immediate action.

## Final Notes
This is my first R package, so please be kind. Report any issues [here](https://github.com/eoppe1022/elite/issues), and I'll do the best that I can to solve them. I'd welcome any Pull Requests too, if you have any changes in mind.

You can follow and message me on [Twitter](http://www.twitter.com/OppenheimerEvan). Feel free to e-mail me at eoppe1022 (at) gmail.com with any questions.

Enjoy!
