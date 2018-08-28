# elite
A package for scraping hockey data from EliteProspects in a [fairly] tidy manner.

## Installation
You can install `elite` from github with:
```
# install.packages("devtools")
devtools::install_github("eoppe1022/elite")
```

To load the package:
```
library(elite)
```

## The Bare Bones
The `elite` package is built on 4 functions:
- `get_drafts()`
- `get_teams()`
- `get_player_stats_team()`
- `get_player_stats_individual()`. 

These 4 functions work in conjunction with one another to allow the user to cleanly and quickly get data from EliteProspects.

### `get_drafts()`

