library(tidyverse)

build_team_season_features <- function(team_games) {

  team_games %>%
    group_by(Season, TeamID) %>%
    summarise(
      GamesPlayed = n(),
      Wins = sum(Win),
      WinPct = mean(Win),
      AvgTeamScore = mean(TeamScore),
      AvgOppScore = mean(OppScore),
      AvgPointDiff = mean(PointDiff),
      .groups = "drop"
    ) %>%
    arrange(Season, TeamID)
}
