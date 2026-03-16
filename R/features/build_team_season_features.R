library(tidyverse)

build_team_season_features <- function(team_games) {

  team_games %>%
    mutate(
      TeamPossessions = TeamFGA - TeamOR + TeamTO + 0.44 * TeamFTA,
      OppPossessions = OppFGA - OppOR + OppTO + 0.44 * OppFTA,
      AdjTeamPossessions = TeamPossessions * (40 / (40 + 5 * NumOT)),
      AdjOppPossessions = OppPossessions * (40 / (40 + 5 * NumOT)),
      OffEff = TeamScore / TeamPossessions,
      DefEff = OppScore / OppPossessions,
      NetEff = OffEff - DefEff) %>%
  
    group_by(Season, TeamID) %>%
    summarise(
      GamesPlayed = n(),
      Wins = sum(Win),
      WinPct = mean(Win),
      AvgTeamScore = mean(TeamScore),
      AvgOppScore = mean(OppScore),
      AvgPointDiff = mean(PointDiff),
      AvgPossessions = mean(TeamPossessions),
      OffEff = mean(OffEff),
      DefEff = mean(DefEff),
      NetEff = mean(NetEff),
      .groups = "drop") %>%
  
    arrange(Season, TeamID)
}
