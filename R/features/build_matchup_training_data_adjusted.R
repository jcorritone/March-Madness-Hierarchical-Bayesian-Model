build_matchup_training_data_adjusted <- function(tourney_results, team_season_features, seeds, team_strength_ratings_ridge = NULL) {

  valid_seasons <- sort(unique(team_season_features$Season))

  matchup_base <- tourney_results %>%
    filter(Season %in% valid_seasons) %>%
    transmute(
      Season,
      Team1 = pmin(WTeamID, LTeamID),
      Team2 = pmax(WTeamID, LTeamID),
      Outcome = if_else(WTeamID < LTeamID, 1, 0))

  team1_features <- team_season_features %>%
    rename_with(~ paste0("Team1_", .x), .cols = -Season)

  team2_features <- team_season_features %>%
    rename_with(~ paste0("Team2_", .x), .cols = -Season)

  seeds_clean <- seeds %>%
    filter(Season %in% valid_seasons) %>%
    mutate(SeedNum = readr::parse_number(Seed)) %>%
    select(Season, TeamID, Seed, SeedNum)

  team1_seeds <- seeds_clean %>%
    rename(
      Team1 = TeamID,
      Team1_Seed = Seed,
      Team1_SeedNum = SeedNum)

  team2_seeds <- seeds_clean %>%
    rename(
      Team2 = TeamID,
      Team2_Seed = Seed,
      Team2_SeedNum = SeedNum)

  matchup_training_data <- matchup_base %>%
    left_join(team1_features, by = c("Season", "Team1" = "Team1_TeamID")) %>%
    left_join(team2_features, by = c("Season", "Team2" = "Team2_TeamID")) %>%
    left_join(team1_seeds, by = c("Season", "Team1")) %>%
    left_join(team2_seeds, by = c("Season", "Team2")) %>%
    
  mutate(
      SeedDiff = Team1_SeedNum - Team2_SeedNum,

      AdjNetEffDiff = Team1_AdjNetEff - Team2_AdjNetEff,
      AdjSOSDiff = Team1_AdjSOS - Team2_AdjSOS,
      AdjTempoDiff = Team1_AdjTempo - Team2_AdjTempo,

      AdjOffEffDiff = Team1_AdjOffEff - Team2_AdjOffEff,
      AdjDefEffDiff = Team1_AdjDefEff - Team2_AdjDefEff)

  if (!is.null(team_strength_ratings_ridge)) {

    team1_ridge <- team_strength_ratings_ridge %>%
      rename(
        Team1 = TeamID,
        Team1_RidgeRating = RidgeRating)

    team2_ridge <- team_strength_ratings_ridge %>%
      rename(
        Team2 = TeamID,
        Team2_RidgeRating = RidgeRating)

    matchup_training_data <- matchup_training_data %>%
      left_join(team1_ridge, by = c("Season", "Team1")) %>%
      left_join(team2_ridge, by = c("Season", "Team2")) %>%
      mutate(
        RidgeRatingDiff = Team1_RidgeRating - Team2_RidgeRating)
  }

  matchup_training_data %>%
    arrange(Season, Team1, Team2)
}
