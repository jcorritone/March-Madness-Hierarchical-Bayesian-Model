library(tidyverse)
library(BradleyTerry2)

build_team_strength_ratings_bt <- function(reg_season_results) {

  bt_ratings <- reg_season_results %>%
    group_split(Season) %>%
    map_dfr(function(season_df) {

      season <- unique(season_df$Season)
      teams <- sort(unique(c(season_df$WTeamID, season_df$LTeamID)))

      player1 <- season_df %>%
        transmute(
          team = factor(WTeamID, levels = teams),
          at.home = case_when(
            WLoc == "H" ~ 1,
            WLoc == "A" ~ 0,
            TRUE ~ 0))

      player2 <- season_df %>%
        transmute(
          team = factor(LTeamID, levels = teams),
          at.home = case_when(
            WLoc == "H" ~ 0,
            WLoc == "A" ~ 1,
            TRUE ~ 0))

      fit <- BradleyTerry2::BTm(
        outcome = cbind(rep(1, nrow(season_df)), rep(0, nrow(season_df))),
        player1 = player1,
        player2 = player2,
        formula = ~ team + at.home,
        id = "team",
        br = TRUE)

      BradleyTerry2::BTabilities(fit) %>%
        as.data.frame() %>%
        rownames_to_column("TeamID") %>%
        as_tibble() %>%
        
        transmute(
          Season = season,
          TeamID = as.integer(TeamID),
          BTRating = ability) %>%
        
        mutate(
          BTRating = BTRating - mean(BTRating, na.rm = TRUE))
    })

  return(bt_ratings)
}
