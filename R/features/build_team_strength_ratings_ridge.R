library(tidyverse)
library(Matrix)
library(glmnet)

build_team_strength_ratings_ridge <- function(team_games) {

  ridge_ratings <- team_games %>%
    group_split(Season) %>%

    map_dfr(function(season_df) {

      season <- unique(season_df$Season)
      teams <- sort(unique(c(season_df$TeamID, season_df$OpponentID)))
      n_teams <- length(teams)
      n_games <- nrow(season_df)
      team_index <- setNames(seq_along(teams), teams)

      y <- season_df$PointDiff

      home_court <- season_df %>%
        mutate(
          HomeCourt = case_when(
            Location == "H" ~ 1,
            Location == "A" ~ -1,
            TRUE ~ 0)
        ) %>%
        pull(HomeCourt)

      X_team <- Matrix::sparseMatrix(
        i = c(seq_len(n_games), seq_len(n_games)),
        j = c(
          team_index[as.character(season_df$TeamID)],
          team_index[as.character(season_df$OpponentID)]
        ),
        x = c(rep(1, n_games), rep(-1, n_games)),
        dims = c(n_games, n_teams),
        dimnames = list(NULL, paste0("Team_", teams)))

      X <- cbind(X_team, HomeCourt = home_court)

      cv_fit <- glmnet::cv.glmnet(
        x = X,
        y = y,
        alpha = 0,
        family = "gaussian",
        intercept = TRUE,
        standardize = FALSE)

      coef_mat <- as.matrix(coef(cv_fit, s = "lambda.min"))
      coef_df <- tibble(
        Term = rownames(coef_mat),
        Estimate = as.numeric(coef_mat[, 1]))

      coef_df %>%
        filter(str_detect(Term, "^Team_")) %>%
        
        mutate(
          TeamID = as.integer(str_remove(Term, "^Team_")),
          RidgeRating = Estimate) %>%
      
        select(TeamID, RidgeRating) %>%
      
        mutate(
          RidgeRating = RidgeRating - mean(RidgeRating),
          Season = season) %>%
      
        select(Season, TeamID, RidgeRating)
    })

  return(ridge_ratings)
}
  
