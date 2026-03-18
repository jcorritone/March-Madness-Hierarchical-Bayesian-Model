fit_model_02b_adj_efficiency <- function(train_data) {

  model_data <- train_data %>%
    select(
      Outcome,
      SeedDiff,
      AdjNetEffDiff,
      AdjSOSDiff,
      AdjTempoDiff
    ) %>%
    drop_na()

  glm(
    Outcome ~ AdjNetEffDiff + I(AdjNetEffDiff^2),
    data = model_data,
    family = binomial(link = "logit"))
}
