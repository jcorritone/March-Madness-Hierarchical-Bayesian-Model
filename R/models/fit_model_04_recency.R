fit_model_04_recency <- function(train_data_recency) {

  model_data <- train_data_recency %>%
    select(
      Outcome,
      SeedDiff,
      RecencyOffEffDiff,
      RecencyDefEffDiff,
      RecencySOSDiff,
      TrendNetEffDiff
    ) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + RecencyOffEffDiff + RecencyDefEffDiff + RecencySOSDiff + TrendNetEffDiff,
    data = model_data,
    family = binomial(link = "logit")
  )
}
