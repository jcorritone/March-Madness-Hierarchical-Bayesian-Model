fit_model_02_efficiency <- function(train_data) {

  model_data <- train_data %>%
    select(
      Outcome,
      SeedDiff,
      NetEffDiff,
      OffEffDiff,
      DefEffDiff
    ) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + NetEffDiff + OffEffDiff + DefEffDiff,
    data = model_data,
    family = binomial(link = "logit")
  )
}
