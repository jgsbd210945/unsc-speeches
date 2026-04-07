source("ml_setup.R")

model_data <- vdem_work |>
  left_join(init_vals, by = c("year", "country_name")) |>
  mutate(
    backslided = case_when(
      backslided == TRUE ~ TRUE, 
      
      (v2x_polyarchy >= lag(v2x_polyarchy, 1)) & 
        (v2x_polyarchy >= lag(v2x_polyarchy, 2)) &
        (v2x_regime_amb >= 8) ~ FALSE, # Stable democracies,
      
      (v2x_regime_amb <= 1) ~ FALSE, #Stable autocracies
      
      TRUE ~ NA
    )
  )

labeled_df <- model_data |>
  filter(!is.na(backslided)) |>
  mutate(backslided = factor(backslided, levels = c("TRUE", "FALSE")))

unlabeled_df <- model_data |>
  filter(is.na(backslided))

set.seed(1)
data_split <- initial_split(labeled_df, prop = 0.8, strata = backslided)
train_data <- training(data_split)
test_data <- testing(data_split)

db_rec <- recipe(backslided ~ ., data = train_data) |>
  update_role(country_name, year, country_text_id, new_role = "id") |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

rf_spec <- rand_forest(trees = 1000) |>
  set_engine("ranger") |>
  set_mode("classification")

db_wf <- workflow() |>
  add_recipe(db_rec) |>
  add_model(rf_spec)

db_fit <- db_wf |>
  fit(data = train_data)

results <- test_data |>
  select(backslided) |>
  bind_cols(predict(db_fit, test_data)) |>
  bind_cols(predict(db_fit, test_data, type = "prob"))

results |> metrics(truth = backslided, estimate = .pred_class)
results |> roc_auc(truth = backslided, .pred_TRUE)
# accuracy = .993; roc_auc = 1

# onto unlabeled data

predicted_df = augment(db_fit, new_data = unlabeled_df)

full_db <- labeled_df |>
  mutate(
    label_source = "qualitative_manual",
    final_backslided = backslided
  ) |>
  bind_rows(
    predicted_df |>
      mutate(
        label_source = "algorithmic_rf",
        final_backslided = .pred_class
      )
  ) |>
  select(country_name, country_text_id, year, final_backslided, label_source, .pred_TRUE, everything()) |>
  arrange(country_name, year) |>
  mutate(final_backslided = as.logical(final_backslided)) # un-factorizing it

final_db <- full_db |> mutate(
  regime = case_when(
    (final_backslided & v2x_regime_amb > 4) ~ "backslide_erode",
    (final_backslided & between(v2x_regime_amb, 3, 4)) ~ "backslide_revert",
    (!final_backslided & v2x_regime_amb > 6) ~ "entrenched_dem",
    (!final_backslided & between(v2x_regime_amb, 4, 6)) ~ "entrenched_illib",
    (!final_backslided & v2x_regime_amb < 4) ~ "entrenched_auto",
    (v2x_regime_amb < 3) ~ "entrenched_auto"
  )) |>
  filter(!is.na(regime))

final_db |>
  ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy, color = regime)) +
  geom_point(alpha = 0.75) +
  labs(color = "Regime") +
  scale_color_manual(values = color_scheme,
                     labels = c("Democratic Erosion", 
                                "Democratic Reversion",
                                "Entrenched Autocracy",
                                "Entrenched Democracy",
                                "Grey Area Regime")) +
  ylab("Electoral Democracy Score") +
  xlab("Difference in Electoral Democracy Score")

mgwreg <- final_db |>
  select(country_name, country_text_id, year, v2x_polyarchy, v2x_regime_amb, diff_polyarchy, backslided, regime)

mgwreg$bve <- ifelse(grepl("backslide_", mgwreg$regime), "backsliding",
                     ifelse(grepl("dem", mgwreg$regime), "democratic",
                            "entrenched"))
