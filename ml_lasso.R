source("ml_class.R")
library(glmnet)

db_preds <- final_db |> filter(is.na(backslided))

grid <- 10^seq(10, -2, length = 100)
y <- db_preds$.pred_TRUE
db_lasso <- dplyr::select(db_preds, matches("v2"))

# Lasso
glmnet(data.matrix(db_lasso), y, alpha = 1, lambda = grid) |> plot()

set.seed(1)
cv_out <- cv.glmnet(data.matrix(db_lasso), y, alpha = 1) 
plot(cv_out)

top_1se <- coef(cv_out, s = "lambda.1se")
top_1se <- top_1se[-1,]
top_1se <- top_1se[top_1se != 0]
sort(top_1se, decreasing = TRUE) |> head(10)
sort(top_1se, decreasing = TRUE) |> tail(10)

top_min <- coef(cv_out, s = "lambda.min")
top_min <- top_min[-1,]
top_min <- top_min[top_min != 0]
sort(top_min, decreasing = TRUE) |> head(10)
sort(top_min, decreasing = TRUE) |> tail(10)



mainwf <- function(data, out_file){
  labeled <- data |>
    filter(!is.na(backslided))|>
    mutate(backslided = factor(backslided, levels = c("TRUE", "FALSE")))
  unlabeled <- data |>
    filter(is.na(backslided))
  
  set.seed(1)
  split <- initial_split(labeled, prop = 0.8, strata = backslided)
  train <- training(split)
  test <- testing(split)
  
  rec <- recipe(backslided ~ ., data = train) |>
    update_role(country_name, year, country_text_id, new_role = "id") |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
  
  rf_spec <- rand_forest(trees = 1000) |>
    set_engine("ranger") |>
    set_mode("classification")
  
  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(rf_spec)
  
  df_fit <- wf |>
    fit(data = train)
  
  results <- test |>
    select(backslided) |>
    bind_cols(predict(df_fit, test)) |>
    bind_cols(predict(df_fit, test, type = "prob"))
  
  results |> metrics(truth = backslided, estimate = .pred_class)
  results |> roc_auc(truth = backslided, .pred_TRUE)
  
  predicted = augment(df_fit, new_data = unlabeled)
  
  final_df <- labeled |>
    mutate(
      label_source = "qualitative_manual",
      final_backslided = backslided
    ) |>
    bind_rows(
      predicted |>
        mutate(
          label_source = "algorithmic_rf",
          final_backslided = .pred_class
        )
    ) |>
    select(country_name, country_text_id, year, final_backslided, label_source, .pred_TRUE, everything()) |>
    arrange(country_name, year) |>
    mutate(final_backslided = as.logical(final_backslided)) |> # un-factorizing it
    left_join(mgwreg, by = c("country_name", "country_text_id", "year")) |>
    mutate(
      regime = case_when(
        (final_backslided & v2x_regime_amb > 4) ~ "backslide_erode",
        (final_backslided & between(v2x_regime_amb, 3, 4)) ~ "backslide_revert",
        (!final_backslided & v2x_regime_amb > 6) ~ "entrenched_dem",
        (!final_backslided & between(v2x_regime_amb, 4, 6)) ~ "entrenched_illib",
        (!final_backslided & v2x_regime_amb < 4) ~ "entrenched_auto",
        (v2x_regime_amb < 3) ~ "entrenched_auto"
      )) |>
    filter(!is.na(regime))
  
  final_df |>
    filter(final_backslided) |>
    select(country_name, year, final_backslided, .pred_TRUE, v2x_polyarchy, diff_polyarchy) |>
    write_csv(out_file)
  
  final_df
}

#trying top_min first
lasso_min <- names(top_min)
data_lsmin <- model_data |>
  select(country_name, year, country_text_id,, backslided, all_of(lasso_min))
min_df <- mainwf(data_lsmin, "testmin.csv")

lasso_1se <- names(top_1se)
data_1se <- model_data |> 
  select(country_name, year, country_text_id,, backslided, all_of(lasso_1se))
df_1se <- mainwf(data_1se, "test1se.csv")

df_1se |>
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
