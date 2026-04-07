source("ml_class.R")

library(xgboost)
library(randomForest)
library(vip)

## XGBoosting
rec <- recipe(backslided ~ ., data = train_data) |>
  step_select(all_numeric(), all_outcomes(), skip = TRUE) |>
  update_role(diff_polyarchy, new_role = "ID")

cv_folds <- vfold_cv(train_data, v = 5, repeats = 1) # Cross-validation

model_xgboost <- boost_tree(trees = 100, mtry=5) |> 
  set_mode("classification") |> 
  set_engine("xgboost")

wf_set <- workflow_set(
  preproc = list(rec),
  models = list(
    xgboost = model_xgboost)
)

wf_set_fitted <- workflow_map(wf_set, "fit_resamples", resamples = cv_folds)
wf_set_fitted |> collect_metrics()

xg_grid <- grid_regular(learn_rate(), levels = 5)

model_xgboost_tune <- boost_tree(trees = 500, mtry=5, learn_rate = tune()) |> 
  set_mode("classification") |> 
  set_engine("xgboost")

wf <- workflow() |>
  add_model(model_xgboost_tune) |>
  add_recipe(rec)

model_res <- wf |>
  tune_grid(resamples = cv_folds,
            grid = xg_grid,
            control = control_grid(save_pred = TRUE))
collect_metrics(model_res)

best_tree <- model_res |> select_best(metric = "accuracy")
final_wf <- wf |> finalize_workflow(best_tree)
final_fit <- final_wf |> last_fit(data_split)
final_fit |> collect_metrics()

trained_workflow <- extract_workflow(final_fit)
final_class <- predict(trained_workflow, new_data = unlabeled_df, type = "class") # predicting unlabeled data, now that I have the XGBoost
final_prob <- predict(trained_workflow, new_data = unlabeled_df, type = "prob")

final_preds <- unlabeled_df |>
  bind_cols(final_class, final_prob)

final_preds |>
  select(country_text_id, year, .pred_class, .pred_TRUE) |>
  filter(.pred_class == TRUE)