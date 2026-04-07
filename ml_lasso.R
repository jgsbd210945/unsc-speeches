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

top_features <- coef(cv_out, s = "lambda.1se")
top_features <- top_features[-1,]
top_features <- top_features[top_features != 0]
sort(top_features, decreasing = TRUE) |> head(10)
sort(top_features, decreasing = TRUE) |> tail(10)
