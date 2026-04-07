source("main.R")
library(tidymodels)

vdem_work <- tibble(vdem) |>
  filter(year > 1975) |> # So I can do the lags correctly. This will be standardized later.
  group_by(country_text_id) |>
  arrange(year) |> # Should already be the case, but just in case it's not
  mutate(diff_polyarchy = v2x_polyarchy - lag(v2x_polyarchy)) |>
  ungroup() |>
  mutate(diff_polyarchy = replace_na(diff_polyarchy, 0)) |>
  filter(year > 1990) |>
  select(!c(country_id, historical_date:COWcode, # Tracking info we don't need
            ends_with(c("codelow", "codehigh", "sd", "osp", "mean", "nr", "ord"))) # non-measure stats
  ) |>
  select(country_name:year, where(is.numeric))
vdem_work <- vdem_work[ , colSums(is.na(vdem_work))==0]