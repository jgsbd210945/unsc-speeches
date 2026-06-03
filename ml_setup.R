source("main.R")
library(tidymodels)

vdem_work <- tibble(vdem) |>
  filter(year > 1975) |> # So I can do the lags correctly. This will be standardized later.
  select(!c(country_id, historical_date:COWcode, # Tracking info we don't need
            ends_with(c("codelow", "codehigh", "sd", "osp", "mean", "nr", "ord")), # non-measure stats
            starts_with("e_")) # Vaguely duplicates of existing stats.
  ) |>
  group_by(country_text_id) |>
  arrange(year) |> # Should already be the case, but just in case it's not
  mutate(
    across(
      (where(is.numeric) & !year), # All cols with scores, essentially.
      ~ . - lag(., 1), # diff. from previous year
      .names = "diff1_{.col}"
    ),
    #same idea for 3y diffs
    across(
      where(is.numeric) & !year & !starts_with("diff1"),
      ~ . - lag(., 3),
      .names = "diff3_{.col}"
    )
  ) |>
  ungroup() |>
  filter(year > 1990) |>
  mutate(
    across(
      starts_with("diff"),
      ~ replace_na(., 0) # Replacing NAs (i.e., new countries, etc.)
    )
  ) |>
  select(country_name:year, where(is.numeric))
vdem_work <- vdem_work[ , colSums(is.na(vdem_work))==0]
