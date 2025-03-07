library(tidyverse)
library(vdemdata)

meetings <- read_tsv("Data/meetings.tsv") |> filter(year > 1990)
speeches <- read_tsv("Data/speeches.tsv") |> filter(year > 1990)
scmakeup <- read_tsv("Data/SC Makeup.tsv") |> mutate(is_SC = TRUE)

wvdem <- tibble(vdem) |>
  filter(year > 1975) |> # So I can do the lags correctly. This will be standardized later.
  group_by(country_text_id) |>
  arrange(year) |> # Should already be the case, but just in case it's not
  mutate(diff_polyarchy = v2x_polyarchy - lag(v2x_polyarchy)) |>
  # Backsliding boolean variable. I'm quantitatively defining it as having decreased in electoral democracy score by more than .005 three years in a row.
  # I also filtered so that the regime has to be at least some semblance of an electoral autocracy (at least) so that hard autocracies getting more autocratic aren't included.
  mutate(backslided = (diff_polyarchy < 0.005) &
           (v2x_regime_amb > 2) &
           (lag(lag(v2x_polyarchy)) - v2x_polyarchy > 0.03)) |>
  # Filling it out
  mutate(backslided = (diff_polyarchy < -0.001) &
           (backslided |
              (lag(backslided) & lag(backslided, 2))|
              lead(backslided) & lag(backslided) |
              lead(backslided, 2) & lead(backslided))) |>
  ungroup() |>
  filter(year > 1990) |>
  arrange(country_text_id) |>
  select(country_name, country_text_id, year, v2x_polyarchy, v2x_libdem, v2x_regime_amb, diff_polyarchy, backslided)

mgdata <- merge(wvdem, scmakeup, by = c('year', 'country_name'), all.x = TRUE) |>
  as_tibble()
mgdata$is_SC <- ifelse(is.na(mgdata$is_SC), FALSE, TRUE)



mgdata |> filter(is_SC, backslided)
backslide_erode <- mgdata |> filter(is_SC, backslided, v2x_regime_amb > 4)
backslide_revert <- mgdata |> filter(is_SC, backslided, v2x_regime_amb < 5)

entrenched_illib <- mgdata |> filter(is_SC, !backslided, between(v2x_regime_amb, 4, 6))
entrenched_auto <- mgdata |> filter(is_SC, !backslided, v2x_regime_amb < 4)

