library(tidyverse)
library(vdemdata)

meetings <- read_tsv("Data/meetings.tsv") |> filter(year > 1990)
speeches <- read_tsv("Data/speeches.tsv") |> filter(year > 1990)
scmakeup <- read_tsv("Data/SC Makeup.tsv") |> mutate(is_SC = TRUE)

wvdem <- tibble(vdem) |>
  filter(year > 1985) |> # So I can do the lags correctly. This will be standardized later.
  group_by(country_text_id) |>
  arrange(year) |> # Should already be the case, but just in case it's not
  mutate(diff_polyarchy = v2x_polyarchy - lag(v2x_polyarchy)) |>
  # Backsliding boolean variable. I'm quantitatively defining it as having decreased in electoral democracy score by more than .005 three years in a row.
  # I also filtered so that the regime has to be at least some semblance of an electoral autocracy (at least) so that hard autocracies getting more autocratic aren't included.
  mutate(backslided = (diff_polyarchy < -0.005) & (v2x_regime_amb) > 3) |>
  # Did it do it at least three years in a row?
  mutate(backslided = (backslided & lag(backslided) & lag(lag(backslided))) |
           lead(backslided) & backslided & lag(backslided) |
           lead(lead(backslided)) & lead(backslided) & backslided) |>
  ungroup() |>
  filter(year > 1990) |>
  arrange(country_text_id) |>
  select(country_name, country_text_id, year, v2x_polyarchy, v2x_libdem, diff_polyarchy, backslided)

mgdata <- merge(wvdem, scmakeup, by = c('year', 'country_name'), all.x = TRUE) |>
  as_tibble()
mgdata$is_SC <- ifelse(is.na(mgdata$is_SC), FALSE, TRUE)
