### This is the main document I should be running every time for setup.
### It makes mgwreg, which is the vdem data merged with sc makeup data and can be used for further analysis.

library(tidyverse)
library(vdemdata)
library(janitor)
library(countrycode)
library(tm)
library(wrMisc)
library(tidytext)

# Loading/Cleaning Data

meetings <- read_tsv("Data/meetings.tsv") |> filter(year > 1990)
scmakeup <- read_tsv("Data/SC Makeup.tsv") |> mutate(is_SC = TRUE)

wvdem <- tibble(vdem) |>
  filter(year > 1975) |> # So I can do the lags correctly. This will be standardized later.
  group_by(country_text_id) |>
  arrange(year) |> # Should already be the case, but just in case it's not
  mutate(diff_polyarchy = v2x_polyarchy - lag(v2x_polyarchy)) |>
  # Backsliding boolean variable. I'm quantitatively defining it as having decreased in electoral democracy score by more than .005 and having decreased by at least 0.03 in the last two years.
  # I also filtered so that the regime has to be at least some semblance of an electoral autocracy (at least) so that hard autocracies getting more autocratic aren't included.
  mutate(backslided = (v2x_regime_amb > 2) &
           (diff_polyarchy < -0.005) &
           (lag(v2x_polyarchy, 2) - v2x_polyarchy > 0.03)) |>
  # Filling it out
  mutate(backslided = (diff_polyarchy < -0.001) &
           (backslided |
              ((lag(backslided) & lag(backslided, 2))|
              lead(backslided) & lag(backslided) |
              lead(backslided, 2) & lead(backslided)))) |>
  #handling NAs - for 2024's data.
  mutate(backslided = ifelse(is.na(backslided),
                             ((diff_polyarchy < -0.01) & lag(backslided)),
                             backslided)) |>
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

entrenched_dem <- mgdata |> filter(is_SC, !backslided, v2x_regime_amb > 6)
entrenched_illib <- mgdata |> filter(is_SC, !backslided, between(v2x_regime_amb, 4, 6))
entrenched_auto <- mgdata |> filter(is_SC, !backslided, v2x_regime_amb < 4)

mgwreg <- mgdata |> mutate(
  regime = case_when(
    (backslided & v2x_regime_amb > 4) ~ "backslide_erode",
    (backslided & v2x_regime_amb < 5) ~ "backslide_revert",
    (!backslided & v2x_regime_amb > 6) ~ "entrenched_dem",
    (!backslided & between(v2x_regime_amb, 4, 6)) ~ "entrenched_illib",
    (!backslided & v2x_regime_amb < 4) ~ "entrenched_auto"
  )) |>
  filter(!is.na(regime))

mgwreg$bve <- ifelse(grepl("backslide_", mgwreg$regime), "backsliding",
                     ifelse(grepl("dem", mgwreg$regime), "democratic",
                            "entrenched"))

clean_nas <- function(df) {
  rmna <- apply(df, 2, \(col) sum(!is.na(col)))
  rmna <- rmna == 0
  df[, !rmna]
}


sc_regime <- mgwreg |>
  filter(is_SC) |>
  select(year, country_name, regime)

findState <- function(abbv, low = 1991, high = 2024) {
  mgwreg |>
    filter(country_text_id == abbv, between(year, low, high)) |>
    select(country_name, year, regime) |>
    print(n = 50)
}

# Using a standardized colorblind-friendly color scheme.
color_scheme <- c("#CC7987", "#800080", "#D55E00", "#0072B2", "#999999", "#009E73", "#56B4E9", "#000000", "#F0E442")

## Full classification by state
mgwreg |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy, color = regime)) +
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

mgwreg |> ggplot(aes(x = year, y = v2x_polyarchy)) +
  geom_smooth(color = "black") +
  ylab("Average Electoral Democracy Score") +
  xlab("Year")

mgwreg |> ggplot(aes(x = year, fill = factor(v2x_regime_amb))) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values = c("#A80000", "#BC0F0F", "#BF4242", "#C16880", "#999999", "#748993", "#78A6BF", "#0072B2", "#0059A8", "#004593"),
                    labels = c("Closed Autocracy",
                               "Closed Autocracy Upper Bound",
                               "Electoral Autocracy Lower Bound",
                               "Electoral Autocracy",
                               "Electoral Autocracy Upper Bound",
                               "Electoral Democracy Lower Bound",
                               "Electoral Democracy",
                               "Electoral Democracy Upper Bound",
                               "Liberal Democracy Lower Bound",
                               "Liberal Democracy")) +
  labs(fill = "Regime") +
  xlab("Year") +
  ylab("Proportion of All States")

mgwreg |> ggplot(aes(x = year, fill = regime)) +
  geom_bar(position = 'fill')
