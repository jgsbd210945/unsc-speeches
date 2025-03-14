library(tidyverse)
library(vdemdata)
library(janitor)
library(countrycode)

# Loading/Cleaning Data

meetings <- read_tsv("Data/meetings.tsv") |> filter(year > 1990)
speeches <- read_tsv("Data/speeches.tsv") |> filter(year > 1990)
scmakeup <- read_tsv("Data/SC Makeup.tsv") |> mutate(is_SC = TRUE)
voting <- read_tsv("Data/UNSC Voting.tsv")  |>
# Multiple cols for Bolivia and Venezuela due to gov't changes/UN Naming conventions.
  mutate(Bolivia = coalesce(Bolivia...134, Bolivia...142),
         Venezuela = coalesce(Venezuela...25, Venezuela...145)) |>
  select(-c(Bolivia...134, Bolivia...142, Venezuela...25, Venezuela...145)) |>
  clean_names() |>
  mutate(meeting_date = as.Date(meeting_date, format = "%m/%d/%y")) |>
  rename(KOR = "south_korea")

# Converting to country codes for easier matching
colnames(voting) <- ifelse(is.na(countrycode(colnames(voting), origin = 'country.name', destination = 'iso3c')),
                           colnames(voting),
                           countrycode(colnames(voting), origin = 'country.name', destination = 'iso3c'))
# Removing empty cols
rm <- apply(voting, 2, \(col) sum(!is.na(col)))
rm <- rm == 0
voting <- voting[,!rm]

wvdem <- tibble(vdem) |>
  filter(year > 1975) |> # So I can do the lags correctly. This will be standardized later.
  group_by(country_text_id) |>
  arrange(year) |> # Should already be the case, but just in case it's not
  mutate(diff_polyarchy = v2x_polyarchy - lag(v2x_polyarchy)) |>
  # Backsliding boolean variable. I'm quantitatively defining it as having decreased in electoral democracy score by more than .005 and having decreased by at least 0.03 in the last two years.
  # I also filtered so that the regime has to be at least some semblance of an electoral autocracy (at least) so that hard autocracies getting more autocratic aren't included.
  mutate(backslided = (diff_polyarchy < -0.005) &
           (v2x_regime_amb > 2) &
           (lag(v2x_polyarchy, 2) - v2x_polyarchy > 0.03)) |>
  # Filling it out
  mutate(backslided = (diff_polyarchy < -0.001) &
           (backslided |
              ((lag(backslided) & lag(backslided, 2))|
              lead(backslided) & lag(backslided) |
              lead(backslided, 2) & lead(backslided)))) |>
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
  ))

# Finding highest/lowest scores in polyarchy to filter
mgdata |> filter(country_text_id %in% c("FRA", "USA", "CHN", "RUS", "GBR")) |>
  group_by(year) |>
  filter(v2x_polyarchy == max(v2x_polyarchy)) |>
  ungroup() |>
  group_by(country_name) |>
  summarize(count = n()) # France all but 10 years, wasn't far behind in the years it wasn't max though.

mgdata |> filter(country_text_id %in% c("FRA", "USA", "CHN", "RUS", "GBR")) |>
  group_by(year) |>
  filter(v2x_polyarchy == min(v2x_polyarchy)) |>
  ungroup() |>
  group_by(country_name) |>
  summarize(count = n()) # China every year since 1991!


# Graphing
sumvotes_year <- function(df) {
  df |> pivot_longer(!meeting_record:link,
                     names_to = "country",
                     values_to = "vote",
                     values_drop_na = TRUE) |>
    select(date, resolution, country, vote) |>
    mutate(year = year(date)) |>
    
    group_by(year, country) |>
    summarize(total = n(),
              yes = sum(vote == "Y") / total,
              no = sum(vote == "N") / total,
              abstain = sum(vote == "A") / total,
              missing = sum(vote == "X") / total,
              .groups = 'drop') |>
    
    merge(mgwreg, by.x = c("year", "country"), by.y = c("year", "country_text_id")) |>
    as_tibble()
}
graph_polyarchy <- function(df, ylower = 0.8){
  ggplot(df, aes(v2x_polyarchy, y = yes)) +
    geom_point(aes(color = regime)) +
    ylim(ylower, 1) +
    geom_smooth()
}
graph_diff_polyarchy <- function(df, xlower = -0.1, xupper = 0.025, ylower = 0.7) {
  ggplot(df, aes(diff_polyarchy, y = yes)) +
    geom_point(aes(color = regime)) +
    xlim(xlower, xupper) +
    ylim(ylower, 1) +
    geom_smooth()
}


# Yes/No/etc.
voterate <- sumvotes_year(voting)
graph_polyarchy(voterate)
graph_diff_polyarchy(voterate)

# Match with France?
reference = "FRA"
compare_votes <- function(col, reference){
  case_when(
    col == "X" ~ "X",
    col == voting[[reference]] ~ "Y",
    col == "A" ~ "A",
    col != voting[[reference]] ~ "N",
    TRUE ~ NA
  )
}

matchFR <- voting |>
  mutate(across(!meeting_record:link, compare_votes)) |>
  sumvotes_year()

graph_polyarchy(matchFR)
graph_diff_polyarchy(matchFR)

 # China
reference = "CHN"
matchCN <- voting |>
  mutate(across(!meeting_record:link, compare_votes)) |>
  sumvotes_year()

graph_polyarchy(matchCN)
graph_diff_polyarchy(matchCN)
