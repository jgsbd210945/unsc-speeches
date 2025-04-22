source("main.R")
#library(cluster)
library(ggdendro)
library(proxy)
library(stats)
library(DescTools)

## From Voting
voting <- read_tsv("Data/UNSC Voting.tsv")  |>
  # Multiple cols for Bolivia and Venezuela due to gov't changes/UN Naming conventions.
  mutate(Bolivia = coalesce(Bolivia...134, Bolivia...142),
         Venezuela = coalesce(Venezuela...25, Venezuela...145)) |>
  select(-c(Bolivia...134, Bolivia...142, Venezuela...25, Venezuela...145)) |>
  clean_names() |>
  mutate(meeting_date = as.Date(meeting_date, format = "%m/%d/%Y"),
         year = year(date)) |>
  relocate(year, .after = date) |>
  rename(KOR = "south_korea")

# Converting to country codes for easier matching
colnames(voting) <- ifelse(is.na(countrycode(colnames(voting), origin = 'country.name', destination = 'iso3c')),
                           colnames(voting),
                           countrycode(colnames(voting), origin = 'country.name', destination = 'iso3c'))
# Removing empty cols
voting <- voting |> clean_nas()

cleaned_voting <- voting
cleaned_voting$meeting_topic <- tolower(cleaned_voting$meeting_topic) |>
  removeWords(stopwords()) |>
  stripWhitespace()
cleaned_voting$meeting_topic <- gsub(" |-|—", "", cleaned_voting$meeting_topic)
cleaned_voting$meeting_topic <- gsub("situation", "", cleaned_voting$meeting_topic)

# Clustering
vote_dist <- function(c1, c2, min_overlap = 10) {
  mask <- !is.na(c1) & !is.na(c2)
  if (sum(mask) < min_overlap) {
    return(1) # too few votes (one year should have at least 10 RES regardless)
  }
  # simple 1 – pct_agreement:
  1 - mean(c1[mask] == c2[mask])
}

hclustering <- function(cvt, groups){
  c_vt <- t(cvt) |>
    as.data.frame() |>
    mutate(across(everything(), ~ factor(.)))
  rownames(c_vt) <- colnames(cvt)
  na_rows <- apply(c_vt, 1, \(x) sum(!is.na(x)))
  c_vt <- c_vt[na_rows != 0,]
  d_mt <- proxy::dist(x = c_vt,
                      method = function(c1, c2) vote_dist(c1, c2, min_overlap = 10))
  n_na <- which(rowSums(!is.na(as.matrix(d_mt))) > 1) 
  d_mt2 <- as.dist(as.matrix(d_mt)[n_na, n_na])
  hcst <- hclust(d_mt2, method = "ward.D2")
  csts <- cutree(hcst, k = groups)
  data.frame(country = names(csts), cluster = csts) |> arrange(cluster)
}

merger <- function(to_merge, begin, end) {
  mgwreg |> filter(between(year, begin, end)) |>
    group_by(country_text_id) |>
    summarize(v2x_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
              v2x_regime_amb = round(mean(v2x_regime_amb, na.rm = TRUE)),
              diff_polyarchy = mean(diff_polyarchy, na.rm = TRUE),
              backslided = any(backslided, na.rm = TRUE),
              regime = names(which.max(table(regime))),
              bve = ifelse(any(bve == "backslided"), "backslided", bve)) |>
    merge(to_merge, by.x = "country_text_id", by.y = "country") |>
    as_tibble()
}

wf <- function(begin, end){
  cut |> filter(between(year, begin, end)) |>
    dplyr::select(-year)|>
    hclustering(groups = 4) |>
    merger(begin, end)
}

plotting <- function(df) {
  ggplot(df, aes(x = diff_polyarchy, y = v2x_polyarchy, color = factor(cluster))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#56B4E9", "#000000", "#CC7987", "#009E73", "#0072B2", "#F0E442", "#999999", "#D55E00", "#800080"))
}

cut <- cleaned_voting |> dplyr::select(-c(meeting_record:date, title:link, chil_es, SOM))

hc1 <- wf(1990, 1994)
hc2 <- wf(1995, 1999)
hc3 <- wf(2000, 2004)
hc4 <- wf(2005, 2009)
hc5 <- wf(2010, 2014)
hc6 <- wf(2015, 2019)
hc7 <- wf(2020, 2024)

hc1 |> plotting()
hc2 |> plotting()
hc3 |> plotting()
hc4 |> plotting()
hc5 |> plotting()
hc6 |> plotting()
hc7 |> plotting()

#### GA ####
wf_ga <- function(begin, end){
  gavote |> filter(between(year, begin, end)) |>
    select(resolution, ms_code, ms_vote) |>
    pivot_wider(id_cols = resolution, names_from = ms_code, values_from = ms_vote) |>
    select(-resolution) |>
    hclustering(groups = 8) |>
    merger_ga(begin, end)
}
merger_ga <- function(to_merge, begin, end) {
  mgwreg |>
    filter(between(year, begin, end)) |>
    group_by(country_text_id) |>
    summarize(v2x_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
              v2x_regime_amb = round(mean(v2x_regime_amb, na.rm = TRUE)),
              diff_polyarchy = mean(diff_polyarchy, na.rm = TRUE),
              backslided = any(backslided, na.rm = TRUE),
              regime = names(which.max(table(regime))),
              bve = ifelse(any(bve == "backslided"), "backslided", bve)) |>
    merge(to_merge, by.x = "country_text_id", by.y = "country") |>
    mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) # scaling
}


gavote <- read_csv("Data/2025_03_31_ga_voting_corr1.csv") |>
  mutate(year = year(date)) |>
  filter(between(year, 1991, 2024)) |>
  dplyr::select(resolution, ms_code, ms_name, ms_vote, year)

ga1 <- wf_ga(1990, 1994)
ga2 <- wf_ga(1995, 1999)
ga3 <- wf_ga(2000, 2004)
ga4 <- wf_ga(2005, 2009)
ga5 <- wf_ga(2010, 2014)
ga6 <- wf_ga(2015, 2019)
ga7 <- wf_ga(2020, 2024)

ga1 |> plotting()
ga2 |> plotting()
ga3 |> plotting()
ga4 |> plotting()
ga5 |> plotting()
ga6 |> plotting()
ga7 |> plotting()
