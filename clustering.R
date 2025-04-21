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

hclustering <- function(df, begin, end){
  cvt <- df |> filter(between(year, begin, end)) |>
    dplyr::select(-year)
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
  csts <- cutree(hcst, k = 4)
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
  hclustering(cut, begin, end) |>
    merger(begin, end)
}  

cut <- cleaned_voting |> dplyr::select(-c(meeting_record:date, title:link, chil_es, SOM))

hc1 <- wf(1990, 1994)
hc2 <- wf(1995, 1999)
hc3 <- wf(2000, 2004)
hc4 <- wf(2005, 2009)
hc5 <- wf(2010, 2014)
hc6 <- wf(2015, 2019)
hc7 <- wf(2020, 2024)

ggplot(hc7, aes(x = regime, y = v2x_polyarchy, color = factor(cluster))) +
  geom_point()


plot(hc)
dhc <- as.dendrogram(hc)
ggdendro::ggdendrogram(dhc, rotate = TRUE)

clusters <- cutree(hc, k = 8)
cluster_df <- data.frame(country = names(clusters), cluster = clusters) |>
  arrange(cluster)


