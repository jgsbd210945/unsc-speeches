source("main.R")
#library(cluster)
library(ggdendro)
library(ggplot2)
library(proxy)
library(stats)

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

cut <- cleaned_voting |> dplyr::select(-c(meeting_record:link, chil_es, SOM))

clust_voting <- t(cut) |>
  as.data.frame() |>
  mutate(across(everything(), ~ factor(.)))
rownames(clust_voting) <- colnames(cut)

vote_dist <- function(c1, c2, min_overlap = 10) {
  mask <- !is.na(c1) & !is.na(c2)
  if (sum(mask) < min_overlap) {
    return(1) # too few votes (one year should have at least 10 RES regardless)
  }
  # simple 1 – pct_agreement:
  1 - mean(c1[mask] == c2[mask])
}

dist_mat <- proxy::dist(x = clust_voting,
                        method = function(c1, c2) vote_dist(c1, c2, min_overlap = 10))

no_na <- which(rowSums(!is.na(as.matrix(dist_mat))) > 1)
dist_mat2 <- as.dist(as.matrix(dist_mat)[no_na, no_na])

hc <- hclust(dist_mat2, method = "average")

plot(hc)
dhc <- as.dendrogram(hc)
ggdendro::ggdendrogram(dhc, rotate = TRUE)


