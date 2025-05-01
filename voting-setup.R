### General voting data page.
### I should be calling this whenever working with voting data; it'll get the
### SC/GA data imported and set up.

source("main.R")

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
