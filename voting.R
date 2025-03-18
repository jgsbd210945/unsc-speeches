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
cleaned_voting$meeting_topic <- gsub(" ", "", cleaned_voting$meeting_topic)
cleaned_voting$meeting_topic <- gsub("situation", "", cleaned_voting$meeting_topic)

mgwreg$bve <- ifelse(grepl("backslide_", mgwreg$regime), "backsliding",
                     ifelse(grepl("dem", mgwreg$regime), "democratic",
                            "entrenched"))

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
pivot_votes <- function(df){
  df |> pivot_longer(!meeting_record:link,
                     names_to = "country",
                     values_to = "vote",
                     values_drop_na = TRUE) |>
    select(date, year, resolution, country, vote, meeting_topic, title)
}

sumvotes_year <- function(df, merger = mgwreg) {
  df |> pivot_votes() |>
    group_by(year, country) |>
    reframe(total = n(),
              yes = sum(vote == "Y") / total,
              no = sum(vote == "N") / total,
              abstain = sum(vote == "A") / total,
              missing = sum(vote == "X") / total) |>
    merge(merger, by.x = c("year", "country"), by.y = c("year", "country_text_id")) |>
    as_tibble()
}

graph_polyarchy <- function(df, ylower = 0.8){
  ggplot(df, aes(v2x_polyarchy, y = yes, color = regime)) +
    geom_point() +
    geom_smooth() +
    ylim(ylower, 1)
}

graph_diff_polyarchy <- function(df, xlower = -0.08, xupper = 0.01, ylower = 0.75) {
  ggplot(df, aes(diff_polyarchy, y = yes, color = regime)) +
    geom_point() +
    facet_wrap(vars(regime)) +
    geom_smooth(method = 'lm') +
    xlim(xlower, xupper) +
    ylim(ylower, 1)
}


# Yes/No/etc.
voterate <- sumvotes_year(voting)
graph_polyarchy(voterate)
graph_diff_polyarchy(voterate)


# Match with France?
reference = "FRA"
compare_votes <- function(col){
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

# Backsliding to entrenched Directly
bve_mg <- sumvotes_year(voting)

bve_mg |> ggplot(aes(x = bve, y = yes)) +
  geom_boxplot() +
  ylim(0.65, 1)
bve_mg |> ggplot(aes(x = regime, y = yes)) +
  geom_boxplot() +
  ylim(0.65, 1)


byreg_votes <- cleaned_voting |> pivot_votes() |>
  merge(mgwreg, by.x = c("year", "country"), by.y = c("year", "country_text_id"),
        all.x = TRUE) |>
  as_tibble() |>
  filter(!is.na(regime)) |>
  group_by(meeting_topic, regime, year) |>
  reframe(total = n(),
            yes = sum(vote == "Y") / total,
            no = sum(vote == "N") / total,
            abstain = sum(vote == "A") / total,
            missing = sum(vote == "X") / total) |>
  filter(!grepl("admission|election|appointment", meeting_topic)) |>
  arrange(desc(total))

bve_votes <- cleaned_voting |> pivot_votes() |>
  merge(mgwreg, by.x = c("year", "country"), by.y = c("year", "country_text_id"),
        all.x = TRUE) |>
  as_tibble() |>
  filter(!is.na(regime)) |>
  group_by(meeting_topic, bve) |>
  reframe(total = n(),
          yes = sum(vote == "Y") / total,
          no = sum(vote == "N") / total,
          abstain = sum(vote == "A") / total,
          missing = sum(vote == "X") / total) |>
  filter(!grepl("admission|election|appointment", meeting_topic)) |>
  arrange(desc(total))

bve_test <- bve_votes |> pivot_wider(
  id_cols = meeting_topic,
  names_from = bve,
  values_from = yes) |>
  relocate(democratic, .before = entrenched) |>
  mutate(bve = backsliding / entrenched) |>
  arrange(bve)
  
#bve_test |> print(n = 25)
#bve_test |> arrange(desc(bve)) |> print(n = 25)

# Case studies
case_clean <- function(term, lowyear = 1991, df = cleaned_voting) {
  df |> filter(grepl(term, meeting_topic), year >= lowyear, (abstain != 0 | no != 0)) |>
    clean_nas()
}

bve_test |> filter(grepl("cyprus|ukraine|georgia|africa", meeting_topic))

case_clean("cyprus", 2000)
case_clean("sudan", 2023) |> select(date, total:yes, DZA, SLE, MOZ)
case_clean("ukraine", 2014)
case_clean("georgia", 2008)
case_clean("taiwan")
case_clean("venezuela")
case_clean("korea") |> select(date, total:RUS)

meetings |> filter(grepl("china", topic))
