## Setting up rhetoric methods

source("main.R")

speeches <- read_tsv("Data/speeches.tsv") |> filter(year > 1990, state == 1)


speeches$state <- ifelse(is.na(countrycode(speeches$affiliation, origin = 'country.name', destination = 'iso3c')),
                         speeches$affiliation,
                         countrycode(speeches$affiliation, origin = 'country.name', destination = 'iso3c'))

speeches$topic <- tolower(speeches$topic) |>
  removeWords(stopwords()) |>
  stripWhitespace()

speeches$topic <- gsub(" |-|—|situation", "", speeches$topic)

gen_speech <- speeches |>
  select(meeting_num, year, month, day, topic, affiliation, speech, state) |>
  merge(mgwreg, by.x = c("year", "state"), by.y = c("year", "country_text_id"), all.x = TRUE) |>
  as_tibble()