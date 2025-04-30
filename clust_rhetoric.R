source("main.R")
library(tidytext)
library(SnowballC)
library(proxy)

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

## Same as clustering.R's method
plotting <- function(df) {
  ggplot(df, aes(x = diff_polyarchy, y = v2x_polyarchy, color = factor(cluster))) +
    geom_point(size = 2) +
    scale_color_manual(values = c("#56B4E9", "#000000", "#CC7987", "#009E73", "#0072B2", "#F0E442", "#999999", "#D55E00", "#800080"))
}

## Functions
make_dmx <- function(df, begin, end){
  freqs <- df |>
    filter(between(year, begin, end)) |>
    unnest_tokens(word, speech) |>
    anti_join(stop_words, by = "word") |>
    mutate(stemmed = wordStem(word)) |>
    count(country_text_id, stemmed, sort = TRUE) |>
    group_by(country_text_id) |>
    mutate(freq = n / sum(n)) |>
    select(-n) |>
    ungroup() |> 
    mutate(freq = freq * 1000000) |>
    filter(grepl("interv|interfer|selfdet|sover|rights|humanright|humanitarian|peac|^(regime|norm|normat|standard|right|liber|neoliber|forc)$", stemmed)) |>
    mutate(stemmed = case_when(
      str_detect(stemmed, "^norm") ~ "norm",
      str_detect(stemmed, "^sovereign") ~ "sovereign",
      str_detect(stemmed, "^peacekeep") ~ "peacekeep",
      str_detect(stemmed, "^peacebuild") ~ "peacebuild",
      str_detect(stemmed, "^interv") ~ "interv",
      str_detect(stemmed, "^humanitarian") ~ "humanitarian",
      (str_starts(stemmed, "peac") &
         !str_starts(stemmed, "peacebuild") &
         !str_starts(stemmed, "peacekeep")) ~ "peac",
      TRUE ~ stemmed
    )) |>
    group_by(country_text_id, stemmed) |>
    summarize(freq = sum(freq), .groups = "drop") |> 
    pivot_wider(names_from = stemmed, values_from = freq, values_fill = 0)
  
  norm_mat <- freqs |>
    select(-country_text_id) |>
    as.matrix()
  rownames(norm_mat) <- freqs$country_text_id
  
  proxy::dist(norm_mat, method = "eJaccard")
}

hc_rhet <- function(dist_mat, groups){
  hcst <- hclust(dist_mat, method = "ward.D2")
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

wf_rhet <- function(df, ngroups, begin, end){
  make_dmx(df, begin, end) |>
    hc_rhet(groups = ngroups) |>
    merger(begin, end)
}

rhet1 <- wf_rhet(speeches, 4, 1990, 1994)
rhet2 <- wf_rhet(speeches, 4, 1995, 1999)
rhet3 <- wf_rhet(speeches, 4, 2000, 2004)
rhet4 <- wf_rhet(speeches, 4, 2005, 2009)
rhet5 <- wf_rhet(speeches, 4, 2010, 2014)
rhet6 <- wf_rhet(speeches, 4, 2015, 2019)
rhet7 <- wf_rhet(speeches, 4, 2020, 2024)


rhet1 |> plotting()
rhet2 |> plotting()
rhet3 |> plotting()
rhet4 |> plotting()
rhet5 |> plotting()
rhet6 |> plotting()
rhet7 |> plotting()



# GA Rhetoric













