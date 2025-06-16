source("rhetoric-setup.R")
library(topicmodels)

## Checking speeches!
ga_speeches <- read_csv("GA_Speech/gaspeeches.csv")


## Individual Words
wf_gaspeeches <- function(begin, end, df = ga_speeches){
  res <- df |>
    filter(between(year, begin, end)) |>
    pull(speech) |>
    to_tdm() |>
    freqTerms(cutoff = 1000)
  res <- res[res < 10000]
  res[!(names(res) %in% c("presid", "general", "assembl"))]
}

gaspeech1 <- wf_gaspeeches(1991, 1994)
gaspeech1pct <- (gaspeech1 / sum(gaspeech1)) * 100

gaspeech2 <- wf_gaspeeches(1995, 1999)
gaspeech2pct <- (gaspeech2 / sum(gaspeech2)) * 100

gaspeech3 <- wf_gaspeeches(2000, 2004)
gaspeech3pct <- (gaspeech3 / sum(gaspeech3)) * 100

gaspeech4 <- wf_gaspeeches(2005, 2009)
gaspeech4pct <- (gaspeech4 / sum(gaspeech4)) * 100

gaspeech5 <- wf_gaspeeches(2010, 2014)
gaspeech5pct <- (gaspeech5 / sum(gaspeech5)) * 100

gaspeech6 <- wf_gaspeeches(2015, 2019)
gaspeech6pct <- (gaspeech6 / sum(gaspeech6)) * 100

gaspeech7 <- wf_gaspeeches(2020, 2024)
gaspeech7pct <- (gaspeech7 / sum(gaspeech7)) * 100

gaspeechfreq <- staple_vecs7(gaspeech1, gaspeech2, gaspeech3, gaspeech4, gaspeech5, gaspeech6, gaspeech7)

gaspeechpct <- staple_vecs7(gaspeech1pct, gaspeech2pct, gaspeech3pct, gaspeech4pct, gaspeech5pct, gaspeech6pct, gaspeech7pct)

gaspeechfreq |> arrange(desc(`2020-24`)) |>
  head(100) |>
  print(n = 100)
gaspeechfreq |> filter(grepl("palestin|ukrain|israel|sudan|democ", term))

gaspeechpct |> arrange(desc(`2020-24`)) |>
  head(100) |>
  print(n = 100)
gaspeechpct |> filter(grepl("palestin|ukrain|israel|sudan|democ", term))


## LDA - Topics!!
wf_lda <- function(begin, end, df = ga_speeches){
  df |>
    filter(between(year, begin, end)) |>
    pull(speech) |>
    to_dtm() |>
    LDA(k = 6, control = list(seed = 123)) |>
    tidy(matrix = "beta")
}

test <- wf_lda(2020, 2024)
top_terms <- test |>
  group_by(topic) |>
  slice_max(beta, n=10) |>
  ungroup() |>
  arrange(topic, -beta)

top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_wide <- test |>
  mutate(topic = paste0("topic", topic)) |>
  pivot_wider(names_from = topic, values_from = beta)

beta_wide$max <- beta_wide |>
  select(topic1:topic6) |>
  apply(1, max)

beta_wide <- beta_wide |>
  mutate(hightopic = case_when(
    max == topic1 ~ 1,
    max == topic2 ~ 2,
    max == topic3 ~ 3,
    max == topic4 ~ 4,
    max == topic5 ~ 5,
    max == topic6 ~ 6
  ))

beta_wide |>
  group_by(hightopic) |>
  slice_max(max, n=10) |>
  ungroup() |>
  arrange(hightopic, -max) |>
  mutate(term = reorder_within(term, max, hightopic)) |>
  ggplot(aes(max, term, fill = factor(hightopic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ hightopic, scales = "free") +
  scale_y_reordered()
