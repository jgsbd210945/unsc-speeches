## Mainly for finding general counting statistics with the voting data.
## There's a good chance I don't need most of this, but this should plot the general
## Yes/No/Abstain.

source("voting-setup.R")

# Functions
pivot_votes <- function(df){ # helper for sumvotes_year (make a dataframe based on voting/res/etc.)
  df |> pivot_longer(!meeting_record:link,
                     names_to = "country",
                     values_to = "vote",
                     values_drop_na = TRUE) |>
    select(date, year, resolution, country, vote, meeting_topic, title)
}

sumvotes_year <- function(df, merger = mgwreg) { # Makes a year-by-year percentage of votes yes/no/abstain.
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

graph_polyarchy <- function(df, ylower = 0.8){ # Graphs based on v2x_polyarchy
  ggplot(df, aes(v2x_polyarchy, yes, color = regime)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    ylim(ylower, 1) +
    labs(color = "Regime") +
    scale_color_manual(values = color_scheme,
                       labels = c("Democratic Erosion",
                                  "Democratic Reversion",
                                  "Entrenched Autocracy",
                                  "Entrenched Democracy",
                                  "Grey Area Regime")) +
    xlab("Electoral Democracy Score") +
    ylab("Rate of Votes In Favor of Resolutions per Year")
}

graph_diff_polyarchy <- function(df, xlower = -0.08, xupper = 0.01, ylower = 0.8) { # Same as above, but with diff_polyarchy.
  ggplot(df, aes(diff_polyarchy, yes)) +
    geom_point(aes(color = regime)) +
    geom_smooth(method = 'lm') +
    xlim(xlower, xupper) +
    ylim(ylower, 1) +
    labs(color = "Regime") +
    scale_color_manual(values = color_scheme,
                       labels = c("Democratic Erosion",
                                  "Democratic Reversion",
                                  "Entrenched Autocracy",
                                  "Entrenched Democracy",
                                  "Grey Area Regime"))+
    xlab("Difference in Electoral Democracy Score") +
    ylab("Rate of Votes in Favor of Resolutions per Year")
}


# Yes/No/etc.
voterate <- sumvotes_year(voting)

## Votes in favor of resolutions based on polyarchy score/diff in polyarchy score
graph_polyarchy(voterate)
graph_diff_polyarchy(voterate)

## Votes against resolutions based on overall v2x_polyarchy score
ggplot(voterate, aes(v2x_polyarchy, no, color = regime)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylim(0, 0.075) +
  labs(color = "Regime") +
  scale_color_manual(values = color_scheme,
                     labels = c("Democratic Erosion",
                                "Democratic Reversion",
                                "Entrenched Autocracy",
                                "Entrenched Democracy",
                                "Entrenched Illiberal"))+
  xlab("Electoral Democracy Score") +
  ylab("Rate of Votes Against of Resolutions per Year")

## GA Stuff
gavote <- read_csv("Data/2025_03_31_ga_voting_corr1.csv") |>
  mutate(year = year(date)) |>
  filter(between(year, 1991, 2024)) |>
  select(resolution, year, ms_code:ms_vote, title, total_yes:total_ms)

group_years <- function(df, begin, end){
  df |>
    group_by(resolution, title) |>
    summarize(year = median(year),
              yes = median(total_yes),
              no = median(total_no),
              abst = median(total_abstentions),
              nonvoting = median(total_non_voting),
              ms = median(total_ms)) |>
    ungroup() |>
    filter(between(year, begin, end))
}

to_tdm <- function(col){
  Corpus(VectorSource(col)) |>
    tm_map(removePunctuation, ucp = TRUE) |>
    TermDocumentMatrix(control = list(
      stopwords = TRUE,
      tolower = TRUE,
      stemming = TRUE,
      removeNumbers = TRUE,
      bounds = list(global = c(3, Inf))
    ))
}

freqTerms <- function(tdm, cutoff = Inf){
  as.matrix(tdm) |>
    apply(1, sum) |>
    sort(decreasing = TRUE) |>
    head(cutoff)
}

wf_gavotes <- function(begin, end, df = gavote){
  res <- df |>
    group_years(begin, end) |>
    pull(title) |>
    to_tdm() |>
    freqTerms()
  res <- res[res < 300] # "General", "Assembly", "Adopt", etc.
}

gatopic1 <- wf_gavotes(1990, 1994)
gatopic2 <- wf_gavotes(1995, 1999)
gatopic3 <- wf_gavotes(2000, 2004)
gatopic4 <- wf_gavotes(2005, 2009)
gatopic5 <- wf_gavotes(2010, 2014)
gatopic6 <- wf_gavotes(2015, 2019)
gatopic7 <- wf_gavotes(2020, 2024)

gatopicfreq <- mergeVectors(gatopic1, gatopic2, gatopic3, gatopic4, gatopic5, gatopic6, gatopic7) |>
  as_tibble() |>
  mutate(years = c("1990-94", "1995-99", "2000-04", "2005-09", "2010-14", "2015-19", "2020-24"),
         .before = 1)
rmfreq <- apply(gatopicfreq, 2, \(col) sum(!is.na(col)))
rmfreq <- rmfreq <= 1
gatopicfreq <- gatopicfreq[,!rmfreq]
gatopicfreq <- gatopicfreq %>% replace(is.na(.), 0) |>
  data.table::transpose(keep.names = "term", make.names = "years") |>
  as_tibble()

gatopicfreq |> arrange(desc(`2020-24`)) |>
  head(100) |>
  print(n = 100)