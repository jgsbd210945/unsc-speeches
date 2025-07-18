source("rhetoric-setup.R")
source("cluster-setup.R")
library(tidytext)
library(SnowballC)

## Functions
filter_norms <- function(df, begin, end){
  df |>
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
    ))
}

make_dmx <- function(df, begin, end, normonly){
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
    
    (\(df) if(normonly) filter_norms(df) else df)() |>
    
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

wf_rhet <- function(df, ngroups, begin, end, normonly = TRUE){
  make_dmx(df, begin, end, normonly) |>
    hc_rhet(groups = ngroups) |>
    merger(begin, end)
}

sc_speeches <- speeches |>
  rename(country_text_id = state)

rhet1 <- wf_rhet(sc_speeches, 4, 1990, 1994)
rhet2 <- wf_rhet(sc_speeches, 4, 1995, 1999)
rhet3 <- wf_rhet(sc_speeches, 4, 2000, 2004)
rhet4 <- wf_rhet(sc_speeches, 4, 2005, 2009)
rhet5 <- wf_rhet(sc_speeches, 4, 2010, 2014)
rhet6 <- wf_rhet(sc_speeches, 4, 2015, 2019)
rhet7 <- wf_rhet(sc_speeches, 4, 2020, 2024)


rhet1 |> plotting()
rhet2 |> plotting()
rhet3 |> plotting()
rhet4 |> plotting()
rhet5 |> plotting()
rhet6 |> plotting()
rhet7 |> plotting()



# GA Rhetoric

ga_speeches <- read_csv("GA_Speech/gaspeeches.csv")

garh1 <- wf_rhet(ga_speeches, 6, 1990, 1994)
garh2 <- wf_rhet(ga_speeches, 6, 1995, 1999)
garh3 <- wf_rhet(ga_speeches, 6, 2000, 2004)
garh4 <- wf_rhet(ga_speeches, 6, 2005, 2009)
garh5 <- wf_rhet(ga_speeches, 6, 2010, 2014)
garh6 <- wf_rhet(ga_speeches, 6, 2015, 2019)
garh7 <- wf_rhet(ga_speeches, 6, 2020, 2024)

garh1 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNGA Rhetoric Clustering, 1991-1994")
garh2 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNGA Rhetoric Clustering, 1995-1999")
garh3 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNGA Rhetoric Clustering, 2000-2004")
garh4 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNGA Rhetoric Clustering, 2005-2009")
garh5 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNGA Rhetoric Clustering, 2010-2014")
garh6 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNGA Rhetoric Clustering, 2015-2019")
garh7 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNGA Rhetoric Clustering, 2020-2024")


scga_speeches <- sc_speeches |>
  select(year, country_text_id, speech) |>
  rbind(ga_speeches)

scgarh1 <- wf_rhet(scga_speeches, 6, 1990, 1994)
scgarh2 <- wf_rhet(scga_speeches, 6, 1995, 1999)
scgarh3 <- wf_rhet(scga_speeches, 6, 2000, 2004)
scgarh4 <- wf_rhet(scga_speeches, 6, 2005, 2009)
scgarh5 <- wf_rhet(scga_speeches, 6, 2010, 2014)
scgarh6 <- wf_rhet(scga_speeches, 6, 2015, 2019)
scgarh7 <- wf_rhet(scga_speeches, 6, 2020, 2024)

scgarh1 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNSC/GA Rhetoric Clustering, 1991-1994")
scgarh2 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNSC/GA Rhetoric Clustering, 1995-1999")
scgarh3 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNSC/GA Rhetoric Clustering, 2000-2004")
scgarh4 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNSC/GA Rhetoric Clustering, 2005-2009")
scgarh5 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNSC/GA Rhetoric Clustering, 2010-2014")
scgarh6 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNSC/GA Rhetoric Clustering, 2015-2019")
scgarh7 |>
  mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |> 
  plotting() +
  labs(title = "UNSC/GA Rhetoric Clustering, 2020-2024")


## Correlation Checking
cortest <- function(rhet, vot){
  merge(rhet, vot, by.x = "country_text_id", by.y = "country") |>
    select(cluster.x, cluster.y) |> cor()
}

gvot1 <- read_csv("GA_distmat/ga1.csv") |> as.dist() |> hclustering(groups = 6)
gvot2 <- read_csv("GA_distmat/ga2.csv") |> as.dist() |> hclustering(groups = 6)
gvot3 <- read_csv("GA_distmat/ga3.csv") |> as.dist() |> hclustering(groups = 6)
gvot4 <- read_csv("GA_distmat/ga4.csv") |> as.dist() |> hclustering(groups = 6)
gvot5 <- read_csv("GA_distmat/ga5.csv") |> as.dist() |> hclustering(groups = 6)
gvot6 <- read_csv("GA_distmat/ga6.csv") |> as.dist() |> hclustering(groups = 6)
gvot7 <- read_csv("GA_distmat/ga7.csv") |> as.dist() |> hclustering(groups = 6)

cortest(scgarh1, gvot1)
cortest(scgarh2, gvot2)
cortest(scgarh3, gvot3)
cortest(scgarh4, gvot4)
cortest(scgarh5, gvot5)
cortest(scgarh6, gvot6)
cortest(scgarh7, gvot7)

## That's strange, it's almost impressive how *not* correlated they are.

## ...& Even if I do it by ALL WORDS, it's still by no means correlated.


