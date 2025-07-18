source("voting-setup.R")
source("cluster-setup.R")

### Focus on *what* states are voting on per period
## Disaggregate by topic!
## Look at which members stopped voting together
## There's a bit that's on the democratic coalition that takes ~an hour to run. Don't source this file unless you want to wait.

# Clustering
wf_scvote <- function(begin, end, ngrp){
  cut |> filter(between(year, begin, end)) |>
    dplyr::select(-year)|>
    get_dist_mat() |>
    hclustering(groups = ngrp) |>
    merger(begin, end)
}

cut <- cleaned_voting |> dplyr::select(-c(meeting_record:date, title:link, chil_es, SOM))

hc1 <- wf_scvote(1990, 1994, 3)
hc2 <- wf_scvote(1995, 1999, 3)
hc3 <- wf_scvote(2000, 2004, 3)
hc4 <- wf_scvote(2005, 2009, 3)
hc5 <- wf_scvote(2010, 2014, 3)
hc6 <- wf_scvote(2015, 2019, 3)
hc7 <- wf_scvote(2020, 2024, 3)

hc1 |> plotting()
hc2 |> plotting()
hc3 |> plotting()
hc4 |> plotting()
hc5 |> plotting()
hc6 |> plotting()
hc7 |> plotting()

### GA Area ###
wf_w_dm <- function(dist_mat, begin, end){
  hclustering(dist_mat, groups = 6) |>
    merger_ga(begin, end)
}

ga1 <- read_csv("GA_distmat/ga1.csv") |> as.dist() |> wf_w_dm(1990, 1994)
ga2 <- read_csv("GA_distmat/ga2.csv") |> as.dist() |> wf_w_dm(1995, 1999)
ga3 <- read_csv("GA_distmat/ga3.csv") |> as.dist() |> wf_w_dm(2000, 2004)
ga4 <- read_csv("GA_distmat/ga4.csv") |> as.dist() |> wf_w_dm(2005, 2009)
ga5 <- read_csv("GA_distmat/ga5.csv") |> as.dist() |> wf_w_dm(2010, 2014)
ga6 <- read_csv("GA_distmat/ga6.csv") |> as.dist() |> wf_w_dm(2015, 2019)
ga7 <- read_csv("GA_distmat/ga7.csv") |> as.dist() |> wf_w_dm(2020, 2024)

## I think the issue mostly is just that the clusters aren't standardised insofar as they can swap around.
## Curious if I can lock in something.

ga1 |> plotting() +
  labs(title = "UNGA Vote Clustering, 1991-1994")
  
ga2 |> plotting() +
  labs(title = "UNGA Vote Clustering, 1995-1999")

ga3 |> plotting() +
  labs(title = "UNGA Vote Clustering, 2000-2004")

ga4 |> plotting() +
  labs(title = "UNGA Vote Clustering, 2005-2009")

ga5 |> plotting() +
  labs(title = "UNGA Vote Clustering, 2010-2014")

ga6 |> plotting() +
  labs(title = "UNGA Vote Clustering, 2015-2019")

ga7 |> plotting() +
  labs(title = "UNGA Vote Clustering, 2020-2024")


### Building Clusters - 2020-24 ###
make_plot <- function(dist, k, beginyear, endyear){
  dist |>
    hclustering(groups = k) |>
    merger_ga(beginyear, endyear) |>
    plotting()
}

dist5 <- read_csv("GA_distmat/ga5.csv") |>
  as.dist()

make_plot(dist5, 2, 2010, 2014) + labs(title = "2010-2014 UNGA Vote Clustering, 2 Clusters")
make_plot(dist5, 3, 2010, 2014) + labs(title = "2010-2014 UNGA Vote Clustering, 3 Clusters")
make_plot(dist5, 4, 2010, 2014) + labs(title = "2010-2014 UNGA Vote Clustering, 4 Clusters")
make_plot(dist5, 5, 2010, 2014) + labs(title = "2010-2014 UNGA Vote Clustering, 5 Clusters")
make_plot(dist5, 6, 2010, 2014) + labs(title = "2010-2014 UNGA Vote Clustering, 6 Clusters")
make_plot(dist5, 7, 2010, 2014) + labs(title = "2010-2014 UNGA Vote Clustering, 7 Clusters")
make_plot(dist5, 8, 2010, 2014) + labs(title = "2010-2014 UNGA Vote Clustering, 8 Clusters")

dist6 <- read_csv("GA_distmat/ga6.csv") |>
  as.dist()

make_plot(dist6, 2, 2015, 2019) + labs(title = "2015-2019 UNGA Vote Clustering, 2 Clusters")
make_plot(dist6, 3, 2015, 2019) + labs(title = "2015-2019 UNGA Vote Clustering, 3 Clusters")
make_plot(dist6, 4, 2015, 2019) + labs(title = "2015-2019 UNGA Vote Clustering, 4 Clusters")
make_plot(dist6, 5, 2015, 2019) + labs(title = "2015-2019 UNGA Vote Clustering, 5 Clusters")
make_plot(dist6, 6, 2015, 2019) + labs(title = "2015-2019 UNGA Vote Clustering, 6 Clusters")
make_plot(dist6, 7, 2015, 2019) + labs(title = "2015-2019 UNGA Vote Clustering, 7 Clusters")
make_plot(dist6, 8, 2015, 2019) + labs(title = "2015-2019 UNGA Vote Clustering, 8 Clusters")

### Country Clustering? ###
make_clust_df <- function(df, countrycode, yrstring){
  ## Helper function to make me not have to copy everything.
  match = df |>
    filter(country_text_id == countrycode) |>
    pull(cluster)
  df |>
    filter(cluster == match) |>
    mutate(yr = yrstring)
}

checkstate <- function(countrycode){
  # Kinda a remnant of how I did this. Would prefer to *not* have to do this,
  # But it works for my purposes.
  gaclst1 <- make_clust_df(ga1, countrycode, "1991-1994")
  gaclst2 <- make_clust_df(ga2, countrycode, "1995-1999")
  gaclst3 <- make_clust_df(ga3, countrycode, "2000-2004")
  gaclst4 <- make_clust_df(ga4, countrycode, "2005-2019")
  gaclst5 <- make_clust_df(ga5, countrycode, "2010-2014")
  gaclst6 <- make_clust_df(ga6, countrycode, "2015-2019")
  gaclst7 <- make_clust_df(ga7, countrycode, "2020-2024")
  rbind(gaclst1, gaclst2, gaclst3, gaclst4, gaclst5, gaclst6, gaclst7)
}

clstFRA <- checkstate("FRA") |> as_tibble() |> relocate(yr, .before = 1)
clstFRA |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2, alpha = 0.6) +
  xlab("Arcsin(100 * Difference in Electoral Democracy Score)") +
  ylab("Electoral Democracy Score") +
  labs(title = "States in the Same Cluster as France")

clstGBR <- checkstate("GBR") |> as_tibble() |> relocate(yr, .before = 1)
clstGBR |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2, alpha = 0.6) +
  xlab("Arcsin(100 * Difference in Electoral Democracy Score)") +
  ylab("Electoral Democracy Score") +
  labs(title = "States in the Same Cluster as the United Kingdom")

clstUSA <- checkstate("USA") |> as_tibble() |> relocate(yr, .before = 1)
clstUSA |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2, alpha = 0.6) +
  xlab("Arcsin(100 * Difference in Electoral Democracy Score)") +
  ylab("Electoral Democracy Score") +
  labs(title = "States in the Same Cluster as the United States")

clstCHN <- checkstate("CHN") |> as_tibble() |> relocate(yr, .before = 1)
clstCHN |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2, alpha = 0.6) +
  xlab("Arcsin(100 * Difference in Electoral Democracy Score)") +
  ylab("Electoral Democracy Score") +
  labs(title = "States in the Same Cluster as China")

clstRUS <- checkstate("RUS") |> as_tibble() |> relocate(yr, .before = 1)
clstRUS |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2, alpha = 0.6) +
  xlab("Arcsin(100 * Difference in Electoral Democracy Score)") +
  ylab("Electoral Democracy Score") +
  labs(title = "States in the Same Cluster as Russia")


## -- DEM Coalition Analysis -- ##

clstdem <- function(df, clst, yrstring){
  df |>
    filter(cluster == clst) |>
    mutate(yr = yrstring)
}
  
pullclst <- function(){
  ## Pulls the democratic clusters from 1991-1994
  ## Then, we'll see which clusters these are in now.
  
  dc1 <- clstdem(ga1, 4, "1991-1994")
  dc2 <- clstdem(ga2, 2, "1995-1999")
  dc3 <- clstdem(ga3, 2, "2000-2004")
  dc4 <- clstdem(ga4, 2, "2005-2019")
  dc5 <- clstdem(ga5, 2, "2010-2014")
  dc6 <- clstdem(ga6, 2, "2015-2019")
  demdf <- rbind(dc1, dc2, dc3, dc4, dc5, dc6) |> as_tibble()
  
  demcoal <- demdf |>
    group_by(country_text_id) |>
    summarize(count = n()) |>
    filter(count == 6) |>
    pull(country_text_id) # Vector of states..
  
  ## That we can now use to filter the 2020-24 df.
  ga7 |>
    filter(country_text_id %in% demcoal) |>
    as_tibble() |>
    select(country_text_id, backslided, regime, cluster) |>
    print(n = 50)
}


gavote <- read_csv("Data/2025_03_31_ga_voting_corr1.csv") |>
  mutate(year = year(date)) |>
  filter(between(year, 1991, 2024)) |>
  dplyr::select(resolution, title, ms_code, ms_name, ms_vote, year)

splits <- gavote |>
  filter(between(year, 2020, 2024), ms_code %in% c("ISL", "GBR", "FRA", "KOR")) |>
  select(-ms_name) |>
  pivot_wider(names_from = ms_code, values_from = ms_vote) |>
  filter(!(FRA == ISL & ISL == KOR & KOR == GBR))

wf_gares <- function(begin, end, df = gavote){
  res <- df |>
    filter(between(year, begin, end)) |>
    pull(title) |>
    unique() |>
    to_tdm() |>
    freqTerms(cutoff = 200)
  res[!(names(res) %in% c("general", "assembl", "resolut", "adopt"))]
}

gares1 <- wf_gares(1991, 1994)
gares2 <- wf_gares(1995, 1999)
gares3 <- wf_gares(2000, 2004)
gares4 <- wf_gares(2005, 2009)
gares5 <- wf_gares(2010, 2024)
gares6 <- wf_gares(2015, 2019)
gares7 <- wf_gares(2020, 2024)

garesfreq <- staple_vecs7(gares1, gares2, gares3, gares4, gares5, gares6, gares7)

garesfreq |> arrange(desc(`2020-24`)) |>
  head(100) |>
  print(n = 50)

demsplits <- function(begin, end, df = gavote){
  df |>
    filter(between(year, begin, end), ms_code %in% c("DEU", "FRA", "BEL", "NLD", "GBR")) |>
    select(-ms_name) |>
    pivot_wider(names_from = ms_code, values_from = ms_vote) |>
    filter(!(DEU == FRA & FRA == BEL & BEL == NLD & NLD == GBR))
}

splits1 <- wf_gares(1991, 1994, demsplits(1991, 1994))
splits2 <- wf_gares(1995, 1999, demsplits(1995, 1999))
splits3 <- wf_gares(2000, 2004, demsplits(2000, 2004))
splits4 <- wf_gares(2005, 2009, demsplits(2005, 2009))
splits5 <- wf_gares(2010, 2014, demsplits(2010, 2014))
splits6 <- wf_gares(2015, 2019, demsplits(2015, 2019))
splits7 <- wf_gares(2020, 2024, demsplits(2020, 2024))

splitsfreq <- staple_vecs7(splits1, splits2, splits3, splits4, splits5, splits6, splits7)
splitsfreq |> arrange(desc(`2020-24`)) |>
  head(100) |>
  print(n = 30)

## Takes a while to run. Run at your own risk.
noIPdm <- function(begin, end, df = gavote){
  df |>
    filter(!grepl("palestin|israel|jerusalem", gavote$title, ignore.case = TRUE)) |>
    filter(between(year, begin, end)) |>
    dplyr::select(resolution, ms_code, ms_name, ms_vote, year) |>
    select(resolution, ms_code, ms_vote) |>
    pivot_wider(id_cols = resolution, names_from = ms_code, values_from = ms_vote) |>
    select(-resolution) |>
    get_dist_mat()
}

noIPdm(1991, 1994) |> wf_w_dm(1991, 1994) |>
  plotting() + labs(title = "Clustering without Israel-Palestine, 1991-1994")
noIPdm(1995, 1999) |> wf_w_dm(1995, 1999) |>
  plotting() + labs(title = "Clustering without Israel-Palestine, 1995-1999")
noIPdm(2000, 2004) |> wf_w_dm(2000, 2004) |>
  plotting() + labs(title = "Clustering without Israel-Palestine, 2000-2004")
noIPdm(2005, 2009) |> wf_w_dm(2005, 2009) |>
  plotting() + labs(title = "Clustering without Israel-Palestine, 2005-2009")
noIPdm(2010, 2014) |> wf_w_dm(2010, 2014) |>
  plotting() + labs(title = "Clustering without Israel-Palestine, 2010-2014")
noIPdm(2015, 2019) |> wf_w_dm(2015, 2019) |>
  plotting() + labs(title = "Clustering without Israel-Palestine, 2015-2019")
noIPdm(2020, 2024) |> wf_w_dm(2020, 2024) |>
  plotting() + labs(title = "Clustering without Israel-Palestine, 2020-2024")

demnoIP <- gavote |>
  filter(!grepl("palestin|israel|jerusalem", gavote$title, ignore.case = TRUE)) |>
  filter(between(year, 2020, 2024)) |>
  dplyr::select(resolution, ms_code, ms_name, ms_vote, year) |>
  select(resolution, ms_code, ms_vote) |>
  pivot_wider(id_cols = resolution, names_from = ms_code, values_from = ms_vote) |>
  select(-resolution) |>
  get_dist_mat()

demnoIPclst <- demnoIP |> wf_w_dm(2020, 2024)
demnoIPclst |> plotting()
