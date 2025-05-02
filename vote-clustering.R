source("voting-setup.R")
source("cluster-setup.R")

# Clustering
wf_scvote <- function(begin, end, ngrp){
  cut |> filter(between(year, begin, end)) |>
    dplyr::select(-year)|>
    get_dist_mat() |>
    hclustering(groups = ngrp) |>
    merger(begin, end)
}

cut <- cleaned_voting |> dplyr::select(-c(meeting_record:date, title:link, chil_es, SOM))

hc1 <- wf_scvote(1990, 1994, 4)
hc2 <- wf_scvote(1995, 1999, 4)
hc3 <- wf_scvote(2000, 2004, 4)
hc4 <- wf_scvote(2005, 2009, 4)
hc5 <- wf_scvote(2010, 2014, 4)
hc6 <- wf_scvote(2015, 2019, 4)
hc7 <- wf_scvote(2020, 2024, 4)

hc1 |> plotting()
hc2 |> plotting()
hc3 |> plotting()
hc4 |> plotting()
hc5 |> plotting()
hc6 |> plotting()
hc7 |> plotting()

### GA Area ###
wf_w_dm <- function(dist_mat, begin, end){
  hclustering(dist_mat, groups = 8) |>
    merger_ga(begin, end)
}

ga1 <- read_csv("GA_distmat/ga1.csv") |> as.dist() |> wf_w_dm(1990, 1994)
ga2 <- read_csv("GA_distmat/ga2.csv") |> as.dist() |> wf_w_dm(1995, 1999)
ga3 <- read_csv("GA_distmat/ga3.csv") |> as.dist() |> wf_w_dm(2000, 2004)
ga4 <- read_csv("GA_distmat/ga4.csv") |> as.dist() |> wf_w_dm(2005, 2009)
ga5 <- read_csv("GA_distmat/ga5.csv") |> as.dist() |> wf_w_dm(2010, 2014)
ga6 <- read_csv("GA_distmat/ga6.csv") |> as.dist() |> wf_w_dm(2015, 2019)
ga7 <- read_csv("GA_distmat/ga7.csv") |> as.dist() |> wf_w_dm(2020, 2024)

ga1 |> plotting()
ga2 |> plotting()
ga3 |> plotting()
ga4 |> plotting()
ga5 |> plotting()
ga6 |> plotting()
ga7 |> plotting()

read_csv("GA_distmat/ga4.csv") |>
  as.dist() |>
  hclustering(groups = 7) |> # Only after doing 8 clusters is there actually a split between the bottom (autocratic) and the massive group in the middle!
  merger_ga(2005, 2009) |>   # This is 6 clusters for ga5 from 2010 to 2014.
  plotting()


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
  geom_point(size = 2)

clstGBR <- checkstate("GBR") |> as_tibble() |> relocate(yr, .before = 1)
clstGBR |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2)

clstUSA <- checkstate("USA") |> as_tibble() |> relocate(yr, .before = 1)
clstUSA |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2)

clstCHN <- checkstate("CHN") |> as_tibble() |> relocate(yr, .before = 1)
clstCHN |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2)

clstRUS <- checkstate("RUS") |> as_tibble() |> relocate(yr, .before = 1)
clstRUS |> ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2)

### General Distribution
mgwreg |> mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) |>
  ggplot(aes(x = diff_polyarchy, y = v2x_polyarchy)) +
  geom_point(size = 2)