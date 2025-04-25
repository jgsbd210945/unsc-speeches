####################### GA CLUSTERING #######################
## THESE ALGORITHMS TAKE A LONG TIME. PROCEED ACCORDINGLY. ##
## The times to run each date range are posted in comments ##
####### next to their call. FULL RUN TIME: 53 minutes #######
#############################################################

# source("clustering.R")

### Clustering on rhetoric!!

pull_dm <- function(begin, end){
  gavote |> filter(between(year, begin, end)) |>
    select(resolution, ms_code, ms_vote) |>
    pivot_wider(id_cols = resolution, names_from = ms_code, values_from = ms_vote) |>
    select(-resolution) |>
    get_dist_mat()
}

wf_ga <- function(begin, end){
  pull_dm(begin, end) |>
    hclustering(groups = 8) |>
    merger_ga(begin, end)
}

wf_w_dm <- function(dist_mat, begin, end){
  hclustering(dist_mat, groups = 8) |>
    merger_ga(begin, end)
}

merger_ga <- function(to_merge, begin, end) {
  mgwreg |>
    filter(between(year, begin, end)) |>
    group_by(country_text_id) |>
    summarize(v2x_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
              v2x_regime_amb = round(mean(v2x_regime_amb, na.rm = TRUE)),
              diff_polyarchy = mean(diff_polyarchy, na.rm = TRUE),
              backslided = any(backslided, na.rm = TRUE),
              regime = names(which.max(table(regime))),
              bve = ifelse(any(bve == "backslided"), "backslided", bve)) |>
    merge(to_merge, by.x = "country_text_id", by.y = "country") |>
    mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) # scaling
}


gavote <- read_csv("Data/2025_03_31_ga_voting_corr1.csv") |>
  mutate(year = year(date)) |>
  filter(between(year, 1991, 2024)) |>
  dplyr::select(resolution, ms_code, ms_name, ms_vote, year)

g1_dm <- pull_dm(1990, 1994)
g2_dm <- pull_dm(1995, 1999)
g3_dm <- pull_dm(2000, 2004)
g4_dm <- pull_dm(2005, 2009)
g5_dm <- pull_dm(2010, 2014)
g6_dm <- pull_dm(2015, 2019)
g7_dm <- pull_dm(2020, 2024)

ga1 <- wf_w_dm(1990, 1994) # 6 minutes to run
ga2 <- wf_w_dm(1995, 1999) # 7 minutes to run
ga3 <- wf_w_dm(2000, 2004) # 5 minutes to run
ga4 <- wf_w_dm(2005, 2009) # 9 minutes to run
ga5 <- wf_w_dm(2010, 2014) # 8 minutes to run
ga6 <- wf_w_dm(2015, 2019) # 10 minutes to run
ga7 <- wf_w_dm(2020, 2024) # 8 minutes to run

g1_dm |> write_csv("GA_distmat/ga1.csv")
g2_dm |> write_csv("GA_distmat/ga2.csv")
g3_dm |> write_csv("GA_distmat/ga3.csv")
g4_dm |> write_csv("GA_distmat/ga4.csv")
g5_dm |> write_csv("GA_distmat/ga5.csv")
g6_dm |> write_csv("GA_distmat/ga6.csv")
g7_dm |> write_csv("GA_distmat/ga7.csv")