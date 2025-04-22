####################### GA CLUSTERING #######################
## THESE ALGORITHMS TAKE A LONG TIME. PROCEED ACCORDINGLY. ##
## The times to run each date range are posted in comments ##
####### next to their call. FULL RUN TIME: xx minutes #######
#############################################################

source("clustering.R")

wf_ga <- function(begin, end){
  gavote |> filter(between(year, begin, end)) |>
    select(resolution, ms_code, ms_vote) |>
    pivot_wider(id_cols = resolution, names_from = ms_code, values_from = ms_vote) |>
    select(-resolution) |>
    hclustering(groups = 8) |>
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

ga1 <- wf_ga(1990, 1994) # 6 minutes to run
ga2 <- wf_ga(1995, 1999) # 7 minutes to run
ga3 <- wf_ga(2000, 2004) ###
ga4 <- wf_ga(2005, 2009) # 9 minutes to run
ga5 <- wf_ga(2010, 2014) # 8 minutes to run
ga6 <- wf_ga(2015, 2019) # 10 minutes to run
ga7 <- wf_ga(2020, 2024) # 8 minutes to run

ga1 |> plotting()
ga2 |> plotting()
ga3 |> plotting()
ga4 |> plotting()
ga5 |> plotting()
ga6 |> plotting()
ga7 |> plotting()
