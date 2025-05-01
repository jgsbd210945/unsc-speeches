####################### GA CLUSTERING #######################
## THESE ALGORITHMS TAKE A LONG TIME. PROCEED ACCORDINGLY. ##
## The times to run each date range are posted in comments ##
####### next to their call. FULL RUN TIME: 53 minutes #######
#############################################################

## For the most part this is a one-time run so I can just use the .csv files in vote-clustering.

source("cluster-setup.R")

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

# Getting them into CSVable format

g1_dm <- g1_dm |> as.matrix()
g1_dm[upper.tri(g1_dm, diag = FALSE)] <- FALSE

g2_dm <- g2_dm |> as.matrix()
g2_dm[upper.tri(g2_dm, diag = FALSE)] <- FALSE

g3_dm <- g3_dm |> as.matrix()
g3_dm[upper.tri(g3_dm, diag = FALSE)] <- FALSE

g4_dm <- g4_dm |> as.matrix()
g4_dm[upper.tri(g4_dm, diag = FALSE)] <- FALSE

g5_dm <- g5_dm |> as.matrix()
g5_dm[upper.tri(g5_dm, diag = FALSE)] <- FALSE

g6_dm <- g6_dm |> as.matrix()
g7_dm[upper.tri(g6_dm, diag = FALSE)] <- FALSE

g7_dm <- g7_dm |> as.matrix()
g7_dm[upper.tri(g7_dm, diag = FALSE)] <- FALSE

g1_dm |> as.data.frame() |> write_csv("GA_distmat/ga1.csv")
g2_dm |> as.data.frame() |> write_csv("GA_distmat/ga2.csv")
g3_dm |> as.data.frame() |> write_csv("GA_distmat/ga3.csv")
g4_dm |> as.data.frame() |> write_csv("GA_distmat/ga4.csv")
g5_dm |> as.data.frame() |> write_csv("GA_distmat/ga5.csv")
g6_dm |> as.data.frame() |> write_csv("GA_distmat/ga6.csv")
g7_dm |> as.data.frame() |> write_csv("GA_distmat/ga7.csv")


