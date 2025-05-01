source("voting-setup.R")
source("cluster-setup.R")

# Clustering
wf_scvote <- function(begin, end){
  cut |> filter(between(year, begin, end)) |>
    dplyr::select(-year)|>
    get_dist_mat() |>
    hclustering(groups = 4) |>
    merger(begin, end)
}

cut <- cleaned_voting |> dplyr::select(-c(meeting_record:date, title:link, chil_es, SOM))

hc1 <- wf_scvote(1990, 1994)
hc2 <- wf_scvote(1995, 1999)
hc3 <- wf_scvote(2000, 2004)
hc4 <- wf_scvote(2005, 2009)
hc5 <- wf_scvote(2010, 2014)
hc6 <- wf_scvote(2015, 2019)
hc7 <- wf_scvote(2020, 2024)

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
