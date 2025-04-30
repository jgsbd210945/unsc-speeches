source("main.R")

######################### ONE-TIME SETUP #########################
## Since there are a lot of files, the following one-time setup ##
## has been commented out. I put it in a csv to make it quicker ##
## to read into my dataframe for future use. #####################
##################################################################

#library(easyr)

#files <- list.files(path="GA_speech/", pattern=".txt", full.names=TRUE, recursive=TRUE)

#speeches <- files |>
#  set_names() |>
#  map_df(~ read.txt(.x), .id = "source_file") |>
#  pivot_longer(everything(), names_to = "filename", values_to = "speech") |>
#  as_tibble() |>
#  separate_wider_delim(filename, delim = "/",
#                       names = c("XX", "session", "filename")) |>
#  mutate(year = as.numeric(right(session, 4)),
#         country_text_id = left(filename, 3)) |>
#  select(speech:country_text_id) |>
#  relocate(speech, .after = country_text_id)

#speeches |> write_csv("GA_Speech/gaspeeches.csv")

speeches <- read_csv("GA_Speech/gaspeeches.csv")

garh1 <- wf_rhet(speeches, 8, 1990, 1994)
garh2 <- wf_rhet(speeches, 8, 1995, 1999)
garh3 <- wf_rhet(speeches, 8, 2000, 2004)
garh4 <- wf_rhet(speeches, 8, 2005, 2009)
garh5 <- wf_rhet(speeches, 8, 2010, 2014)
garh6 <- wf_rhet(speeches, 8, 2015, 2019)
garh7 <- wf_rhet(speeches, 8, 2020, 2024)

garh1 |> plotting()
garh2 |> plotting()
garh3 |> plotting()
garh4 |> plotting()
garh5 |> plotting()
garh6 |> plotting()
garh7 |> plotting()

# Seems the better dataset since every(ish) state is represented.

cortest <- function(rhet, vot){
  merge(rhet, vot, by.x = "country_text_id", by.y = "country") |>
    select(cluster.x, cluster.y) |> cor()
}

gvot1 <- read_csv("GA_distmat/ga1.csv") |> as.dist() |> hclustering(groups = 8)
gvot2 <- read_csv("GA_distmat/ga2.csv") |> as.dist() |> hclustering(groups = 8)
gvot3 <- read_csv("GA_distmat/ga3.csv") |> as.dist() |> hclustering(groups = 8)
gvot4 <- read_csv("GA_distmat/ga4.csv") |> as.dist() |> hclustering(groups = 8)
gvot5 <- read_csv("GA_distmat/ga5.csv") |> as.dist() |> hclustering(groups = 8)
gvot6 <- read_csv("GA_distmat/ga6.csv") |> as.dist() |> hclustering(groups = 8)
gvot7 <- read_csv("GA_distmat/ga7.csv") |> as.dist() |> hclustering(groups = 8)

cortest(garh1, gvot1)
cortest(garh2, gvot2)
cortest(garh3, gvot3)
cortest(garh4, gvot4)
cortest(garh5, gvot5)
cortest(garh6, gvot6)
cortest(garh7, gvot7)
## That's strange, it's almost impressive how *not* correlated they are.

