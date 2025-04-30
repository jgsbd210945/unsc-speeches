source("main.R")

load("Data/UNGA Speech Corpus Data/unga_speech_corpus.RData") # Loads to "txtstatesyears"

txtstatesyears <- txtstatesyears |>
  select(stateabb, year, text) |>
  as_tibble() |>
  rename("speech" = text,
         "country_text_id" = stateabb)


garhet1 <- wf_rhet(txtstatesyears, 8, 1990, 1994)
garhet2 <- wf_rhet(txtstatesyears, 8, 1995, 1999)
garhet3 <- wf_rhet(txtstatesyears, 8, 2000, 2004)
garhet4 <- wf_rhet(txtstatesyears, 8, 2005, 2009)
garhet5 <- wf_rhet(txtstatesyears, 8, 2010, 2014)
garhet6 <- wf_rhet(txtstatesyears, 8, 2015, 2019)


garhet1 |> plotting()
garhet2 |> plotting()
garhet3 |> plotting()
garhet4 |> plotting()
garhet5 |> plotting()
garhet6 |> plotting()

## Takeaway: Seemingly much more "random" so kinda cutting a bunch of states...anything I can do?