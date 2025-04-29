source("main.R")

txtstatesyears <- load("Data/UNGA Speech Corpus Data/unga_speech_corpus.RData")

txtstatesyears <- txtstatesyears |> select(stateabb, year, text) |> as_tibble()
