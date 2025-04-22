source("main.R")
library(unvotes)

gaspeech <- load("Data/UNGA Speech Corpus Data/unga_speech_corpus.RData")

gaword <- unga_word_vectors |> transpose() |> as_tibble()
