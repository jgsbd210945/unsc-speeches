source("main.R")


speeches <- read_tsv("Data/speeches.tsv") |> filter(year > 1990, state == 1)


speeches$state <- ifelse(is.na(countrycode(speeches$affiliation, origin = 'country.name', destination = 'iso3c')),
                         speeches$affiliation,
                         countrycode(speeches$affiliation, origin = 'country.name', destination = 'iso3c'))

gen_speech <- speeches |>
  select(meeting_num, year, month, day, topic, affiliation, speech, state) |>
  merge(mgwreg, by.x = c("year", "state"), by.y = c("year", "country_text_id"), all.x = TRUE) |>
  as_tibble()

# Rhetoric
wf <- function(term, loweryear, upperyear) {
  pullspeech(term, loweryear, upperyear) |>
    to_tdm()
}
pullspeech <- function(term, loweryear, upperyear) {
  gen_speech |>
    filter(grepl(term, state), between(year, loweryear, upperyear)) |>
    pull(speech)
}
to_tdm <- function(col){
  Corpus(VectorSource(col)) |>
    tm_map(removePunctuation, ucp = TRUE) |>
    TermDocumentMatrix(control = list(
      stopwords = TRUE,
      tolower = TRUE,
      stemming = TRUE,
      removeNumbers = TRUE,
      bounds = list(global = c(3, Inf))
    ))
}
freqTerms <- function(tdm, cutoff = Inf){
  as.matrix(tdm) |>
    apply(1, sum) |>
    sort(decreasing = TRUE) |>
    head(cutoff)
}

erode_tdm <- gen_speech |> filter(regime == "backslide_erode") |>
  pull(speech) |>
  to_tdm()
erodefreq <- freqTerms(erode_tdm)
erodepct <- (erodefreq / sum(erodefreq)) * 10000

revert_tdm <- gen_speech |> filter(regime == "backslide_revert") |>
  pull(speech) |>
  to_tdm()
revertfreq <- freqTerms(revert_tdm)
revertpct <- (revertfreq / sum(revertfreq)) * 10000

auto_tdm <- gen_speech |> filter(regime == "entrenched_auto") |>
  pull(speech) |>
  to_tdm()
autofreq <- freqTerms(auto_tdm)
autopct <- (autofreq / sum(autofreq)) * 10000

illib_tdm <- gen_speech |> filter(regime == "entrenched_illib") |>
  pull(speech) |>
  to_tdm()
illibfreq <- freqTerms(illib_tdm)
illibpct <- (illibfreq / sum(illibfreq)) * 10000

dem_tdm <- gen_speech |> filter(regime == "entrenched_dem") |>
  pull(speech) |>
  to_tdm()
demfreq <- freqTerms(dem_tdm)
dempct <- (demfreq / sum(demfreq)) * 10000

# Percentages are individual frequency / total number of non-stopwords used * 10000
# i.e., if something has a value of 100, it appears in 1% of all words.

freqbyregime <- mergeVectors(erodefreq, illibfreq, revertfreq, autofreq, demfreq) |>
  as_tibble() |>
  mutate(regime = c("dem_erosion", "entr_illib", "dem_revert", "entr_auto", "entr_dem"),
         .before = 1)
rmfreq <- apply(freqbyregime, 2, \(col) sum(!is.na(col)))
freqbyregime <- freqbyregime[,!rmfreq]
freqbyregime <- freqbyregime %>% replace(is.na(.), 0) # Need the old pipe for this!

specific_terms <- c("regime", "norm", "normat", "standard", "right", "liber", "neoliber", "forc", "peac")

normnum <- cbind(freqbyregime[specific_terms], freqbyregime[grepl("interv|interfer|selfdet|sover|rights|humanright|humanitarian|peace", colnames(pctbyregime))])
normnum <- normnum |>
  mutate(tot_norm = rowSums(select(normnum, matches("^norm")), na.rm = TRUE),
         tot_sovereign = rowSums(select(normnum, matches("^sovereign")), na.rm = TRUE),
         tot_peacekeep = rowSums(select(normnum, matches("^peacekeep")), na.rm = TRUE),
         tot_peacebuild = rowSums(select(normnum, matches("^peacebuild")), na.rm = TRUE),
         tot_interv = rowSums(select(normnum, matches("^interv")), na.rm = TRUE),
         tot_humanitarian = rowSums(select(normnum, matches("^humanitarian")), na.rm = TRUE)) |>
  select(-matches("^norm|^sovereign|^peacekeep|^peacebuild|^interv|^humanitarian")) |>
  data.table::transpose(keep.names = "term", make.names = "regime") |>
  as_tibble()
  

pctbyregime <- mergeVectors(erodepct, illibpct, revertpct, autopct, dempct) |>
  as_tibble() |>
  mutate(regime = c("dem_erosion", "entr_illib", "dem_revert", "entr_auto", "entr_dem"),
         .before = 1)
rmpct <- apply(pctbyregime, 2, \(col) sum(!is.na(col)))
rmpct <- rmpct <= 1
pctbyregime <- pctbyregime[,!rmpct]
pctbyregime <- pctbyregime %>% replace(is.na(.), 0)

normlang <- cbind(pctbyregime[specific_terms], pctbyregime[grepl("interv|interfer|selfdet|sover|rights|humanright|humanitarian|peace", colnames(pctbyregime))])

normlang <- normlang |>
  mutate(tot_norm = rowSums(select(normlang, matches("^norm")), na.rm = TRUE),
         tot_sovereign = rowSums(select(normlang, matches("^sovereign")), na.rm = TRUE),
         tot_peacekeep = rowSums(select(normlang, matches("^peacekeep")), na.rm = TRUE),
         tot_peacebuild = rowSums(select(normlang, matches("^peacebuild")), na.rm = TRUE),
         tot_interv = rowSums(select(normlang, matches("^interv")), na.rm = TRUE),
         tot_humanitarian = rowSums(select(normlang, matches("^humanitarian")), na.rm = TRUE)) |>
  select(-matches("^norm|^sovereign|^peacekeep|^peacebuild|^interv|^humanitarian")) |>
  data.table::transpose(keep.names = "term", make.names = "regime") |>
  as_tibble() |>
  mutate(bve_erosion = dem_erosion / entr_illib,
         bve_revert = dem_revert / entr_auto) |>
  arrange(desc(bve_erosion)) 
  
  
# Case Studies
pol_tdm <- wf("POL", 2019, 2019)
polfreq <- freqTerms(pol_tdm)
polpct <- (polfreq / sum(polfreq)) * 10000

indo_tdm <- wf("IDN", 2019, 2019)
indofreq <- freqTerms(indo_tdm)
indopct <- (indofreq / sum(indofreq)) * 10000

mex_tdm <- wf("MEX", 2021, 2022)
mexfreq <- freqTerms(mex_tdm)
mexpct <- (mexfreq / sum(mexfreq)) * 10000

ken_tdm <- wf("KEN", 2021, 2022)
kenfreq <- freqTerms(ken_tdm)
kenpct <- (kenfreq / sum(kenfreq)) * 10000

rus_tdm <- wf("RUS", 2000, 2024)
rusfreq <- freqTerms(rus_tdm)
ruspct <- (rusfreq / sum(rusfreq)) * 10000

chn_tdm <- wf("CHN", 2000, 2024)
chnfreq <- freqTerms(chn_tdm)
chnpct <- (chnfreq / sum(chnfreq)) * 10000

casesfreq <- mergeVectors(polfreq, indofreq, mexfreq, kenfreq, rusfreq, chnfreq) |>
  as_tibble() |>
  mutate(state = c("Poland", "Indonesia", "Mexico", "Kenya", "Russia", "China"),
         .before = 1)
rmcsfreq <- apply(casesfreq, 2, \(col) sum(!is.na(col)))
rmcsfreq <- rmcsfreq <= 1
casesfreq <- casesfreq[,!rmcsfreq]
casesfreq <- casesfreq %>% replace(is.na(.), 0)

casespct <- mergeVectors(polpct, indopct, mexpct, kenpct, ruspct, chnpct) |>
  as_tibble() |>
  mutate(state = c("Poland", "Indonesia", "Mexico", "Kenya", "Russia", "China"),
         .before = 1)
rmcspct <- apply(casespct, 2, \(col) sum(!is.na(col)))
rmcspct <- rmcspct <= 1
casespct <- casespct[,!rmcspct]
casespct <- casespct %>% replace(is.na(.), 0)
