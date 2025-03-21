source("main.R")


speeches <- read_tsv("Data/speeches.tsv") |> filter(year > 1990, state == 1)


speeches$state <- ifelse(is.na(countrycode(speeches$affiliation, origin = 'country.name', destination = 'iso3c')),
                         speeches$affiliation,
                         countrycode(speeches$affiliation, origin = 'country.name', destination = 'iso3c'))

speeches$topic <- tolower(speeches$topic) |>
  removeWords(stopwords()) |>
  stripWhitespace()

speeches$topic <- gsub(" |-|—|situation", "", speeches$topic)

gen_speech <- speeches |>
  select(meeting_num, year, month, day, topic, affiliation, speech, state) |>
  merge(mgwreg, by.x = c("year", "state"), by.y = c("year", "country_text_id"), all.x = TRUE) |>
  as_tibble()

# Rhetoric
wf <- function(term, lowyear = 1991) {
  pullspeech(term, lowyear) |>
    to_tdm()
}
pullspeech <- function(term, lowyear) {
  gen_speech |>
    filter(grepl(term, topic), year >= lowyear) |>
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
rmfreq <- rmfreq <= 1
freqbyregime <- freqbyregime[,!rmfreq]
freqbyregime <- freqbyregime %>% replace(is.na(.), 0) # Need the old pipe for this!

specific_terms <- c("regime", "norm", "normat", "standard", "right", "liber", "neoliber", "forc", "peac")

normnum <- cbind(freqbyregime[specific_terms], freqbyregime[grepl("interv|interfer|selfdet|sover|rights|humanright|humanitarian|peace", colnames(freqbyregime))])
normnum <- normnum |>
  mutate(tot_norm = rowSums(select(normnum, matches("^norm")), na.rm = TRUE),
         tot_sovereign = rowSums(select(normnum, matches("^sovereign")), na.rm = TRUE),
         tot_peacekeep = rowSums(select(normnum, matches("^peacekeep")), na.rm = TRUE),
         tot_peacebuild = rowSums(select(normnum, matches("^peacebuild")), na.rm = TRUE),
         tot_interv = rowSums(select(normnum, matches("^interv")), na.rm = TRUE),
         tot_humanitarian = rowSums(select(normnum, matches("^humanitarian")), na.rm = TRUE),
         tot_peace = rowSums(select(normnum, matches("^peac")), na.rm = TRUE)) |>
  select(-matches("^norm|^sovereign|^peacekeep|^peacebuild|^interv|^humanitarian")) |>
  data.table::transpose(keep.names = "term", make.names = "regime") |>
  as_tibble() |>
  mutate(bve_erosion = dem_erosion / entr_illib,
         bve_revert = dem_revert / entr_auto) |>
  arrange(desc(bve_erosion)) 


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
         tot_humanitarian = rowSums(select(normlang, matches("^humanitarian")), na.rm = TRUE),
         tot_peace = rowSums(select(normlang, matches("^peac")), na.rm = TRUE)) |>
  select(-matches("^norm|^sovereign|^peacekeep|^peacebuild|^interv|^humanitarian")) |>
  data.table::transpose(keep.names = "term", make.names = "regime") |>
  as_tibble() |>
  mutate(bve_erosion = dem_erosion / entr_illib,
         bve_revert = dem_revert / entr_auto) |>
  arrange(desc(bve_erosion)) 
  
normlang |> rowwise() |> mutate(bvd = mean(c(dem_erosion, dem_revert), na.rm = TRUE) / entr_dem,
                   evd = mean(c(entr_illib, entr_auto), na.rm = TRUE) / entr_dem,
                   bnevd = mean(c(dem_erosion, dem_revert, entr_illib, entr_auto), na.rm = TRUE) / entr_dem) |>
  select(term, bvd, evd, bnevd) |> arrange(desc(bnevd)) |> print(n = 33)

filter(normlang, term != "tot_peace") |>
  select(-term) |>
  lapply(sum)

normpct <- cbind(pctbyregime[specific_terms], pctbyregime[grepl("interv|interfer|selfdet|sover|rights|humanright|humanitarian|peace", colnames(pctbyregime))])
normpct <- normpct |>
  data.table::transpose(keep.names = "term", make.names = "regime") |>
  as_tibble() |>
  mutate(bve_erosion = dem_erosion / entr_illib,
         bve_revert = dem_revert / entr_auto) |>
  arrange(desc(bve_erosion))


# Case Studies
cyp_tdm <- wf("cyprus", 2000)
cypfreq <- freqTerms(cyp_tdm)
cyppct <- (cypfreq / sum(cypfreq)) * 10000

wsah_tdm <- wf("westernsahara", 2020)
wsahfreq <- freqTerms(wsah_tdm)
wsahpct <- (wsahfreq / sum(wsahfreq)) * 10000

rusuk_tdm <- wf("russia|ukraine", 2014)
rusukfreq <- freqTerms(rusuk_tdm)
rusukpct <- (rusukfreq / sum(rusukfreq)) * 10000


casesfreq <- mergeVectors(cypfreq, wsahfreq, rusukfreq) |>
  as_tibble() |>
  mutate(case = c("Cyprus", "Western Sahara", "Russia-Ukraine"),
         .before = 1)
rmcsfreq <- apply(casesfreq, 2, \(col) sum(!is.na(col)))
rmcsfreq <- rmcsfreq <= 1
casesfreq <- casesfreq[,!rmcsfreq]
casesfreq <- casesfreq %>% replace(is.na(.), 0)

casespct <- mergeVectors(cyppct, wsahpct, rusukpct) |>
  as_tibble() |>
  mutate(state = c("Cyprus", "Western Sahara", "Russia-Ukraine"),
         .before = 1)
rmcspct <- apply(casespct, 2, \(col) sum(!is.na(col)))
rmcspct <- rmcspct <= 1
casespct <- casespct[,!rmcspct]
casespct <- casespct %>% replace(is.na(.), 0)

ukrmeet <- gen_speech |> filter(grepl("ukraine", topic), year >= 2014)
r_ukr_tdm <- ukrmeet |>
  filter(grepl("Russia", affiliation)) |>
  pull(speech) |>
  to_tdm() |>
  freqTerms() |>
  mergeVectors() |>
  as_tibble()
r_ukr_tdm <- (r_ukr_tdm / sum(r_ukr_tdm)) * 10000
normr_ukr <- cbind(r_ukr_tdm[c("norm", "standard", "right", "liber", "forc", "peac")], r_ukr_tdm[grepl("interv|interfer|selfdet|sover|rights|humanright|humanitarian|peace", colnames(r_ukr_tdm))])
