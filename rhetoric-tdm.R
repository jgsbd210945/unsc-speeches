## Rhetoric TermDocumentMatricies/counting

source("rhetoric-setup.R")

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

ga_speeches <- ga_speeches |>
  merge(mgwreg) |>
  as_tibble() |>
  rename(state = country_text_id) |>
  select(year, state, speech, regime, bve)

gen_speech <- gen_speech |>
  select(year, state, speech, regime, bve) |>
  rbind(ga_speeches)

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

specific_terms <- c("regime", "norm", "normat", "standard", "right", "liber", "neoliber", "forc", "peac",
                    "protect", "civilianprotect", "childprotect", "protection", "protectionof", "protectionofcivilian", "protector") # Since some "protect" stems aren't dealing with stuff like R2P.

normnum <- cbind(freqbyregime[specific_terms], freqbyregime[grepl("interv|interfer|selfdet|sover|rights|humanright|humanitarian|peace", colnames(freqbyregime))])
normnum <- normnum |>
  mutate(tot_norm = rowSums(select(normnum, matches("^norm")), na.rm = TRUE),
         tot_sovereign = rowSums(select(normnum, matches("^sovereign")), na.rm = TRUE),
         tot_peacekeep = rowSums(select(normnum, matches("^peacekeep")), na.rm = TRUE),
         tot_peacebuild = rowSums(select(normnum, matches("^peacebuild")), na.rm = TRUE),
         tot_interv = rowSums(select(normnum, matches("^interv")), na.rm = TRUE),
         tot_humanitarian = rowSums(select(normnum, matches("^humanitarian")), na.rm = TRUE),
         tot_peace = rowSums(select(normnum, matches("^peac")), na.rm = TRUE),
         tot_prot = rowSums(select(normnum, matches("protect")), na.rm = TRUE)) |>
  select(-matches("^norm|^sovereign|^peacekeep|^peacebuild|^interv|^humanitarian|protect")) |>
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
         tot_peace = rowSums(select(normlang, matches("^peac")), na.rm = TRUE),
         tot_prot = rowSums(select(normlang, matches("protect")), na.rm = TRUE)) |>
  select(-matches("^norm|^sovereign|^peacekeep|^peacebuild|^interv|^humanitarian|protect")) |>
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

normbyreg <- normpct |>
  data.table::transpose(keep.names = "regime", make.names = "term") |>
  as_tibble()

rhet_cases <- normlang |>
  select(-matches("^bve")) |>
  data.table::transpose(keep.names = "regime", make.names = "term") |>
  as_tibble() |>
  mutate(Regime = c("Democratic Erosion", "Entrenched Grey Area Regime", "Democratic Reversion", "Entrenched Autocracy", "Entrenched Democracy"))

## Gen. Bar Chart
normlang |>
  select(-matches("^bve")) |>
  filter(grepl("^tot|^nonint|^self|^int|^forc$", normlang$term)) |>
  mutate(across(dem_erosion:entr_dem, \(x) x / entr_dem),
         term = c("nonintervent", "humanitarian", "interfer", "sovereign", "prot", "norm", "interv", "forc", "peacekeep", "peacebuild", "peace", "noninterfer", "selfdetermin")) |>
  pivot_longer(!term, names_to = "regime", values_to = "pct") |>
  mutate(
    term = factor(term),
    regime = factor(regime, levels = c("entr_dem", "dem_erosion", "entr_illib", "dem_revert", "entr_auto"))) |>
  ggplot(aes(x = factor(term), y = log(pct), fill = factor(regime))) +
    geom_col(position = "dodge2") +
  scale_fill_manual(labels = c("Entrenched Democracy", "Democratic Erosion", "Grey Area Regime", "Democratic Reversion", "Entrenched Autocracy"),
                    values = c("#0072B2", "#CC7987", "#999999", "#800080", "#D55E00"),
                    name = "Regime Type") +
  labs(x = "Term",
       y = "log(Proportion to Entrenched Democracy)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))


rhet_cases |>
  ggplot(aes(x = tot_prot, y = reorder(Regime, -tot_prot))) +
  geom_col() +
  ylab("Regime Type") +
  xlab("Uses of Words Related to R2P per 10,000 Words") +
  labs(title = paste("Usage of Words related to R2P by Regime"))

rhet_cases |>
  ggplot(aes(x = tot_sovereign, y = reorder(Regime, -tot_sovereign))) +
  geom_col() +
  ylab("Regime Type") +
  xlab("Uses of Sovereignty per 10,000 Words") +
  labs(title = paste("Usage of Sovereignty by Regime"))

rhet_cases |>
  ggplot(aes(x = selfdetermin, y = reorder(Regime, -selfdetermin))) +
  geom_col() +
  ylab("Regime Type") +
  xlab("Uses of Self-Determination per 10,000 Words") +
  labs(title = paste("Usage of Self-Determination by Regime"))

rhet_cases |>
  mutate(interv = nonintervent + tot_interv) |>
  ggplot(aes(x = interv, y = reorder(Regime, -interv))) +
  geom_col() +
  ylab("Regime Type") +
  xlab("Uses of Intervention/Non-Intervention per 10,000 Words") +
  labs(title = paste("Usage of Intervention/Non-Intervention by Regime"))

rhet_cases |>
  mutate(interfer = noninterfer + interfer) |>
  ggplot(aes(x = interfer, y = reorder(Regime, -interfer))) +
  geom_col() +
  ylab("Regime Type") +
  xlab("Uses of Interference/Non-Interference per 10,000 Words") +
  labs(title = paste("Usage of Interference/Non-Interference by Regime"))
