source("main.R")

# Finding highest/lowest scores in polyarchy to filter
mgdata |> filter(country_text_id %in% c("FRA", "USA", "CHN", "RUS", "GBR")) |>
  group_by(year) |>
  filter(v2x_polyarchy == max(v2x_polyarchy)) |>
  ungroup() |>
  group_by(country_name) |>
  summarize(count = n()) # France all but 10 years, wasn't far behind in the years it wasn't max though.

mgdata |> filter(country_text_id %in% c("FRA", "USA", "CHN", "RUS", "GBR")) |>
  group_by(year) |>
  filter(v2x_polyarchy == min(v2x_polyarchy)) |>
  ungroup() |>
  group_by(country_name) |>
  summarize(count = n()) # China every year since 1991!


# Graphing
sumvotes_year <- function(df, merger = mgwreg) {
  df |> pivot_longer(!meeting_record:link,
                     names_to = "country",
                     values_to = "vote",
                     values_drop_na = TRUE) |>
    select(date, resolution, country, vote) |>
    mutate(year = year(date)) |>
    
    group_by(year, country) |>
    summarize(total = n(),
              yes = sum(vote == "Y") / total,
              no = sum(vote == "N") / total,
              abstain = sum(vote == "A") / total,
              missing = sum(vote == "X") / total,
              .groups = 'drop') |>
    
    merge(merger, by.x = c("year", "country"), by.y = c("year", "country_text_id")) |>
    as_tibble()
}
graph_polyarchy <- function(df, ylower = 0.8){
  ggplot(df, aes(v2x_polyarchy, y = yes)) +
    geom_point(aes(color = regime)) +
    ylim(ylower, 1) +
    geom_smooth()
}
graph_diff_polyarchy <- function(df, xlower = -0.1, xupper = 0.025, ylower = 0.7) {
  ggplot(df, aes(diff_polyarchy, y = yes)) +
    geom_point(aes(color = regime)) +
    xlim(xlower, xupper) +
    ylim(ylower, 1) +
    geom_smooth()
}


# Yes/No/etc.
voterate <- sumvotes_year(voting)
graph_polyarchy(voterate)
graph_diff_polyarchy(voterate)

# Match with France?
reference = "FRA"
compare_votes <- function(col){
  case_when(
    col == "X" ~ "X",
    col == voting[[reference]] ~ "Y",
    col == "A" ~ "A",
    col != voting[[reference]] ~ "N",
    TRUE ~ NA
  )
}

matchFR <- voting |>
  mutate(across(!meeting_record:link, compare_votes)) |>
  sumvotes_year()

graph_polyarchy(matchFR)
graph_diff_polyarchy(matchFR)

# China
reference = "CHN"
matchCN <- voting |>
  mutate(across(!meeting_record:link, compare_votes)) |>
  sumvotes_year()

graph_polyarchy(matchCN)
graph_diff_polyarchy(matchCN)

# Backsliding to entrenched Directly
mgwreg$bve <- ifelse(grepl("backslide_", mgwreg$regime), "backsliding",
                     ifelse(grepl("dem", mgwreg$regime), "democratic",
                            "entrenched"))
bve_mg <- sumvotes_year(voting)

bve_mg |> ggplot(aes(x = bve, y = yes)) +
  geom_boxplot() +
  ylim(0.65, 1)
bve_mg |> ggplot(aes(x = regime, y = yes)) +
  geom_boxplot() +
  ylim(0.65, 1)