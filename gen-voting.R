## Mainly for finding general counting statistics with the voting data.
## There's a good chance I don't need most of this, but this should plot the general
## Yes/No/Abstain.

source("voting-setup.R")

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
pivot_votes <- function(df){
  df |> pivot_longer(!meeting_record:link,
                     names_to = "country",
                     values_to = "vote",
                     values_drop_na = TRUE) |>
    select(date, year, resolution, country, vote, meeting_topic, title)
}

sumvotes_year <- function(df, merger = mgwreg) {
  df |> pivot_votes() |>
    group_by(year, country) |>
    reframe(total = n(),
              yes = sum(vote == "Y") / total,
              no = sum(vote == "N") / total,
              abstain = sum(vote == "A") / total,
              missing = sum(vote == "X") / total) |>
    merge(merger, by.x = c("year", "country"), by.y = c("year", "country_text_id")) |>
    as_tibble()
}

graph_polyarchy <- function(df, ylower = 0.8){
  ggplot(df, aes(v2x_polyarchy, yes, color = regime)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    ylim(ylower, 1)
}

graph_diff_polyarchy <- function(df, xlower = -0.08, xupper = 0.01, ylower = 0.8) {
  ggplot(df, aes(diff_polyarchy, yes)) +
    geom_point(aes(color = regime)) +
    geom_smooth(method = 'lm') +
    xlim(xlower, xupper) +
    ylim(ylower, 1)
}


# Yes/No/etc.
voterate <- sumvotes_year(voting)
graph_polyarchy(voterate) +
  labs(color = "Regime") +
  scale_color_manual(values = color_scheme,
                     labels = c("Democratic Erosion",
                                "Democratic Reversion",
                                "Entrenched Autocracy",
                                "Entrenched Democracy",
                                "Entrenched Illiberal"))+
  xlab("Electoral Democracy Score") +
  ylab("Rate of Votes In Favor of Resolutions per Year")
graph_diff_polyarchy(voterate) +
  scale_color_manual(values = color_scheme,
                     labels = c("Democratic Erosion",
                                "Democratic Reversion",
                                "Entrenched Autocracy",
                                "Entrenched Democracy",
                                "Entrenched Illiberal"))+
  xlab("Difference in Electoral Democracy Score") +
  ylab("Rate of Votes in Favor of Resolutions per Year")
ggplot(voterate, aes(v2x_polyarchy, no, color = regime)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(0, 0.075) +
  labs(color = "Regime") +
  scale_color_manual(values = color_scheme,
                     labels = c("Democratic Erosion",
                                "Democratic Reversion",
                                "Entrenched Autocracy",
                                "Entrenched Democracy",
                                "Entrenched Illiberal"))+
  xlab("Electoral Democracy Score") +
  ylab("Rate of Votes Against of Resolutions per Year")


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
bve_mg <- sumvotes_year(voting)

bve_mg |> ggplot(aes(x = bve, y = yes)) +
  geom_boxplot() +
  ylim(0.65, 1)
bve_mg |> ggplot(aes(x = regime, y = yes)) +
  geom_boxplot() +
  ylim(0.65, 1)

bve_mg |> ggplot(aes(v2x_polyarchy, y = yes, color = bve)) +
  geom_point() +
  geom_smooth() +
  ylim(0.65, 1)

bve_mg |> ggplot(aes(diff_polyarchy, y = yes)) +
  geom_point(aes(color = bve)) +
  geom_smooth(method = "lm") +
  ylim(0.65, 1) +
  xlim(-0.06, 0.01)

bve_mg |> ggplot(aes(diff_polyarchy, y = yes)) +
  facet_wrap(vars(bve)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0.65, 1) +
  xlim(-0.06, 0.01)


byreg_votes <- cleaned_voting |> pivot_votes() |>
  merge(mgwreg, by.x = c("year", "country"), by.y = c("year", "country_text_id"),
        all.x = TRUE) |>
  as_tibble() |>
  filter(!is.na(regime)) |>
  group_by(meeting_topic, regime, year) |>
  reframe(total = n(),
            yes = sum(vote == "Y") / total,
            no = sum(vote == "N") / total,
            abstain = sum(vote == "A") / total,
            missing = sum(vote == "X") / total) |>
  filter(!grepl("admission|election|appointment", meeting_topic)) |>
  arrange(desc(total))

bve_votes <- cleaned_voting |> pivot_votes() |>
  merge(mgwreg, by.x = c("year", "country"), by.y = c("year", "country_text_id"),
        all.x = TRUE) |>
  as_tibble() |>
  filter(!is.na(regime)) |>
  group_by(meeting_topic, bve) |>
  reframe(total = n(),
          yes = sum(vote == "Y") / total,
          no = sum(vote == "N") / total,
          abstain = sum(vote == "A") / total,
          missing = sum(vote == "X") / total) |>
  filter(!grepl("admission|election|appointment", meeting_topic)) |>
  arrange(desc(total))

bve_test <- bve_votes |> pivot_wider(
  id_cols = meeting_topic,
  names_from = bve,
  values_from = yes) |>
  relocate(democratic, .before = entrenched) |>
  mutate(bve = backsliding / entrenched) |>
  arrange(bve)

bve_year <- bve_mg |> group_by(year, regime) |>
  summarize(yes = mean(yes, na.rm = TRUE),
            no = mean(no, na.rm = TRUE),
            abstain = mean(abstain, na.rm = TRUE),
            missing = mean(missing, na.rm = TRUE)) |>
  pivot_wider(id_cols = year,
              names_from = regime,
              values_from = c(no, abstain)) |>
  rename(no_ea = no_entrenched_auto,
         no_ed = no_entrenched_dem,
         no_ei = no_entrenched_illib,
         no_br = no_backslide_revert,
         no_be = no_backslide_erode,
         ab_ea = abstain_entrenched_auto,
         ab_ed = abstain_entrenched_dem,
         ab_ei = abstain_entrenched_illib,
         ab_br = abstain_backslide_revert,
         ab_be = abstain_backslide_erode)

bve_sum <- bve_year |>
  mutate(no_dem = no_ed,
         no_entr = sum(no_ea, no_ei, na.rm = TRUE) / 2,
         no_back = sum(no_br, no_be, na.rm = TRUE) / 2,
         bve_no = no_back / no_entr,
         ab_dem = ab_ed,
         ab_entr = sum(ab_ea, ab_ei, na.rm = TRUE) / 2,
         ab_back = sum(ab_br, ab_be, na.rm = TRUE) / 2,
         bve_ab = ab_back / ab_entr) |>
  select(year, no_dem:bve_ab)
