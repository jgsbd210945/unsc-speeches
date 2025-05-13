## Mainly for finding general counting statistics with the voting data.
## There's a good chance I don't need most of this, but this should plot the general
## Yes/No/Abstain.

source("voting-setup.R")

# Functions
pivot_votes <- function(df){ # helper for sumvotes_year (make a dataframe based on voting/res/etc.)
  df |> pivot_longer(!meeting_record:link,
                     names_to = "country",
                     values_to = "vote",
                     values_drop_na = TRUE) |>
    select(date, year, resolution, country, vote, meeting_topic, title)
}

sumvotes_year <- function(df, merger = mgwreg) { # Makes a year-by-year percentage of votes yes/no/abstain.
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

graph_polyarchy <- function(df, ylower = 0.8){ # Graphs based on v2x_polyarchy
  ggplot(df, aes(v2x_polyarchy, yes, color = regime)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    ylim(ylower, 1) +
    labs(color = "Regime") +
    scale_color_manual(values = color_scheme,
                       labels = c("Democratic Erosion",
                                  "Democratic Reversion",
                                  "Entrenched Autocracy",
                                  "Entrenched Democracy",
                                  "Grey Area Regime")) +
    xlab("Electoral Democracy Score") +
    ylab("Rate of Votes In Favor of Resolutions per Year")
}

graph_diff_polyarchy <- function(df, xlower = -0.08, xupper = 0.01, ylower = 0.8) { # Same as above, but with diff_polyarchy.
  ggplot(df, aes(diff_polyarchy, yes)) +
    geom_point(aes(color = regime)) +
    geom_smooth(method = 'lm') +
    xlim(xlower, xupper) +
    ylim(ylower, 1) +
    labs(color = "Regime") +
    scale_color_manual(values = color_scheme,
                       labels = c("Democratic Erosion",
                                  "Democratic Reversion",
                                  "Entrenched Autocracy",
                                  "Entrenched Democracy",
                                  "Grey Area Regime"))+
    xlab("Difference in Electoral Democracy Score") +
    ylab("Rate of Votes in Favor of Resolutions per Year")
}


# Yes/No/etc.
voterate <- sumvotes_year(voting)

## Votes in favor of resolutions based on polyarchy score/diff in polyarchy score
graph_polyarchy(voterate)
graph_diff_polyarchy(voterate)

## Votes against resolutions based on overall v2x_polyarchy score
ggplot(voterate, aes(v2x_polyarchy, no, color = regime)) +
  geom_point() +
  geom_smooth(method = 'lm') +
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
