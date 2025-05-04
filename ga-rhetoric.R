source("main.R") # Technically don't need the gen_speech df here, so just main.

######################### ONE-TIME SETUP #########################
## Since there are a lot of files, the following one-time setup ##
## has been commented out. I put it in a csv to make it quicker ##
## to read into my dataframe for future use. #####################
##################################################################

library(easyr)

files <- list.files(path="GA_speech/", pattern=".txt", full.names=TRUE, recursive=TRUE)

speeches <- files |>
  set_names() |>
  map_df(~ read.txt(.x), .id = "source_file") |>
  pivot_longer(everything(), names_to = "filename", values_to = "speech") |>
  as_tibble() |>
  separate_wider_delim(filename, delim = "/",
                       names = c("XX", "session", "filename")) |>
  mutate(year = as.numeric(right(session, 4)),
         country_text_id = left(filename, 3)) |>
  select(speech:country_text_id) |>
  relocate(speech, .after = country_text_id)

speeches |> write_csv("GA_Speech/gaspeeches.csv")