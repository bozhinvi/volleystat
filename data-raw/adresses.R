#### Load libraries #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(ggmap)

#### Load season list #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

df_seasons <- read_csv("./data-raw/seasons/seasons.csv")

#### Download *.csv season files from VBL website #### #### #### #### #### #### #### #### #### #### #### #### #### #####

vbl_id <- df_seasons$vbl_id

#### Importing seasons #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ##

# Importing main round

vbl_id_main <- df_seasons[df_seasons$competition_stage == "Main",]$vbl_id

df_main_round <- data.frame()

for (num in 1:length(vbl_id_main)){

  tmp <- read.csv(paste0("./data-raw/seasons/", vbl_id_main[num], ".csv"),
                  stringsAsFactors = FALSE, fileEncoding = "iso-8859-1", sep = ";")

  tmp$vbl_id <- vbl_id_main[num]

  df_main_round <- rbind(df_main_round, tmp, stringsAsFactors = FALSE)

  rm(tmp)

}

# Importing playoffs

vbl_id_play <- df_seasons[df_seasons$competition_stage != "Main",]$vbl_id

df_play_off <- data.frame()

for (num in 1:length(vbl_id_play)){

  tmp <- read.csv(paste0("./data-raw/seasons/", vbl_id_play[num], ".csv"),
                  stringsAsFactors = FALSE, fileEncoding = "iso-8859-1", sep = ";")

  tmp$vbl_id <- vbl_id_play[num]

  df_play_off <- rbind(df_play_off, tmp, stringsAsFactors = FALSE)

  rm(tmp)

}

adresses_1 <- df_main_round %>% select(X., Austragungsort, vbl_id)
adresses_2 <- df_play_off   %>% select(X., Austragungsort, vbl_id)

match_adresses <- rbind(adresses_1, adresses_2) %>%
                  right_join(df_seasons) %>%
                  select(season_id, X., Austragungsort) %>%
                  mutate(Austragungsort = str_remove(Austragungsort, "\\(SLZB\\)")) %>%
                  separate(Austragungsort, into = c("gym", "adress"), "\\(") %>%
                  mutate(adress = str_remove(adress, "\\)"))


geocodes <- geocode(match_adresses$adress)

match_adresses <- bind_cols(match_adresses, geocodes) %>% rename(match_id = X.)

rm(adresses_1, adresses_2, df_main_round, df_seasons, df_play_off)

#### Save adresses dataset #### #### #### #### #### #### #### #### #### #### #### #### #### ##### ##### ##### ##### ####

devtools::use_data(match_adresses, overwrite = TRUE)

#### Load season list #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

df_teamlist <- read_csv2("./data-raw/teams/teams.csv")

geocodes_teamlist <- geocode(df_teamlist$gym_adress)

team_adresses <- bind_cols(df_teamlist, geocodes_teamlist) %>% select(-vbl_id)

#### Save adresses dataset #### #### #### #### #### #### #### #### #### #### #### #### #### ##### ##### ##### ##### ####

devtools::use_data(team_adresses, overwrite = TRUE)
