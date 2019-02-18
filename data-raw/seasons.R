#### Load libraries #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####

    library(dplyr)
    library(tidyr)
    library(readr)
    library(purrr)
    library(stringr)

#### Load season list #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

  df_seasons <- read_csv("./data-raw/seasons/seasons.csv")

#### Download *.csv season files from VBL website #### #### #### #### #### #### #### #### #### #### #### #### #### #####

  vbl_id <- df_seasons$vbl_id

  # for (id in 1:length(vbl_id)){
  #
  #   try(download.file(
  #     paste("https://www.volleyball-bundesliga.de/servlet/league/PlayingScheduleCsvExport?matchSeriesId=",
  #           vbl_id[id], sep=""),
  #     paste0("./data-raw/seasons/", vbl_id[id],".csv")),
  #     silent = TRUE)
  #
  # }

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

#### Select relevant columns and join seasons #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

  # Select columns in df_main round

  df_main_round <- df_main_round %>% select(-c(Geschlecht, Saison, X, X..1, X..2, X..3, X..4, X..5,
                                               X..6, Spielrunde, Ergebnis, Satzpunkte, Ballpunkte,
                                               Austragungsort, Wochentag, Austragungsort.Ergebnis))

  # Rename variables

  df_main_round <- df_main_round %>% rename(date = Datum)                             %>%
                                     rename(time = Uhrzeit)                           %>%
                                     rename(match_id = X.)                            %>%
                                     rename(match_day = ST)                           %>%
                                     rename(team_1 = Mannschaft.1)                    %>%
                                     rename(team_2 = Mannschaft.2)                    %>%
                                     rename(home_team = Gastgeber)                    %>%
                                     rename(match_duration = Spieldauer)              %>%
                                     rename(spectators = Zuschauerzahl)               %>%
                                     rename(set_won_team_1 = Satzpunkte.1)            %>%
                                     rename(set_won_team_2 = Satzpunkte.2)            %>%
                                     rename(set_1_pt_team_1 = Satz.1...Ballpunkte.1)  %>%
                                     rename(set_1_pt_team_2 = Satz.1...Ballpunkte.2)  %>%
                                     rename(set_1_duration = Satz.1...Satzdauer)      %>%
                                     rename(set_2_pt_team_1 = Satz.2...Ballpunkte.1)  %>%
                                     rename(set_2_pt_team_2 = Satz.2...Ballpunkte.2)  %>%
                                     rename(set_2_duration = Satz.2...Satzdauer)      %>%
                                     rename(set_3_pt_team_1 = Satz.3...Ballpunkte.1)  %>%
                                     rename(set_3_pt_team_2 = Satz.3...Ballpunkte.2)  %>%
                                     rename(set_3_duration = Satz.3...Satzdauer)      %>%
                                     rename(set_4_pt_team_1 = Satz.4...Ballpunkte.1)  %>%
                                     rename(set_4_pt_team_2 = Satz.4...Ballpunkte.2)  %>%
                                     rename(set_4_duration = Satz.4...Satzdauer)      %>%
                                     rename(set_5_pt_team_1 = Satz.5...Ballpunkte.1)  %>%
                                     rename(set_5_pt_team_2 = Satz.5...Ballpunkte.2)  %>%
                                     rename(set_5_duration = Satz.5...Satzdauer)

  # Select columns in df_play_off round

  df_play_off <- df_play_off  %>% select(-c(Geschlecht, Saison, X, X..1, X..2, X..3, X..4, X..5,
                                            X..6, Ergebnis, Wochentag, Spielfeld,
                                            Austragungsort, Austragungsort.Ergebnis, Satzpunkte,
                                            Ballpunkte, Spielrunde, Gruppen.Ebene))

  # Rename variables

  df_play_off <- df_play_off  %>% rename(date = Datum)                            %>%
                                  rename(time = Uhrzeit)                          %>%
                                  rename(match_id = X.)                           %>%
                                  rename(competition_stage = Spielgruppe)         %>%
                                  rename(team_1 = Mannschaft.1)                   %>%
                                  rename(team_2 = Mannschaft.2)                   %>%
                                  rename(home_team = Gastgeber)                   %>%
                                  rename(match_duration = Spieldauer)             %>%
                                  rename(spectators = Zuschauerzahl)              %>%
                                  rename(set_won_team_1 = Satzpunkte.1)           %>%
                                  rename(set_won_team_2 = Satzpunkte.2)           %>%
                                  rename(set_1_pt_team_1 = Satz.1...Ballpunkte.1) %>%
                                  rename(set_1_pt_team_2 = Satz.1...Ballpunkte.2) %>%
                                  rename(set_1_duration = Satz.1...Satzdauer)     %>%
                                  rename(set_2_pt_team_1 = Satz.2...Ballpunkte.1) %>%
                                  rename(set_2_pt_team_2 = Satz.2...Ballpunkte.2) %>%
                                  rename(set_2_duration = Satz.2...Satzdauer)     %>%
                                  rename(set_3_pt_team_1 = Satz.3...Ballpunkte.1) %>%
                                  rename(set_3_pt_team_2 = Satz.3...Ballpunkte.2) %>%
                                  rename(set_3_duration = Satz.3...Satzdauer)     %>%
                                  rename(set_4_pt_team_1 = Satz.4...Ballpunkte.1) %>%
                                  rename(set_4_pt_team_2 = Satz.4...Ballpunkte.2) %>%
                                  rename(set_4_duration = Satz.4...Satzdauer)     %>%
                                  rename(set_5_pt_team_1 = Satz.5...Ballpunkte.1) %>%
                                  rename(set_5_pt_team_2 = Satz.5...Ballpunkte.2) %>%
                                  rename(set_5_duration = Satz.5...Satzdauer)

#### Cleaning variables main round matches #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ##

  # date.time variable

    df_main_round <- df_main_round %>% unite(date, time, col = "date_time", sep = " ")

    df_main_round$date_time <- as.POSIXct(strptime(df_main_round$date_time, format = "%d.%m.%Y %H:%M:%S"))

  # match.day

    df_main_round$match_day <- as.integer(str_replace(df_main_round$match_day, "Spieltag", ""))

  # durations

    df_main_round <- df_main_round %>% mutate_each_(funs(as.numeric), c("set_1_duration", "set_2_duration",
                                                                     "set_3_duration", "set_4_duration",
                                                                     "set_5_duration")) %>%
                                       mutate(match_duration = as.integer(match_duration))

  # Delete match information of "CLOUD&HEAT VOLLEY DRESDEN" (team went bankrupt during the season)

    df_main_round <- df_main_round %>% filter((team_1    != "CLOUD&HEAT VOLLEY DRESDEN") &
                                              (team_2    != "CLOUD&HEAT VOLLEY DRESDEN") &
                                              (home_team != "CLOUD&HEAT VOLLEY DRESDEN"))

  # Competition

    df_main_round$competition_stage <- "Main round"

#### Cleaning variables playoff matches #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####


    # date.time variable

      df_play_off <- df_play_off %>% unite(date, time, col = "date_time", sep = " ")

      df_play_off$date_time <- as.POSIXct(strptime(df_play_off$date_time,
                                                   format = "%d.%m.%Y %H:%M:%S"))

    # competition_stage

      df_play_off$competition_stage <- gsub(".*Viertelfinale.*", "Quarterfinal",
                                            df_play_off$competition_stage)

      df_play_off$competition_stage <- gsub(".*Halbfinale.*",    "Semifinal",
                                            df_play_off$competition_stage)

      df_play_off$competition_stage <- gsub(".*Pre-Playoff.*",   "Pre-Playoff",
                                            df_play_off$competition_stage)

      df_play_off$competition_stage <- gsub(".*Finale.*",        "Finale",
                                            df_play_off$competition_stage)

      df_play_off$competition_stage <- gsub(".*Playdown.*",      "Playdown",
                                            df_play_off$competition_stage)

    # Replace missings in home team

      df_play_off$home_team[df_play_off$vbl_id   == 8750462 &
                            df_play_off$match_id == 1510]  <- "NETZHOPPERS KW-Bestensee"

      df_play_off$home_team[df_play_off$vbl_id   == 8750462 &
                            df_play_off$match_id == 1507]  <- "CV Mitteldeutschland"

      df_play_off$home_team[df_play_off$vbl_id   == 21637369 &
                            df_play_off$match_id == 1510]  <- "TSV Herrsching"

    # Generate matchday == NA

      df_play_off$match_day <- NA

#### Join datasets and add team_ids #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

   # Load team_ids (file has been generated using the names of all teams). Note that since VCO Berlin has a team in
   # the men's and women's league, it is necessary to join via team name and legaue gender.

   df_teamids <- read_csv2("./data-raw/seasons/team_ids.csv")

   # Joining data frames

   df_seasons <- df_seasons %>% select(-competition_stage) %>% right_join(bind_rows(df_main_round, df_play_off)) %>%
                 select(-vbl_id) %>%
                 left_join(df_teamids, by = c("team_1" = "team_name", "league_gender" = "league_gender")) %>%
                 rename(team_id_1 = team_id) %>%
                 left_join(df_teamids, by = c("team_2" = "team_name", "league_gender" = "league_gender")) %>%
                 rename(team_id_2 = team_id)

#### Match level data #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

     # Select relevant information on match level
     # Gather team names, generate match, team_id and set_won variables
     # Remove unnecessary variables

         matches <- df_seasons                                                      %>%
         select(league_gender, season_id, match_id,
                  competition_stage, date_time, match_day,
                  spectators, match_duration, team_id_1, team_id_2, team_1, team_2,
                  set_won_team_1, set_won_team_2)                                   %>%
         gather(team_1, team_2, key = "match", value = "team_name")                 %>%
         mutate(match = replace(match, match == "team_1", "home"),
                match = replace(match, match == "team_2", "away"),
                team_id = ifelse(match == "home", team_id_1, team_id_2),
                set_won = ifelse(match == "home", set_won_team_1, set_won_team_2))  %>%
         select(league_gender, season_id, competition_stage,
                match_id, match, match_day, date_time, spectators, match_duration,
                team_id, team_name, set_won)                                        %>%
         mutate(match = factor(match))

         save(matches, file = "./data/matches.rda")

#### Set level data #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####

    # Gather set duration and set points for each team, spread it into two columns.
    # Gather team names, generate team_id and pt_set for each team
    # Remove unnecessary variables and all sets which do not exists because match was
    # shorter than 5 sets.

    sets <- df_seasons %>%
    select(-home_team, -set_won_team_1, -set_won_team_2, -match_duration,
           -spectators, match_day, competition_stage, date_time) %>%
    gather(set_1_duration, set_2_duration, set_3_duration, set_4_duration, set_5_duration,
           set_1_pt_team_1, set_2_pt_team_1, set_3_pt_team_1, set_4_pt_team_1, set_5_pt_team_1,
           set_1_pt_team_2, set_2_pt_team_2, set_3_pt_team_2, set_4_pt_team_2, set_5_pt_team_2,
           key="key", value="points") %>%
    mutate(set = as.integer(str_extract(key, "[:digit:]")))%>%
    mutate(key = str_extract(key, "pt_team_[:digit:]")) %>%
    replace_na(list(key = "set_duration")) %>%
    spread(key, points) %>%
    gather(team_1, team_2,
           key = "match", value = "team_name") %>%
    mutate(match = replace(match, match == "team_1", "home"),
           match = replace(match, match == "team_2", "away"),
           team_id = ifelse(match == "home", team_id_1, team_id_2),
           pt_set  = ifelse(match == "home", pt_team_1, pt_team_2),
           match = factor(match)
           ) %>%
    select(league_gender, season_id, match_id, match, team_id, team_name,
           set, set_duration, pt_set) %>%
    filter(!is.na(pt_set))

    save(sets, file = "./data/sets.rda")
