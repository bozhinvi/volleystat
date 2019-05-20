########################################################################################################################

    library(readr)
    library(stringr)
    library(dplyr)
    library(tidyr)
    library(purrr)

########################################################################################################################

      setwd("./data-raw/matchreports")

  # Load files and import as data.frame

      files <- as.list(list.files(pattern = "MATCHSTAT_*"))

      df <- map(files, read_csv) %>% bind_rows()

  # Remove all players who are not fielded (i.e., all where pt_tot:blo_pt is NA)
  # and replace all other NAs with zero.

      df <- df %>%
            mutate(n_na = rowSums(is.na(df[c(8:23)])))                 %>%
            filter(n_na != 16)                                         %>%
            select(-n_na)                                              %>%
            replace_na(list(pt_tot      = 0))                          %>%
            replace_na(list(pt_bp       = 0))                          %>%
            replace_na(list(pt_profloss = 0))                          %>%
            replace_na(list(serv_tot    = 0))                          %>%
            replace_na(list(serv_err    = 0))                          %>%
            replace_na(list(serv_pt     = 0))                          %>%
            replace_na(list(rec_tot = 0))                              %>%
            replace_na(list(rec_err = 0))                              %>%
            replace_na(list(rec_pos = 0))                              %>%
            replace_na(list(rec_per = 0))                              %>%
            replace_na(list(att_tot = 0))                              %>%
            replace_na(list(att_err = 0))                              %>%
            replace_na(list(att_blo = 0))                              %>%
            replace_na(list(att_pt = 0))                               %>%
            replace_na(list(att_per = 0))                              %>%
            replace_na(list(blo_pt = 0))                               %>%
            mutate(team_name = str_replace_all(team_name, "\\.", " "),
                   team_name = str_remove_all(team_name, "[:digit:]"),
                   team_name = str_trim(team_name, side = "both"))      %>%
            filter(str_detect(player_name, "Kotrainer") != TRUE)

    # File with team names were exportes as CSV and the team_ids were merged

      df <- read_csv2("team_ids.csv") %>% right_join(df)

    # Drop matches of teams where no information is available (i.e. teams without teamid)

      matches_to_drop <- df %>%
                         filter(is.na(team_id)) %>%
                         distinct(league_gender, season_id, match_id)

      df <- df %>% anti_join(matches_to_drop)

    # Remove useless variables

      df <- df %>%
            select(-team_name) %>%
            select(league_gender, season_id, team_id, match_id, everything())

    # Generate player_id

      df$player_id <- df$shirt_number

      df[(df$player_name == "Pinheiro" &
            df$team_id == 1014 &
            df$season_id == "1314" &
            df$league_gender == "Men"),]$player_id <- 101

      df[str_detect(df$player_name, "Onyejekwe") == TRUE & df$season_id == "1516",]$player_id <- 103
      df[str_detect(df$player_name, "Senger")    == TRUE & df$season_id == "1516" & df$team_id == 2003,]$player_id <- 100
      df[str_detect(df$player_name, "Mancuso")   == TRUE & df$season_id == "1617" & df$team_id == 2002,]$player_id <- 102
      df[str_detect(df$player_name, "Wiesner")   == TRUE & df$season_id == "1617" & df$team_id == 2012,]$player_id <- 101
      df[str_detect(df$player_name, "Schieder")  == TRUE & df$season_id == "1718" & df$team_id == 2012,]$player_id <- 100

########################################################################################################################

  # Consistency checks:

  # Sum of serv_pt + att_pt + blo_pt must be equal zero

    df %>%
    mutate(problem = serv_pt + att_pt + blo_pt - pt_tot) %>%
    filter(problem != 0) %>%
    arrange(league_gender, season_id, match_id, player_id) %>%
    distinct(league_gender, season_id, match_id, .keep_all = TRUE)

    # Remove observations with Trainer in 1819 season
    df <- df %>% filter(str_detect(player_name, "Trainer[:alpha:]{1,}") == FALSE)

  # att_per = att_pt/att_tot

    df %>%
    filter(att_tot > 0) %>%
    mutate(att_per_new = round(att_pt/att_tot*100, digits = 0)) %>%
    filter(att_per_new != att_per) %>% select(league_gender:vote, att_tot:att_per, att_per_new)

  # pt_profloss = serv_pt + att_pt + blo_pt - serv_err - rec_err -att_err -att_blo

    df %>%
    mutate(pt_profloss_new = serv_pt + att_pt + blo_pt - serv_err - rec_err -att_err -att_blo) %>%
    filter(pt_profloss_new != pt_profloss)

  # Some match reports do not seem to be inconsistent with the computation of profits - losses. This inconsistency
  # is corrected. Note that the inconsistency is not due to scrapping but it is the true values which are not
  # consistent in the match reports in season 13/14 with respect to all other reports.

    df <- df %>% mutate(pt_profloss = serv_pt + att_pt + blo_pt - serv_err - rec_err -att_err -att_blo)

########################################################################################################################

  # Create final dataset

    matchstats <- df %>%
    select(league_gender, season_id, match_id, team_id, player_id, everything(), -player_name)

    #save(matchstats, file = "../../data/matchstats.rda")
    devtools::use_data(matchstats, matchstats, overwrite = TRUE)

########################################################################################################################
