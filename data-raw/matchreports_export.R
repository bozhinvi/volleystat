########################################################################################################################

  # Loading packages

    library(tabulizer)
    library(stringr)
    library(dplyr)
    library(tidyr)
    library(purrr)

  # Loading functions (funs.R)

    source("./data-raw/matchreports_fun.R")

########################################################################################################################

  # Set season WD

    setwd("./data-raw/matchreports/Women2017-18")

  # Set season name

    filename      <- "MATCHSTAT_WOMEN_1718.csv" # Set file name which is exported in parent directory
    league_gender <- "Women"                    # Set league.gender to Men or Women
    season_id     <- "1718"                     # Set season.id

########################################################################################################################

# First, all match report file names are exported into a vector. Then, two lists containing data.frames (one
# for each team, one table for each match) are generated using a function (match_stat) defined to extarct the
# relevant table of the pdf.

  # Vector with file names

    nm.vec <- as.list(list.files(pattern = "*.pdf"))

  # Scrap Teams from PDFs

    team.1 <- map(nm.vec, match_stat, position = 1)
    team.2 <- map(nm.vec, match_stat, position = 2)

  # Scrap Team names from PDFs

    team.names.1 <- map(team.1, export_teamname)
    team.names.2 <- map(team.2, export_teamname)

  # Attach match ids as name

    match.id <- str_remove(nm.vec, ".pdf")

    names(team.1) <- match.id
    names(team.2) <- match.id

    names(team.names.1) <- match.id
    names(team.names.2) <- match.id

########################################################################################################################

  # Data Cleaning

  # 1. The first column contains the shirt numbers and names of each player. In addition, the first column often is
  # mixed up with the starting six in set 1.

    # To extract shirt.number and player name, the function clean_col_1 is used. This function takes the first
    # column of the respective data.frame and separates into two columns: shirt.number and player.name

    # Cleaning column 1

      team.1.part.1 <- map(team.1, clean_col_1)
      team.2.part.1 <- map(team.2, clean_col_1)

  # 2. The second problem occurs not very often. However, in some data.frames the column encompassing the share of
  # perfect receptions and the column counting the number of attacks of each player are messed up. More precisely,
  # they are glued together into one column. For this purpose, the data.frames are trimmed to the relevant columns
  # (Vote-Bk Pkt). Then all NA columns are removed. Finally, the function __clean_reception__ is used to separate
  # the corrupt column into two columns.

    # Only columns 8 and all subsequent columns are selected

      team.1.row_vote <- map(team.1, return_rown_vote)
      team.2.row_vote <- map(team.2, return_rown_vote)

      team.1.part.2   <- map2(team.1, team.1.row_vote, function(x, y){x[-c(1:y)]})
      team.2.part.2   <- map2(team.2, team.2.row_vote, function(x, y){x[-c(1:y)]})

    # Delete all empty columns:

      team.1.part.2 <- map(team.1.part.2, function(x){x %>% select_if(~sum(!is.na(.)) > 0)})
      team.2.part.2 <- map(team.2.part.2, function(x){x %>% select_if(~sum(!is.na(.)) > 0)})

    # Clean reception variable

      team.1.part.2 <- map(team.1.part.2, clean_reception)
      team.2.part.2 <- map(team.2.part.2, clean_reception)

  # For each team the shirt number and the players name is saved in part.1. Part.2 contains all other
  # information. In addition, the tables contain also aggregate information which is not needed (i.e., the
  # bottom of each team's match statistics). First, the corresponding parts are combined into one object which
  # is the final list of data.frames for each match (and each team). Second, the function rename_cols() is
  # applied to rename the columns accoring to the respective match statistic. Then the function
  # rm_useless_rows() removes non the relevant information. Finally, characters are transformed into numerics.

    # Combine part.1 and part.2

      team.1.clean <- map2(team.1.part.1, team.1.part.2, bind_cols)
      team.2.clean <- map2(team.2.part.1, team.2.part.2, bind_cols)

    # Rename cols

      team.1.clean <- map(team.1.clean, rename_cols)
      team.2.clean <- map(team.2.clean, rename_cols)

    # Remove rows with shirt.number == NA

      team.1.clean <- map(team.1.clean, rm_useless_rows)
      team.2.clean <- map(team.2.clean, rm_useless_rows)

    # Transform chars to integers

      team.1.clean <- map(team.1.clean, numerize_vars)
      team.2.clean <- map(team.2.clean, numerize_vars)

  # Before unnesting the data.frames and combining team 1 and team 2 into a the final data.frame,
  # team informion needs to be added. Since the offical vbl number is used as an identifier for
  # each match report, this can be used to add the team name and team id to each data.frame before unnesting.

  # Unnesting data.frames for Team 1 and merging team.names

     team.names.1.df <- bind_rows(team.names.1) %>%
                        gather(key = "match_id", value = "team_name") %>%
                        mutate(match_id = as.integer(match_id))

     team.1.clean.df <- bind_rows(team.1.clean, .id = "match_id") %>%
                        mutate(match_id = as.integer(match_id)) %>%
                        left_join(team.names.1.df)

  # Unnesting data.frames for Team 2 and merging team.names

    team.names.2.df <- bind_rows(team.names.2) %>%
                       gather(key = "match_id", value = "team_name") %>%
                       mutate(match_id = as.integer(match_id)) %>%
                       mutate(team_name = replace(team_name, team_name == "", NA))

    team.2.clean.df <- bind_rows(team.2.clean, .id = "match_id") %>%
                       mutate(match_id = as.integer(match_id)) %>%
                       left_join(team.names.2.df)

  # Combining final season Data

    df <- bind_rows(team.1.clean.df, team.2.clean.df)

    df$season_id     <- season_id
    df$league_gender <- league_gender

    df <- df %>% select(league_gender, season_id, match_id, team_name, everything())

  # Export data as CSV

    write.csv(df, filename, row.names = FALSE)

    setwd("../../../")
