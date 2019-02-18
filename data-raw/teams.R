#### Load libraries #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####

  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(countrycode)

#### Load team list #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####

  df_teamlist <- read_csv("./data-raw/teams/teams.csv")

#### Download *.csv team files from VBL website #### #### #### #### #### #### #### #### #### #### #### #### #### #### ##

  vbl_id <- df_teamlist$vbl_id

  # for (i in 1:length(vbl_id)){
  #
  #   try(download.file(
  #     paste0("https://www.volleyball-bundesliga.de/servlet/sportsclub/TeamMemberCsvExport?teamId=", vbl_id[i]),
  #     paste0("./data-raw//teams/", vbl_id[i],".csv")),
  #     silent = TRUE)
  #
  # }

#### Importing teams #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

  df_teams <- data.frame()

  for (id in 1:length(vbl_id)){

    tmp <- read.csv(paste0("./data-raw/teams/", vbl_id[id], ".csv"),
                    stringsAsFactors = FALSE, fileEncoding = "iso-8859-1", sep = ";")

    tmp$vbl_id <- vbl_id[id]

    df_teams <- rbind(df_teams, tmp, stringsAsFactors = FALSE)

    rm(tmp)
  }

  df_teams <- as_tibble(df_teams)

#### Select relevant columns and join teams and remove vbl.id #### #### #### #### #### #### #### #### #### #### #### ###

  df_teams <-  df_teams %>%
               left_join(df_teamlist, by = c("vbl_id" = "vbl_id")) %>%
               select(season_id, league_gender, team_id, team_name, Nachname,
                      Vorname, Größe, Geschlecht, Geburtsdatum, Trikot, Staatsangehörigkeit,
                      NAT, Position.Funktion.Offizieller, Staatsangehörigkeit)

#### Rename columns #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####

  names(df_teams) <- c("season_id", "league_gender", "team_id", "team_name", "lastname", "firstname",
                    "height", "gender", "birthdate", "shirt_number", "nat_long", "nat_short",
                    "position")

#### Recoding variables #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #

  # Firstname

    df_teams[df_teams$firstname == "Bart?omiej",]$firstname  <- "Bartłomiej"

  # Lastname

    df_teams[df_teams$lastname == "Bo??d?",]$lastname        <- "Bołądź"
    df_teams[df_teams$lastname == "Sláde?ek",]$lastname      <- "Sládeček"

  # Gender

    df_teams[df_teams$gender == "weiblich",]$gender <- "female"
    df_teams[df_teams$gender == "männlich",]$gender <- "male"

    df_teams$gender <- factor(x = df_teams$gender, levels = c("female", "male"))

  # Birthdate

    df_teams$birthdate <- as.Date(df_teams$birthdate, format = "%d.%m.%Y")

  # Position

    df_teams[df_teams$position == "Diagonal",]$position           <- "Diagonal"
    df_teams[df_teams$position == "Mittelblock",]$position        <- "Middle block"
    df_teams[df_teams$position == "Außenangriff",]$position       <- "Outside spiker"
    df_teams[df_teams$position == "Libero",]$position             <- "Libero"
    df_teams[df_teams$position == "Zuspiel",]$position            <- "Setter"

    df_teams[df_teams$position == "Trainer",]$position            <- "Coach"
    df_teams[df_teams$position == "Co-Trainer",]$position         <- "Assistant coach"
    df_teams[df_teams$position == "Arzt",]$position               <- "Doctor"
    df_teams[df_teams$position == "Physiotherapeut",]$position    <- "Physiotherapist"
    df_teams[df_teams$position == "Statistiker",]$position        <- "Statistician"
    df_teams[df_teams$position == "Co-Trainer (Scout)",]$position <- "Assistant coach (scout)"

    df_teams$position <- factor(df_teams$position)

  # Role

    df_teams$role <- "Player"

    df_teams[df_teams$position == "Coach",]$role                   <- "Staff"
    df_teams[df_teams$position == "Assistant coach",]$role         <- "Staff"
    df_teams[df_teams$position == "Doctor",]$role                  <- "Staff"
    df_teams[df_teams$position == "Physiotherapist",]$role         <- "Staff"
    df_teams[df_teams$position == "Statistician",]$role            <- "Staff"
    df_teams[df_teams$position == "Assistant coach (scout)",]$role <- "Staff"

    df_teams$role <- factor(df_teams$role)

  # Nationality

   df_teams <- df_teams %>% mutate(nationality =  countrycode(nat_long, "country.name.de", "country.name"))

   # List countries with NA

   df_teams %>% select(nationality, nat_long) %>% filter(is.na(nationality))

   # Replace countries with NA and remove German nationality variables

   df_teams <- df_teams %>% select(-nat_long, -nat_short) %>%
   mutate(nationality = replace(nationality, is.na(nationality), c("United Kingdom"))) %>%
   mutate(nationality = factor(nationality))

#### Correcting missings #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #####

  # Checking for NA's in height/birthdate/shirt_number

    df_teams %>% filter(role != "Staff") %>%
                 select(height, birthdate, shirt_number) %>%
                 summary()

    # Missing height

     df_teams %>% filter(role != "Staff" & is.na(height))

     # Missing height belongs to staff according to web search
     df_teams[(is.na(df_teams$height) == TRUE) & (df_teams$role != "Staff"),]$role <- "Staff"

   # Missing position

     df_teams %>% filter(role != "Staff" & position == "")

     # Missing position in Men's league is setter according to web search
     df_teams[(df_teams$position == "") &
              (df_teams$team_id == 1016) &
              (df_teams$league_gender == "Men") &
              (df_teams$season_id == "1314"),]$position <- "Setter"

     # Missing position in Women's league is setter according to web search
     df_teams[(df_teams$position == "") &
                (df_teams$team_id == 2007) &
                (df_teams$league_gender == "Women") &
                (df_teams$season_id == "1314"),]$position <- "Outside spiker"

  # Missing names

     # Missing names of players where checked in match reports and replaced.
     # Missing player nmes mean that player were never fielded in the season.
     # Therefore, it is not possible to find the name.

     df_teams[df_teams$lastname == "privat - ausgeblendet",]

     df_teams[(df_teams$lastname == "privat - ausgeblendet" &
               df_teams$team_id == 1014 &
               df_teams$shirt_number == 5 &
               df_teams$season_id == "1314" &
               df_teams$league_gender == "Men"),]$lastname <- "Grosche"

     df_teams[(df_teams$firstname == "privat - ausgeblendet" &
               df_teams$team_id == 1014 &
               df_teams$shirt_number == 5 &
               df_teams$season_id == "1314" &
               df_teams$league_gender == "Men"),]$firstname <- "Eric"

     df_teams[(df_teams$lastname == "privat - ausgeblendet" &
               df_teams$team_id == 1018 &
               df_teams$shirt_number == 7 &
               df_teams$season_id == "1314" &
               df_teams$role == "Player" &
               df_teams$league_gender == "Men"),]$lastname <- "Foyer"

     df_teams[(df_teams$firstname == "privat - ausgeblendet" &
               df_teams$team_id == 1018 &
               df_teams$shirt_number == 7 &
               df_teams$season_id == "1314" &
               df_teams$role == "Player" &
               df_teams$league_gender == "Men"),]$firstname <- "Kevin"

     df_teams[(df_teams$lastname == "privat - ausgeblendet" &
               df_teams$team_id == 2011 &
               df_teams$shirt_number == 5 &
               df_teams$season_id == "1516" &
               df_teams$league_gender == "Women"),]$lastname <- "Salancoiva"

     df_teams[(df_teams$firstname == "privat - ausgeblendet" &
               df_teams$team_id == 2011 &
               df_teams$shirt_number == 5 &
               df_teams$season_id == "1516" &
               df_teams$league_gender == "Women"),]$firstname <- "Erika"

  # Missing gender

     df_teams %>% filter(role != "Staff" & is.na(gender))

     df_teams[(df_teams$lastname == "Valentine" &
                 df_teams$team_id == 2010 &
                 df_teams$shirt_number == 9 &
                 df_teams$season_id == "1314" &
                 df_teams$league_gender == "Women"),]$gender <- "female"

#### Generate player_id #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ######

  # Generate player_id which is equal to the shirt number except of cases where shirt numbers wihin a team
  # season are duplicates. In these cases the player_id is equal to a 3-digit number starting with 100 for all
  # but one player which keeps his original shirt number.

    df_teams$player_id <- df_teams$shirt_number

  # Generate data frame with duplicate shirt numbers

    duplicate_shirts <- df_teams %>%
      filter(role == "Player")  %>%
      group_by(season_id, league_gender, team_id, shirt_number) %>%
      summarize(n = n()) %>%
      filter(n > 1) %>%
      left_join(df_teams) %>%
      select(-n, -role)

  #### Season 2013/2014 MEN

     # Check which player is fielded with this shirt number in the season:

     # read_csv("./data-raw/matchreports/MATCHSTAT_MEN_1314.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 9 & team.id == 1016) %>% select(player.name) %>% table()

     # Change shirt number of Hopt to 100

    df_teams[(df_teams$lastname == "Hopt" & df_teams$season_id == "1314"),]$player_id <- 100

     # Check which player is fielded with this shirt number in the season:

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELMEN1314.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 10 & team.id == 1014) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Kroß"     & df_teams$season_id == "1314"),]$player_id <- 100
     df_teams[(df_teams$lastname == "Pinheiro" & df_teams$season_id == "1314"),]$player_id <- 101

  #### Season 2014/2015 MEN

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELMEN1415.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 2 & team.id == 1016) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Günthör" &
               df_teams$firstname == "Jakob"  &
               df_teams$season_id == "1415"),]$player_id             <- 100

  #### Season 2013/2014 WOMEN

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1314.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 1 & team.id == 2002) %>% select(player.name) %>% table()

     # All players are neverfielded in this season.

     df_teams[(df_teams$lastname == "Grütze"  &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1314"   &
               df_teams$league_gender == "Women"),]$player_id <- 100

     df_teams[(df_teams$lastname == "Kirsten" &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1314"   &
               df_teams$league_gender == "Women"),]$player_id <- 101

     df_teams[(df_teams$lastname == "Petter"  &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1314"   &
               df_teams$league_gender == "Women"),]$player_id <- 102

     df_teams[(df_teams$lastname == "Plath"   &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1314"   &
               df_teams$league_gender == "Women"),]$player_id <- 103

     df_teams[(df_teams$lastname == "Wolf"    &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1314"   &
               df_teams$league_gender == "Women"),]$player_id <- 104

  #### Season 2014/2015 WOMEN

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1415.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 2 & team.id == 2002) %>% select(player.name) %>% table()

     # All players are neverfielded in this season.

     df_teams[(df_teams$lastname == "Kirsten" &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1415"   &
               df_teams$league_gender == "Women"),]$player_id <- 100

     df_teams[(df_teams$lastname == "Petter"  &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1415"   &
               df_teams$league_gender == "Women"),]$player_id <- 101

     df_teams[(df_teams$lastname == "Plath"   &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1415"   &
               df_teams$league_gender == "Women"),]$player_id <- 102

     df_teams[(df_teams$lastname == "Wolf"    &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1415"   &
               df_teams$league_gender == "Women"),]$player_id <- 103


  #### Season 2015/2016 WOMEN

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1516.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 6 & team.id == 2002) %>% select(player.name) %>% table()

     # All players are neverfielded in this season.

     df_teams[(df_teams$lastname == "Lowke"   &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1516"   &
               df_teams$league_gender == "Women"),]$player_id <- 100

     df_teams[(df_teams$lastname == "Plath"   &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1516"   &
               df_teams$league_gender == "Women"),]$player_id <- 101

     df_teams[(df_teams$lastname == "Wolf"    &
               df_teams$team_id == 2002       &
               df_teams$season_id == "1516"   &
               df_teams$league_gender == "Women"),]$player_id <- 102

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1516.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 18 & team.id == 2002) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Onyejekwe" &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1516"     &
               df_teams$league_gender == "Women"),]$player_id <- 103

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1516.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 16 & team.id == 2003) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Senger"    &
               df_teams$team_id == 2003         &
               df_teams$season_id == "1516"     &
               df_teams$league_gender == "Women"),]$player_id <- 100

  #### Season 2016/2017 WOMEN

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1617.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 1 & team.id == 2002) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Kömmling"  &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1617"     &
               df_teams$league_gender == "Women"),]$player_id <- 100

     df_teams[(df_teams$lastname == "Weitzel"   &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1617"     &
               df_teams$league_gender == "Women"),]$player_id <- 101

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1617.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 17 & team.id == 2002) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Mancuso"   &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1617"     &
               df_teams$league_gender == "Women"),]$player_id <- 102

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1617.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 13 & team.id == 2012) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Wiesner"   &
               df_teams$team_id == 2012         &
               df_teams$season_id == "1617"     &
               df_teams$league_gender == "Women"),]$player_id <- 101

  #### Season 2017/2018 WOMEN

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1718.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 17 & team.id == 2002) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Kömmling"   &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1718"     &
               df_teams$league_gender == "Women"),]$player_id <- 100

     df_teams[(df_teams$lastname == "Nestler"   &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1718"     &
               df_teams$league_gender == "Women"),]$player_id <- 101

     df_teams[(df_teams$lastname == "Nitsche"   &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1718"     &
               df_teams$league_gender == "Women"),]$player_id <- 102

     df_teams[(df_teams$lastname == "Scholz"    &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1718"     &
               df_teams$league_gender == "Women"),]$player_id <- 103

     df_teams[(df_teams$lastname == "Stemmler"  &
               df_teams$team_id == 2002         &
               df_teams$season_id == "1718"     &
               df_teams$league_gender == "Women"),]$player_id <- 104

     # read_csv("./Matchreports/SEASONSMATCHPLAYERLEVELWOMEN1718.csv") %>%
     # select(match.id, team.id, shirt.number, player.name) %>%
     # filter(shirt.number == 18 & team.id == 2012) %>% select(player.name) %>% table()

     df_teams[(df_teams$lastname == "Schieder"  &
               df_teams$team_id == 2012         &
               df_teams$season_id == "1718"     &
               df_teams$league_gender == "Women"),]$player_id <- 100

     # Remove tibble with duplicates

     rm(duplicate_shirts)

#### Split and save datasets #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #

     # Split dataset into Staff and Players

     players <- df_teams[df_teams$role == "Staff",] %>%
     select(-role, -shirt_number, -height, -player_id) %>%
     mutate(position = droplevels(position),
            nationality = droplevels(nationality))

     save(players, file = "./data/staff.rda")

     players <- df_teams[df_teams$role != "Staff",] %>%
     select(-role, league_gender, season_id, team_id, team_name,
            player_id, shirt_number, position, firstname, lastname,
            gender, birthdate, height, nationality) %>%
     mutate(position = droplevels(position),
            nationality = droplevels(nationality))

     save(staff, file = "./data/players.rda")
