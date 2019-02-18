###################################################################################
############ FUNCTION match_stat BEGIN ############################################
#
# Function uses the tabulizer package to export the
# Team statistcs table. It takes a data.frame as first
# argument and the team as second argument (position).
# The function is calibrated for the position of the team
# statistics manually by using the locate_areas()
# function of the tabulizer package
#

match_stat <- function(x, position = 1){

  if (position == 1) {

    pos <- list(c(177,  0, 340, 840))

  }

  if (position == 2) {

    pos <- list(c(415, 0, 573, 840))

  }

  as.data.frame(extract_tables(x,
                               area = pos,
                               output = "data.frame",
                               method = "stream"))

}

############ FUNCTION match_stat END ##############################################
###################################################################################

###################################################################################
############ FUNCTION clean_reception BEGIN #######################################
#
# Function takes a data.frame as argument and checks if the number of columns
# is 16. If TRUE then column 11 ist assumed to be messed up and it is separated
# using regular expressions. Then everything is put together and returned. If
# condition is FALSE, the function returns the original object
#

clean_reception <- function(x){

  # If number of columns is 16, then column 11
  # has to be cleaned. This is done in the if
  # col_number 16 == TRUE case

  if(dim(x)[2] == 16) {

    # Split data.frame in 3 parts

    part.1 <- x[1:10]
    part.2 <- x[11]
    part.3 <- x[12:16]

    # Clean column 11 and separate it into two columns

    names(part.2) <- c("X1")

    part.2   <- part.2 %>%
      mutate(X1   = str_replace(X1, "[:space:]{1,5}", "")) %>%
      mutate(col1 = str_extract(X1, "([:digit:]|[:digit:][:digit:])%")) %>%
      mutate(col2 = str_sub(X1, -2)) %>%
      mutate(col1 = str_remove(col1, "\\(")) %>%
      mutate(col1 = str_remove(col1, "\\)")) %>%
      mutate(col1 = str_remove(col1, "%")) %>%
      mutate(col2 = str_remove(col2, "\\)")) %>%
      mutate(col2 = str_replace_all(col2, "·{1,}", "")) %>%
      select(col1, col2)

    # Bind everything back together

    part.1 <- bind_cols(part.1, part.2)
    part   <- bind_cols(part.1, part.3)

    return(part)

  } else {

    return(x)

  }
}
############ FUNCTION clean_reception END ######################
################################################################


################################################################
############ FUNCTION clean_col_1 BEGIN ########################

clean_col_1 <- function(x) {

  y <- x[1]
  names(y) <- c("X1")

  y <- y %>%
    mutate(shirt_number = str_extract(X1, "[:digit:]{1,2}")) %>%
    mutate(shirt_number = as.integer((shirt_number))) %>%
    mutate(set.1 = as.integer(str_sub(X1, -2)))       %>%
    mutate(X1 = str_replace_all(X1, "[:digit:]", "")) %>%
    mutate(X1 = str_replace_all(X1, "C ", ""))        %>%
    mutate(X1 = str_replace_all(X1, "L ", ""))        %>%
    mutate(X1 = str_remove(X1, "[:blank:]"))          %>%
    rename(player_name = X1)                          %>%
    select(shirt_number, player_name)

  return(y)

}

############ FUNCTION clean_col_1 END ##########################
################################################################

################################################################
############ FUNCTION rm_useless_rows BEGIN ####################
#
# Function removes columns without shirt numbers, i.e.
# non-player columns.
#

rm_useless_rows <- function(x) {

  return(x %>% filter(!is.na(shirt_number)))

}

############ FUNCTION rm_useless_rows END ######################
################################################################

################################################################
############ FUNCTION rename_cols BEGIN ########################

rename_cols <- function(x) {

  names(x) <- c("shirt_number", "player_name", "vote",
                "pt_tot", "pt_bp", "pt_profloss", "serv_tot",
                "serv_err", "serv_pt", "rec_tot", "rec_err",
                "rec_pos", "rec_per", "att_tot", "att_err",
                "att_blo", "att_pt", "att_per","blo_pt")

  return(x)

}

############ FUNCTION rename_cols END ##########################
################################################################


################################################################
############ FUNCTION numerize_vars BEGIN ######################

numerize_vars <- function(x) {

  return(
    x %>% mutate(rec_pos = str_remove(rec_pos, "%"),
                 rec_per = str_remove(rec_per, "%"),
                 rec_per = str_remove(rec_per, "\\("),
                 rec_per = str_remove(rec_per, "\\)"),
                 att_per = str_remove(att_per, "%")) %>%
      mutate_each(funs(as.integer), starts_with("vote")) %>%
      mutate_each(funs(as.integer), starts_with("pt_")) %>%
      mutate_each(funs(as.integer), starts_with("serv_")) %>%
      mutate_each(funs(as.integer), starts_with("rec_")) %>%
      mutate_each(funs(as.integer), starts_with("att_")) %>%
      mutate_each(funs(as.integer), starts_with("blo_pt"))
  )

}

############ FUNCTION numerize_vars END ################################################################################
########################################################################################################################

########################################################################################################################
############ FUNCTION export_teamname BEGIN ############################################################################

export_teamname <- function(x) {

    y <- names(x)

    return(
      y[1]
    )

}
############ FUNCTION export_teamname END ##############################################################################
########################################################################################################################

########################################################################################################################
############ FUNCTION return_rown_vote BEGIN ###########################################################################
#
# Function checks in which row the vote for each player is and substract it by 1 such that it can be used to subset
# the data frame. Since match reports occur in German, Italian and English, three conditions are checked to detect
# the correct row.
#
#

      return_rown_vote <- function(x) {

        if (any(str_detect(names(x), "Note")) == TRUE) {

          row_num <- which(names(x) == "Note") - 1

        } else if (any(str_detect(names(x), "Vote")) == TRUE) {

          row_num <- which(names(x) == "Vote") - 1

        } else if (any(str_detect(names(x), "Voto")) == TRUE) {

          row_num <- which(names(x) == "Voto") - 1

        } else {

          row_num <- NA

        }

        return(row_num)

    }

############ FUNCTION return_rown_vote END #############################################################################
########################################################################################################################
