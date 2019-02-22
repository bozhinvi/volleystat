#' Players data
#'
#' A dataset containing attributes of all players
#' in the German first division volleyball league for each season
#' starting in 2013/2014 and ending in 2017/2018.
#'
#' @format Data frame with 1517 rows and 13 columns.
#' \describe{
#'   \item{league_gender}{Men or women's league.}
#'   \item{season_id}{ID for each season unique within league_gender. It consists of the last two digits of the season
#'   years, e.g., the season_id of season 2014/2015 equals 1415.}
#'   \item{team_id}{Team identifier which is unique across all teams and seasons.}
#'   \item{team_name}{Official name of the team. Note that VCO Berlin has a team in men's and women's league.}
#'   \item{player_id}{Player identifier which is unique within a team in a season. This identifier equals to the
#'   shirt number of the player. However, in cases where more than one player has the same shirt number, the identifier
#'   is replaced by a 3-digit number starting at 100.}
#'   \item{shirt_number}{Official shirt number, not unique within teams (see player_id).}
#'   \item{position}{Player's specialization}
#'   \item{firstname}{First name of the player}
#'   \item{lastname}{Last name of the player}
#'   \item{gender}{Player's gender: male or female.}
#'   \item{birthdate}{Date of birth}
#'   \item{height}{Height in cm}
#'   \item{nationality}{Nationality of player}
#' }
#' @source \url{https://www.volleyball-bundesliga.de/cms/home/1blf/1blf_archiv/1blf_tabellen_ergebnisse.xhtml}
#'         \url{https://www.volleyball-bundesliga.de/cms/home/1blm/1blm_archiv/1blm_tabellen_ergebnisse.xhtml}
#' @examples players
"players"

#' Team staff data
#'
#' A dataset containing attributes of all staff members of teams
#' in the German first division volleyball league for each season
#' starting in 2013/2014 and ending in 2017/2018.
#'
#' @format A data frame with 983 rows and 10 variables:
#' \describe{
#'   \item{league_gender}{Men or women's league.}
#'   \item{season_id}{ID for each season unique within league_gender. It consists of the last two digits of the season
#'   years, e.g., the season_id of season 2014/2015 equals 1415.}
#'   \item{team_id}{Team identifier which is unique across all teams and seasons.}
#'   \item{team_name}{Official name of the team. Note that VCO Berlin has a team in men's and women's league.}
#'   \item{firstname}{First name of the player}
#'   \item{lastname}{Last name of the player}
#'   \item{gender}{Player's gender: male or female.}
#'   \item{birthdate}{Date of birth}
#'   \item{position}{Person's role in the team, e.g., couch, statistician, or assistant couch.}
#'   \item{nationality}{Nationality of team member.}
#' }
#' @source \url{https://www.volleyball-bundesliga.de/cms/home/1blf/1blf_archiv/1blf_tabellen_ergebnisse.xhtml}
#'         \url{https://www.volleyball-bundesliga.de/cms/home/1blm/1blm_archiv/1blm_tabellen_ergebnisse.xhtml}
#' @examples staff
"staff"

#' Matches data
#'
#' A dataset containing all matches of the German first division volleyball league for each season
#' starting in 2013/2014 and ending in 2017/2018. Note that all matches are included twice in the
#' dataset, i.e., from the perspective of the home team and from the perspective of the away team.
#'
#' @format Data frame with 2778 rows and 12 columns.
#' \describe{
#'   \item{league_gender}{Men or women's league.}
#'   \item{season_id}{ID for each season unique within league_gender. It consists of the last two digits of the season
#'   years, e.g., the season_id of season 2014/2015 equals 1415.}
#'   \item{competition_stage}{Stage of the competition, i.e., main round or play-offs.}
#'   \item{match_id}{Official VBL match id. Unique within seasons.}
#'   \item{match}{Factor variable identifying whether observation is from the home team's perspective or
#'   from the away team's perspective.}
#'   \item{match_day}{Match day (only main round, see competition_stage).}
#'   \item{date_time}{POSIXct date-time variable of the match.}
#'   \item{spectators}{Number of spectators in the gym.}
#'   \item{match_duration}{Length of match in minutes.}
#'   \item{team_id}{Team identifier which is unique across all teams and seasons.}
#'   \item{team_name}{Official name of the team. Note that VCO Berlin has a team in men's and women's league.}
#'   \item{set_won}{Number of sets won by the team.}
#' }
#' @source \url{https://www.volleyball-bundesliga.de/cms/home/1blf/1blf_archiv/1blf_tabellen_ergebnisse.xhtml}
#'         \url{https://www.volleyball-bundesliga.de/cms/home/1blm/1blm_archiv/1blm_tabellen_ergebnisse.xhtml}
#' @examples matchstats
"matches"

#' Sets data
#'
#' A dataset containing all matches of the German first division volleyball league for each season on set level.
#' starting in 2013/2014 and ending in 2017/2018. Note that all sets are included twice in the
#' dataset, i.e., from the perspective of the home team and from the perspective of the away team.
#'
#' @format Data frame with 10374 rows and 9 columns.
#' \describe{
#'   \item{league_gender}{Men or women's league.}
#'   \item{season_id}{ID for each season unique within league_gender. It consists of the last two digits of the season
#'   years, e.g., the season_id of season 2014/2015 equals 1415.}
#'   \item{match_id}{Official VBL match id. Unique within seasons.}
#'   \item{match}{Factor variable identifying whether observation is from the home team's perspective or
#'   from the away team's perspective.}
#'   \item{team_id}{Team identifier which is unique across all teams and seasons.}
#'   \item{team_name}{Official name of the team. Note that VCO Berlin has a team in men's and women's league.}
#'   \item{set}{Set identifier.}
#'   \item{set_duration}{Length of set in minutes.}
#'   \item{pt_set}{Points scored in set.}
#' }
#' @source \url{https://www.volleyball-bundesliga.de/cms/home/1blf/1blf_archiv/1blf_tabellen_ergebnisse.xhtml}
#'         \url{https://www.volleyball-bundesliga.de/cms/home/1blm/1blm_archiv/1blm_tabellen_ergebnisse.xhtml}
#' @examples matchstats
"sets"

#' Matchstats data
#'
#' A dataset containing match-player level statistics of the German first division volleyball league for each match and
#' each season starting in 2013/2014 and ending in 2017/2018.
#'
#' @format Data frame with 26692 rows and 23 columns.
#' \describe{
#'   \item{league_gender}{Men or women's league.}
#'   \item{season_id}{ID for each season unique within league_gender. It consists of the last two digits of the season
#'   years, e.g., the season_id of season 2014/2015 equals 1415.}
#'   \item{match_id}{Official VBL match id. Unique within seasons.}
#'   \item{team_id}{Team identifier which is unique across all teams and seasons.}
#'   \item{player_id}{Player identifier which is unique within a team in a season. This identifier equals to the
#'   shirt number of the player. However, in cases where more than one player has the same shirt number, the identifier
#'   is replaced by a 3-digit number starting at 100.}
#'   \item{shirt_number}{Official shirt number, not unique within teams (see player_id).}
#'   \item{vote}{Score computed according to the DataVolley settings. NA if not reported as numeric in the match report.}
#'   \item{pt_tot}{Total points scored.}
#'   \item{pt_bp}{Total break points scored.}
#'   \item{pt_profloss}{Won minus lost points. More precisely, this variable is computed according to the following
#'   equation: serv_pt + att_pt + blo_pt - serv_err - rec_err - att_err - att_blo.}
#'   \item{serv_tot}{Total number of serves.}
#'   \item{ser_err}{Number of serving errors.}
#'   \item{serv_pt}{Number of scored points at service, e.g., aces.}
#'   \item{rec_tot}{Number of receptions.}
#'   \item{rec_err}{Number of errors in reception.}
#'   \item{rec_pos}{Share of positive receptions.}
#'   \item{rec_per}{Share of perfect receptions.}
#'   \item{att_tot}{Number of total attacks.}
#'   \item{att_err}{Number of failed attacks.}
#'   \item{att_blo}{Number of blocked attacks.}
#'   \item{att_pt}{Number of successful attacks.}
#'   \item{att_per}{Share of successful attacks.}
#'   \item{blo_pt}{Number of successful block.}
#' }
#' @source \url{https://www.volleyball-bundesliga.de/cms/home/1blf/1blf_archiv/1blf_tabellen_ergebnisse.xhtml}
#'         \url{https://www.volleyball-bundesliga.de/cms/home/1blm/1blm_archiv/1blm_tabellen_ergebnisse.xhtml}
#' @examples matchstats
"matchstats"

