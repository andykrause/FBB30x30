#*****************************************************************************************
#
#  Package data objects
#
#*****************************************************************************************

### Data Sources -------------------------------------------------------------------------

#' 
#' Baseball Reference tables and codes
#' 
#' Types of data from Baseball-reference.com (not complete)
#' 
#' @docType data
#' @usage data(baseballref_df)
#' @source Author compiled
#' @format A \code{'tbl_df', 'tbl', "data.frame"} with 17 rows and 2 columns
#'  \describe{
#'   \item{data}{Type of data [chr]}
#'   \item{link_or_code}{html link or codes for functions [chr]}
#' }
'baseballref_df'

### Players and Teams --------------------------------------------------------------------

#' 
#' Team Identifier Codes
#' 
#' Team codes from various sources
#' 
#' @docType data
#' @usage data(teams_df)
#' @source Author compiled
#' @format A \code{"seasons", 'tbl_df', 'tbl', "data.frame"} with 30 rows and 6 columns
#'  \describe{
#'   \item{fp_id}{Fantasy Pros team id [chr]}
#'   \item{br_id}{Baseball reference team id [chr]}
#'   \item{lahman_id}{Lahman team id [chr]}
#'   \item{retro_id}{Retro sheet standard Pros team id [chr]}
#'   \item{espn_id}{ESPN team id [chr]}
#'   \item{retrosheet_id}{Retro sheet data scraping team id [chr]}
#' }
'teams_df'

#' 
#' Player Information
#' 
#' Unique IDs, player names and last game information
#' 
#' @docType data
#' @usage data(players_df)
#' @source Lathman (see create_data_objects.R scripts)
#' @format A \code{"players", 'tbl_df', 'tbl', "data.frame"} with 2444 rows and 8 columns
#'  \describe{
#'   \item{player_id}{Lahman id [chr]}
#'   \item{retro_id}{Retrosheet id [chr]}
#'   \item{bbref_id}{Baseball reference id [chr]}
#'   \item{first}{First name [chr]}
#'   \item{last}{Last name [chr]}
#'   \item{given}{Given name [chr]}
#'   \item{last_game}{Last game played in [date]}
#'   \item{full}{Full name [chr]}
#' }
'players_df'

### Statistics ---------------------------------------------------------------------------

#' Basic fielding statistics
#' 
#' Lahman Database fielding stats: 2014 - 2017
#' 
#' @docType data
#' @usage data(fielding_ndf)
#' @source Lahman baseball database
#' @format A \code{"fielding", 'tbl_df', 'tbl', "data.frame"} nested data.frame with 
#' 7,749 rows and 3 (4) variables
#' \describe{
#'   \item{player_id}{The unique Lahman player ID [fct]}
#'   \item{year}{Year of data [int]}
#'   \item{fielding_stats}{Nested data.frame of fielding stats [list]}
#'   \item{...team}{Players team (Lahman code) [fct]}
#'   \item{...pos}{Position played [fct]}
#'   \item{...g}{Games at that position [int]}
#'   \item{...e}{Errors at that position [int]}
#'}
"fielding_ndf"

#' Basic batting statistics
#' 
#' Lahman Database batting stats: 2014 - 2017
#' 
#' @docType data
#' @usage data(batting_ndf)
#' @source Lahman baseball database
#' @format A \code{"batting", 'tbl_df', 'tbl', "data.frame"} nested data.frame with 5,898
#'  rows and 3 (20) variables
#' \describe{
#'   \item{player_id}{The unique Lahman player ID [fct]}
#'   \item{year}{Year of data [int]}
#'   \item{batting_stats}{Nested data.frame of batting stats [list]}
#'   \item{...team}{Players team (Lahman code) [fct]}
#'   \item{...g}{Games at that position [int]}
#'   \item{...ab}{At bats [int]}
#'   \item{...r}{Runs [int]}
#'   \item{...hr}{Home Runs [int]}
#'   \item{...rbi}{Runs batted in [int]}
#'   \item{...sb}{Stolen bases [int]}
#'   \item{...so}{Strikeouts [int]}
#'   \item{...gidp}{Ground into double plays [int]}
#'   \item{...sh}{Sacrifice hits (bunts) [int]}
#'   \item{...sf}{Sacrifice flies [int]}
#'   \item{...h}{Hits [int]}
#'   \item{...x2b}{Doubles [int]}
#'   \item{...x3b}{Triples [int]}
#'   \item{...ibb}{Intentional Walks [int]}
#'   \item{...hbp}{Hit by pitch [int]}
#'   \item{...bb}{Base on balls (Walks) [int]}
#'   \item{...slg}{Slugging percentage [dbl]}
#'   \item{...pa}{Plate Appearances [int]}
#'   \item{...obp}{On base percentage [dbl]}
#'}
"batting_ndf"

#' Basic pitching statistics
#' 
#' Lahman Database pitching stats: 2014 - 2017
#' 
#' @docType data
#' @usage data(pitching_ndf)
#' @source Lahman baseball database
#' @format A \code{"pitching", 'tbl_df', 'tbl', "data.frame"} nested data.frame with 
#' 2,924 rows and 3 (13) variables
#' \describe{
#'   \item{player_id}{The unique Lahman player ID [fct]}
#'   \item{year}{Year of data [int]}
#'   \item{pitching_stats}{Nested data.frame of pitching stats [list]}
#'   \item{...team}{Players team (Lahman code) [fct]}
#'   \item{...w}{Wins [int]}
#'   \item{...g}{Games [int]}
#'   \item{...gs}{Games started [int]}
#'   \item{...h}{Hits allowed [int]}
#'   \item{...er}{Earned runs allowed [int]}
#'   \item{...bb}{Base on balls issued [int]}
#'   \item{...k}{Strikeouts [int]}
#'   \item{...ibb}{Intentional Walks [int]}
#'   \item{...sv}{Saves [int]}
#'   \item{...ip}{Innings pitched [int]}
#'   \item{...whip}{(Walks + Hits) / Innings Pitched [dbl]}
#'}
"pitching_ndf"

### Projections and Rankings -------------------------------------------------------------

#' 
#' Batting Projections
#' 
#' Projected batting stats (Fantasy Pros)
#' 
#' @docType data
#' @usage data(batprojs_df)
#' @source Fantasy Pros Website
#' @format A \code{"battingprojections", 'tbl_df', 'tbl', "data.frame"} data.frame with 
#' 3,479 rows and 18 variables
#' \describe{
#'   \item{player}{Standard player name [fct]}
#'   \item{year}{Year of data [int]}
#'   \item{team}{Players team (ESPN code) [fct]}
#'   \item{pos}{Position(s) [char]}
#'   \item{ab}{At bats [int]}
#'   \item{h}{Hits [int]}
#'   \item{x2b}{Doubles [int]}
#'   \item{x3b}{Triples [int]}
#'   \item{r}{Runs [int]}
#'   \item{hr}{Home Runs [int]}
#'   \item{rbi}{Runs batted in [int]}
#'   \item{bb}{Base on balls (Walks) [int]}
#'   \item{sb}{Stolen bases [int]}
#'   \item{so}{Strikeouts [int]}
#'   \item{avg}{Batting average [dbl]}
#'   \item{obp}{On base percentage [dbl]}
#'   \item{slg}{Slugging percentage [dbl]}
#'   \item{ops}{On base percentage plus slugging [dbl]}
#'}
"batprojs_df"

#' 
#' Pitching Projections
#' 
#' Projected pitching stats (Fantasy Pros)
#' 
#' @docType data
#' @usage data(pitchprojs_df)
#' @source Fantasy Pros Website
#' @format A \code{"pitchingprojections", 'tbl_df', 'tbl', "data.frame"} data.frame with 
#' 4,069 rows and 17 variables
#' \describe{
#'   \item{player}{Standard player name [fct]}
#'   \item{year}{Year of data [int]}
#'   \item{team}{Players team (ESPN code) [fct]}
#'   \item{g}{Games [int]}
#'   \item{gs}{Games started [int]}
#'   \item{cg}{Complete games [int]}
#'   \item{sv}{Saves [int]}
#'   \item{ip}{Innings pitched [int]}
#'   \item{w}{Wins [int]}
#'   \item{l}{Losses [int]}
#'   \item{h}{Hits allowed [int]}
#'   \item{er}{Earned runs allowed [int]}
#'   \item{k}{Strikeouts [int]}
#'   \item{bb}{Base on balls issued [int]}
#'   \item{era}{Earned Run Average [dbl]}
#'   \item{whip}{(Walks + Hits) / Innings Pitched [dbl]} 
#' }
"pitchprojs_df"

#' 
#' Draft Rankings
#' 
#' Projected draft rankings (Fantasy Pros)
#' 
#' @docType data
#' @usage data(rankings_df)
#' @source Fantasy Pros Website
#' @format A \code{"rankings", 'tbl_df', 'tbl', "data.frame"} data.frame with 1,786 rows 
#' and 10 variables
#' \describe{
#'   \item{player}{Standard player name [chr]}
#'   \item{team}{Players team (ESPN code) [chr]}
#'   \item{pos}{Player position(s) [chr]}
#'   \item{year}{Year of data [int]}
#'   \item{rank}{Overall player ranking [int]}
#'   \item{best}{Best draft position [int]}
#'   \item{worst}{Worst draft position [int]}
#'   \item{avg}{Average drafted position [dbl]}
#'   \item{std.dev}{St. Dev of average draft position [dbl]}
#'   \item{adp}{Average draft position (rounded and rank ordered) [dbl]}
#' }
"rankings_df"

### Past Seasons -------------------------------------------------------------------------

#' 
#' Season Results
#' 
#' Previous Season Results from 2015 to 2018
#' 
#' @docType data
#' @usage data(seasons_)
#' @source Author's own Leagues
#' @format A list with individual \code{"seasons", 'tbl_df', 'tbl', "data.frame"} 
#' data.frames with varying rows and variables.  Most common variables are:
#'  \describe{
#'   \item{manager}{Unique team manager (ESPN) id [int]}
#'   \item{rank}{Season Ranking [int]}
#'   \item{r}{Runs accumulated [int]}
#'   \item{hr}{Home runs accumulated [int]}
#'   \item{rbi}{RBIs accumulated [int]}
#'   \item{so}{Strikeouts (batter) accumulated [int]}
#'   \item{sb}{Stolen bases accumulated [int]}
#'   \item{obp}{On base percentage accumulated [num]}
#'   \item{slg}{Slugging percentage accumulated [num]}
#'   \item{gidp}{Grounded into double plays accumulated [int]}
#'   \item{e}{Errors accumulated [int]}
#'   \item{ip}{Innings pitched accumulated [int]}
#'   \item{qs}{Quality starts accumulated [int]}
#'   \item{k}{Strikeouts (pitcher) accumulated [int]}
#'   \item{sv}{Saves accumulated [int]}
#'   \item{hd}{Holds accumulated [int]}
#'   \item{whip}{WHIP accumulated [num]}
#'   \item{moves}{Moves made during the season [int]}
#'   \item{starts}{Pitchers starts used [int]}
#' }
"seasons_"




