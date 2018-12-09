#*****************************************************************************************
#
#  Package data objects
#
#*****************************************************************************************

#' Basic fielding statistics
#' 
#' Lahman Database fielding stats: 2014 - 2017
#' 
#' @docType data
#' @usage data(fielding_ndf)
#' @source Lahman baseball database
#' @format A \code{"fielding", 'tbl_df', 'tbl', "data.frame"} nested data.frame with 7,749 rows and 3 (4) variables
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
#' @format A \code{"batting", 'tbl_df', 'tbl', "data.frame"} nested data.frame with 5,898 rows and 3 (20) variables
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
#' @format A \code{"pitching", 'tbl_df', 'tbl', "data.frame"} nested data.frame with 2,924 rows and 3 (13) variables
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

#' 
#' Batting Projections
#' 
#' Projected batting stats (Fantasy Pros)
#' 
#' @docType data
#' @usage data(batprojs_df)
#' @source Fantasy Pros Website
#' @format A \code{"battingprojections", 'tbl_df', 'tbl', "data.frame"} data.frame with 3,479 rows and 18 variables
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
#' @format A \code{"pitchingprojections", 'tbl_df', 'tbl', "data.frame"} data.frame with 4,069 rows and 17 variables
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
#' @format A \code{"rankings", 'tbl_df', 'tbl', "data.frame"} data.frame with 1,786 rows and 10 variables
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
