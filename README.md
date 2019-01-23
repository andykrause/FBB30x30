# FBB30x30

## Files

### R

* **\_data.R**: Documentation for data objects
* **analyzeSeasons**: Functions for analyzing completed seasons
* **calcStats**: Functions for calculating advanced statistics from basic data
* **collectData**: Functions for collecting data from outside sources
* **mungeData**: Functions for cleaning and integrating data

### Scripts

* **create_data_objects.R**: Creates the objects in **\_data.R** from existing data or 
outside gathering functions.

* **sandbox.R**: Working area for construction of functions, data objects, new scripts etc. 

### Documentation

* **Data_Sources.Rmd/html**: Working collection of possible data sources
* **Progress.Rmd/html**: Running log of progress and to-do (deprecate to Github soon)

### Data

* **/raw**: Raw data files from Lahman and Fantasy Pros.  Hand downloaded. 

#### Players and Teams

* **players_df.rda**: Players, their codes and time periods from Lahman
* **teams_df.rda**: Teams with various ID codes across a number of sources

#### Stats

* **batting_df.rda**: BR Batting stats 2014-18
* **fielding_df.rda**: BR Fielding stats 2014-18
* **pitching_df.rda**: BR Pitchin stats 2014-18

#### Projections and rankings

* **batprojs_df.rda**: Fantasy Pros batting projections 2014-18
* **pitchprojs_df.rda**: Fantasy Pros pitching projections 2014-18
* **marcel2018_proj.rda**: Marcel batting and pitching (BR) projections 2018
* **rankings_df.rda**: Fantasy Pros draft rankings 2014-18

#### League

* **seasons_.rda**: Completed season data from 30x30 league 2015-2018

#### Metadata

* **baseballref_df.rda**: List of available Baseball-Reference data for downloading
