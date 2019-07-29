library(rvest)
library(httr)
library(lubridate)
library(tidyverse)

# read in the scraped data
afl_pre_19 <- read.csv("data/afl_games_pre2019.csv", stringsAsFactors = F)
afl2019 <- read.csv("data/afl_games_2019.csv", stringsAsFactors = F)

# combine into one DF
all_data <- rbind(afl_pre_19, afl2019) %>% data.frame()

# remove the individual datasets
rm(afl_pre_19, afl2019);gc()

# Data pre-processing -----------------------------------------------------

# make all variables character type to make splitting and string manipulation easier
all_data <- all_data %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(team1_score = as.numeric(team1_score),
         team2_score = as.numeric(team2_score))


# North Melbourne have their name as both 'North Melbourne' and 'Kangaroos'
all_data <- all_data %>% 
  mutate(team1 = ifelse(team1 == "Kangaroos", "North Melbourne", team1),
         team2 = ifelse(team2 == "Kangaroos", "North Melbourne", team2))


# Splitting metadata into meaningful variables ----------------------------

# split out round info
all_data_cleaned <- all_data %>% 
 separate(col = match_facts, into= c("round", "info"), sep = "Venue:") 
# remove the "round" from the string and only keep the round number
all_data_cleaned <- all_data_cleaned %>% 
  mutate(round = sub("Round: ", "", round))

# split out the venue information to its own variable 
all_data_cleaned <- all_data_cleaned %>% 
  separate(col = info, into = c("venue", "info"), sep = "Date:")

# split out the attendance information to its own variable 
all_data_cleaned <- all_data_cleaned %>% 
  separate(col = info, into = c("date", "attendance"), sep = "Attendance:")
all_data_cleaned <- all_data_cleaned %>% 
  mutate(attendance = as.numeric(attendance))

# split out the date information to its own variable, remove the time in parentheses and only keep the local game start time 
all_data_cleaned <- all_data_cleaned %>% 
  separate(date, into = c("date", NA), sep = "\\(") 
# remove the day of week from the date variable - this can always be added in using the lubridate package
all_data_cleaned <- all_data_cleaned %>% 
  separate(date, into = c("weekday", "date"), sep = ", ")
# split out the date variable to separate variables for date and time
all_data_cleaned <- all_data_cleaned %>% 
  separate(date, into = c("date", "start_time", "am_pm"), sep = " ")

all_data_cleaned <- all_data_cleaned %>% 
  mutate(date = ymd(as.Date(date, format="%d-%B-%Y")))


all_data_cleaned <- all_data_cleaned %>% 
  mutate_if(is.character, str_squish)

# Feature Engineering -----------------------------------------------------

# create a variable for the season
all_data_cleaned <- all_data_cleaned %>% 
  mutate(season = year(date))

# create a variable for period in the day - afternoon, evening, night
all_data_cleaned <- all_data_cleaned %>% 
  mutate(start_hour = ifelse(am_pm == "PM" & as.numeric(sub("\\:.*", "", start_time)) < 12, as.numeric(sub("\\:.*", "", start_time)) + 12, as.numeric(sub("\\:.*", "", start_time)))) %>% 
  mutate(time_period = ifelse(between(start_hour, 11, 16), "Afternoon", ifelse(between(start_hour, 17, 18), "Evening", "Night")))

# create a variable that calculates the margin, total combined score and whether the home team won (team_1)
all_data_cleaned <- all_data_cleaned %>% 
  mutate(margin = team1_score - team2_score,
         total_score = team1_score + team2_score,
         winner = ifelse(team1_score > team2_score, "Home", ifelse(team2_score > team1_score, "Away", "Draw"))) 


venue <- all_data_cleaned %>% count(venue) %>% pull(venue)
VenueCity <- c("Adelaide", "Hobart", "Sydney", "Gold Coast", "Cairns", "Melbourne", "Ballarat", "Adelaide", "Brisbane", "China", "Geelong", "Melbourne", "Canberra", "Darwin", "Perth", "Melbourne", "Gold Coast", "Sydney", "Sydney", "Perth", "Sydney", "Darwin", "Perth", "NZ", "Launceston")

venues_df <- cbind(venue, VenueCity) %>% data.frame() %>% mutate_if(is.factor, as.character)

rm(all_data);gc()

# join to the full DF
all_data_cleaned <- all_data_cleaned %>% 
  left_join(venues_df, by = "venue")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Rain Data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# rain_data <- read.csv("data/cleaned_data/preprocessed_rain_data.csv", stringsAsFactors = F) %>% data.frame()
weather_data <- read.csv("data/cleaned_data/preprocessed_rain_temp_data.csv", stringsAsFactors = F) %>% data.frame()

# select only required variables
weather_data <- weather_data %>%
  select(weather_date, rainfall_clean, VenueCity = cities, min_temp, max_temp)

# ensure date variable in weather data is Date type
weather_data <- weather_data %>% mutate(weather_date = ymd(weather_date))

# there are a few Melbourne rain events not captured... will input them here manually
weather_data <- weather_data %>% 
  mutate(rainfall_clean = ifelse(VenueCity == "Melbourne" & weather_date == '2019-07-20', 0, rainfall_clean)) %>% 
  mutate(rainfall_clean = ifelse(VenueCity == "Melbourne" & weather_date == '2019-07-21', 0, rainfall_clean))

# calculate the last five days rain from each date
weather_data <- weather_data %>% 
  group_by(VenueCity) %>% 
  mutate(last_five_days_rain = rainfall_clean + lag(rainfall_clean, 1) + lag(rainfall_clean, 2) + lag(rainfall_clean, 3)+ lag(rainfall_clean, 4)) %>% 
  ungroup()

# Join back to main df
all_data_cleaned <- all_data_cleaned %>%
  left_join(weather_data, by = c("date" = "weather_date", "VenueCity"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AFL Betting Data --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in function to get and clean betting data from web
source("R/return_betting_data.R")

# create betting data DF
betting_data <- return_betting_data()

# Need to make joinging data possible, so will need to change GWS Giants to "Greater Western Sydney"
betting_data <- betting_data %>% 
  mutate(HomeTeam = ifelse(HomeTeam == "GWS Giants", "Greater Western Sydney", HomeTeam),
         AwayTeam = ifelse(AwayTeam == "GWS Giants", "Greater Western Sydney", AwayTeam)) %>% 
  mutate(HomeTeam = ifelse(HomeTeam == "Brisbane", "Brisbane Lions", HomeTeam),
         AwayTeam = ifelse(AwayTeam == "Brisbane", "Brisbane Lions", AwayTeam)) %>% 
  mutate(GameID = paste(Date, HomeTeam, AwayTeam, sep = "-"))


# create a GamiID varibale for joining and join betting data to main DF
all_data_cleaned <- all_data_cleaned %>% 
  mutate(GameID = paste(date, team1, team2, sep = "-")) %>% 
  left_join(betting_data, by = "GameID")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feature Engineering -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# theses four Melbourne-based teams are traditional rivals - I will flag games played between them as rivalry games
melbourne_rivals <- c("Carlton", "Collingwood", "Essendon", "Richomond")

# also, the derby in bith SA and WA are fierce rivalries, as is Hawthorn v Geelong games
all_data_cleaned <- all_data_cleaned %>% 
  mutate(rivalry_game = ifelse(team1 %in% c("West Coast", "Fremantle") & team2 %in% c("West Coast", "Fremantle"), "Rivalry",
                       ifelse(team1 %in% c("Adelaide", "Port Adelaide") & team2 %in% c("Adelaide", "Port Adelaide"), "Rivalry", 
                              #ifelse(team1 %in% c("Hawthorn", "Geelong") & team2 %in% c("Hawthorn", "Geelong"), "Rivalry",
                                     ifelse(team1 %in% melbourne_rivals & team2 %in% melbourne_rivals, "Rivalry", "Normal"))))


# calculate the odds differences, if it rained or not, and whether the home team is the favourite
all_data_cleaned <- all_data_cleaned %>% 
  mutate(odds_diff = abs(HomeOddsOpen - AwayOddsOpen),
         HomeTeamFav = ifelse(HomeOddsOpen > AwayOddsOpen, "Yes", "No"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculating The Team's Prior Result -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# to do this, the team's need to be in the one column, and need to indicate whether the team is the home or away team
last_weeks_result <- bind_rows(all_data_cleaned %>% select(team = team1, round, season, winner) %>% mutate(home_or_away_team = "Home"),
                  all_data_cleaned %>% select(team = team2, round, season, winner) %>% mutate(home_or_away_team = "Away"))

# filter out the finals series games
last_weeks_result <- last_weeks_result %>% filter(!str_detect(round, "Final")) %>% mutate(round = as.numeric(round))
# determine whether the team won or lost
last_weeks_result <- last_weeks_result %>% arrange(team, season, round) %>% mutate(result = ifelse(winner == home_or_away_team, "Won", "Lost"), round = as.character(round))
# use lag to calculate the result from the previous week
last_weeks_result <- last_weeks_result %>% group_by(team) %>% mutate(last_result = lag(result)) %>% ungroup()
# # round 1 games will not have a previous week, so just flag this
# last_weeks_result <- last_weeks_result %>% mutate(last_result = ifelse(is.na(last_result), "Round 1", last_result))

# now join back to the main dataset
all_data_cleaned <- all_data_cleaned %>%
  left_join(last_weeks_result %>% select(team, season, round, last_result), by = c("team1" = "team", "season", "round")) %>% rename(team1_last_result = last_result) %>% 
  left_join(last_weeks_result %>% select(team, season, round, last_result), by = c("team2" = "team", "season", "round")) %>% rename(team2_last_result = last_result)




# Calculating Full or Split Rounds ----------------------------------------

round_games <- all_data_cleaned %>% filter(!str_detect(round, "Final")) %>% mutate(round = as.numeric(round)) %>% 
  count(season, round) %>% ungroup() %>% rename(games_this_round = n) %>% arrange(season, round) %>% mutate(round = as.character(round))

# Before the 2011 season, there were 8 games every round, but this changed in 2011 when Goald Coast was introduced, 
# bringing the total teams to 17, meaning there was always one team on bye.
# from 2012, the full number of games per round was 9, so any round that didn't have 9 games is considered a split round.
round_games <- round_games %>% 
  mutate(split_round = ifelse(season < 2012 | games_this_round == 9, "Full", "Split"))

# join back to the main df
all_data_cleaned <- all_data_cleaned %>%
  left_join(round_games %>% select(season, round, split_round), by = c("season", "round"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rest of analysis for Premiership Season Only ----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# filter out finals rounds
afl_premiership_season <- all_data_cleaned %>% filter(!str_detect(round, "Final"))


# Feature for whether the team played finals last season ------------------
# create DF of finals games only
finals <- all_data_cleaned %>% filter(str_detect(round, "Final"))

# convert to long DF
played_finals_last_season <- finals %>% 
  select(season, Team = team1) %>% 
  rbind(
    finals %>% select(season, Team = team2)
  ) %>% 
  distinct(.keep_all = T)

played_finals_last_season <- played_finals_last_season %>% mutate(last_season = season + 1)

rm(finals)

# join finals feature to main DF, clean up column names
afl_premiership_season <- afl_premiership_season %>% 
  left_join(played_finals_last_season, by = c("season" = "last_season", "team1" = "Team"))

afl_premiership_season <- afl_premiership_season %>% 
  mutate(home_team_finals_last_season = ifelse(is.na(season.y), "No", "Yes")) %>% select(-season.y)


afl_premiership_season <- afl_premiership_season %>% 
  left_join(played_finals_last_season, by = c("season" = "last_season", "team2" = "Team"))

afl_premiership_season <- afl_premiership_season %>% 
  mutate(away_team_finals_last_season = ifelse(is.na(season.y), "No", "Yes")) %>% select(-season.y)



# Feature to indicate which of the playing teams played finals last season --------
afl_premiership_season <- afl_premiership_season %>% 
  mutate(finals_last_season = ifelse(home_team_finals_last_season == "Yes" & away_team_finals_last_season == "Yes", "both_played",
                                     ifelse(home_team_finals_last_season == "Yes" & away_team_finals_last_season == "No", "home_only_played",
                                            ifelse(home_team_finals_last_season == "No" & away_team_finals_last_season == "Yes", "away_only_played", "neither_played"))))



# Feature for classifying game start times --------------------------------

# because there is too many combinations of day of week, start times, etc, variables will be created to group these...ie weekday = Saturday, start time = 4pm, then "Sat Afternoon".
# weekdays will be classed as "Weekday"
weekend_days <- c("Fri", "Sat", "Sun")


# feature engineering
afl_premiership_season <- afl_premiership_season %>%
  mutate(season_stage = ifelse(between(as.numeric(round), 1, 6), "First Quarter", ifelse(between(as.numeric(round), 7, 12), "Second Quarter", ifelse(between(as.numeric(round), 13, 18), "Third Quarter", "Last Quarter")))) %>%
  mutate(time_period = ifelse(between(start_hour, 11, 15), "Afternoon", ifelse(between(start_hour, 16, 17), "Evening", "Night"))) %>%
  mutate(weekday = ifelse(weekday %in% weekend_days, weekday, "Weekday")) %>% 
  mutate(game_time = paste(weekday, time_period, sep = ' ')) %>%
  mutate(game_month = month(date, label = T))


# Feature for using min temp for night games and max temp for day games  --------
afl_premiership_season <- afl_premiership_season %>% 
  mutate(temperature = ifelse(start_hour < 18, max_temp, min_temp))


# Milestone games features ------------------------------------------------
afl_premiership_season <- afl_premiership_season %>% 
  mutate(IsHomeMilestone = ifelse(home_milestone > 0 | home_300 > 0, "Yes", "No"),
         IsAwayMilestone = ifelse(away_milestone > 0 | away_300 > 0, "Yes", "No"),
         count_milestones = home_300 + home_milestone + away_300 + away_milestone)


# Feature for last results of both teams ----------------------------------
afl_premiership_season <- afl_premiership_season %>% 
  mutate(last_results = ifelse(team1_last_result == "Won" & team2_last_result == "Won", "both_won",
                               ifelse(team1_last_result == "Lost" & team2_last_result == "Lost", "both_lost",
                                      ifelse(team1_last_result == "Won" & team2_last_result == "Lost", "only_home_team_won",
                                             ifelse(team1_last_result == "Lost" & team2_last_result == "Won", "only_away_team_won", "Other")))))




# Feature for teams from same or diff states ------------------------------
# create a dataframe of teams and the state they're from
teams <- sort(unique(afl_premiership_season$team1))
states <- c("SA", "QLD", "VIC", "VIC", "VIC", "WA", "VIC", "QLD", "NSW", "VIC", "VIC", "VIC", "SA", "VIC", "VIC", "NSW", "WA", "VIC")
team_states <- data.frame(teams = teams, states = states) %>% mutate_all(as.character)

# join team_states df to main DF and create feature
afl_premiership_season <- afl_premiership_season %>% 
  left_join(team_states, by = c("team1" = "teams")) %>% 
  left_join(team_states, by = c("team2" = "teams")) %>% 
  rename(home_team_state = states.x, away_team_state = states.y) %>% 
  mutate(is_same_state = ifelse(home_team_state == away_team_state, "yes", "no"))


# Feature for diff in min and max temps -----------------------------------
afl_premiership_season <- afl_premiership_season %>% 
  mutate(temperature_diff = max_temp - min_temp)



# Feature to calculate change in opening and closing odds -----------------
afl_premiership_season <- afl_premiership_season %>% 
  mutate(home_odds_change = HomeOddsClose - HomeOddsOpen,
         away_odds_change = AwayOddsClose - AwayOddsOpen,
         home_line_change = HomeLineClose - HomeLineOpen,
         away_line_change = AwayLineClose - AwayLineOpen)



# Feature for stadiums that are not primary AFL home grounds --------------
secondary_venues <- c("Bellerive Oval", "Eureka Stadium", "Jiangwan Stadium", "Marrara Oval", "Riverway Stadium", "Stadium Australia", "Traeger Park", "Wellington", "York Park")

afl_premiership_season <- afl_premiership_season %>% 
  mutate(second_home_game = ifelse(venue %in% secondary_venues, "Yes", "No"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to create a ladder as at every round of every season
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_ladder <- function(afl_df) {
  
  # create a long df, with each observation being a team, for the round, for the season
  team_view <- afl_premiership_season %>% 
    select(Team = team1, round, season, winner, Score = team1_score, OppScore = team2_score) %>% 
    mutate(round = as.numeric(round), home_or_away = "Home") %>%
    bind_rows(afl_premiership_season %>% 
                select(Team = team2, round, season, winner, Score = team2_score, OppScore = team1_score) %>% 
                mutate(round = as.numeric(round), home_or_away = "Away"))  %>%
    mutate(win = ifelse(winner == "Draw", 0.5, ifelse(winner == home_or_away, 1, 0))) %>% 
    mutate(points = win * 4)
  
  
  
  # because there were byes throughout, some teams are missing for ladder construction purposes
  # ie in some rounds, there aren't the right amount of teams in each round
  df <- team_view %>%
    distinct(season, Team) %>% 
    left_join(team_view %>% 
                distinct(season, round), by = "season") %>% 
    left_join(team_view, by = c("season", "round", "Team")) %>% 
    select(-winner, -home_or_away)
  
  
  # function to replace the missing results (ie where the team had a bye) with zeros
  replace_with_zero <- function(x){
    if(is.na(x)) {x <- 0
    } else {
      x <- x
    }
  }
  
  # fill in the missing values with zeros
  df <- df %>% 
    mutate(Score = mapply(replace_with_zero, Score),
           OppScore = mapply(replace_with_zero, OppScore),
           win = mapply(replace_with_zero, win),
           points = mapply(replace_with_zero, points))
  
  
  # calculate cumulative scores for each team
  df <- df %>% 
    mutate(round = as.numeric(round)) %>% 
    arrange(season, Team, round) %>% 
    group_by(season, Team) %>% 
    mutate(season_points = cumsum(points),
           score_for = cumsum(Score),
           score_against = cumsum(OppScore),
           percentage = score_for / score_against) %>% ungroup()
  
  # Round 1 in 2011, Gold Coast had a bye in round 1, so need to fix the NaN for their percentage (R doesn't like 0 / 0)
  df$percentage[is.nan(df$percentage)] <- 0
  
  # arrange teams so that the top ranked team is at the top
  ladder <- df %>%
    arrange(season, round, desc(season_points), desc(percentage))
  
  # apply the ladder position for each round. Because there were different numbers of teams each season, need to find out how many teams
  for(i in unique(ladder$season)){
    num_teams <- length(unique(ladder$Team[ladder$season == i]))
    ladder$ladder_pos[ladder$season == i] <- rep(1:num_teams)
  }
  
  return(ladder)
}

# create the ladder
ladder <- create_ladder(afl_premiership_season)

# because we will want to use the current week's ladder position for the following week's game, need to add 1 to the round variable
ladder <- ladder %>% mutate(round = round + 1)

# join ladder data for home team (team1)
afl_premiership_season <- afl_premiership_season %>% 
  left_join(ladder %>% 
              select(season, Team, round, home_season_points = season_points, home_score_for = score_for, home_score_against = score_against, home_percentage = percentage, home_ladder_pos = ladder_pos) %>% 
              mutate(round = as.character(round)), 
            by = c('team1' = 'Team', 'season', 'round'))

# join ladder data for away team (team2)
afl_premiership_season <- afl_premiership_season %>% 
  left_join(ladder %>% 
              select(season, Team, round, away_season_points = season_points, away_score_for = score_for, away_score_against = score_against, away_percentage = percentage, away_ladder_pos = ladder_pos) %>% 
              mutate(round = as.character(round)), 
            by = c('team2' = 'Team', 'season', 'round'))



# if there's NAs in the following colums (round 1 games), fill with a 1
cols <- c("home_season_points", "home_score_for", "home_score_against", "home_percentage", "home_ladder_pos", "away_season_points", "away_score_for", "away_score_against", "away_percentage","away_ladder_pos")

afl_premiership_season[, cols][is.na(afl_premiership_season[, cols])] <- 1

# create a feature to determine if teams are in the top eight to use in model
afl_premiership_season <- afl_premiership_season %>% 
  mutate(teams_in_eight = ifelse(home_ladder_pos <= 8 & away_ladder_pos <= 8, "both",
                                 ifelse(home_ladder_pos <= 8 & away_ladder_pos >= 8, "home_only",
                                        ifelse(home_ladder_pos >= 8 & away_ladder_pos <= 8, "away_only", "neither"))))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to create a DF to calculate the last three games record for each team
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_last_three_wins <- function(afl_df) {
  # create a long df, with each observation being a team, for the round, for the season
  team_view <- afl_premiership_season %>% 
    select(Team = team1, round, season, winner) %>% 
    mutate(round = as.numeric(round), home_or_away = "Home") %>%
    bind_rows(afl_premiership_season %>% 
                select(Team = team2, round, season, winner) %>% 
                mutate(round = as.numeric(round), home_or_away = "Away"))  %>%
    mutate(win = ifelse(winner == "Draw", 0.5, ifelse(winner == home_or_away, 1, 0)))
  
  
  
  # because there were byes throughout, some teams are missing for ladder construction purposes
  # ie in some rounds, there aren't the right amount of teams in each round
  df <- team_view %>%
    distinct(season, Team) %>% 
    left_join(team_view %>% 
                distinct(season, round), by = "season") %>% 
    left_join(team_view, by = c("season", "round", "Team")) %>% 
    select(-winner, -home_or_away)
  
  
  # fill in the missing values with zeros
  df <- df %>% 
    arrange(season, Team, as.numeric(round)) %>% 
    group_by(season, Team) %>% 
    mutate(win = ifelse(is.na(win), lag(win), win)) %>% 
    # adelaide v geelong game cancelled for tragic circumstances. Each team was awarded the draw
    mutate(win = ifelse(season == 2015 & round == 14 & Team %in% c("Geelong", "Adelaide"), 0.5, win)) %>% 
    # Gold Coast's first possible game was a bye
    mutate(win = ifelse(is.na(win), 0, win)) %>% 
    mutate(wins_last_three = lag(win, 1) + lag(win, 2) + lag(win, 3)) %>% 
    mutate(wins_last_three = ifelse(round == 1, 0, ifelse(round == 2, 0 + lag(win, 1), ifelse(round == 3, 0 + lag(win, 1) + lag(win, 2), lag(win, 1) + lag(win, 2) + lag(win, 3))))) %>% 
    select(-win) %>% 
    mutate(round = as.character(round)) %>% ungroup()
  
}

# create the DF
last_three <- create_last_three_wins(afl_premiership_season)

# join the data for both home and away teams
afl_premiership_season <- afl_premiership_season %>% 
  left_join(last_three, by = c('team1' = 'Team', 'season', 'round')) %>% 
  rename(home_wins_last_three = wins_last_three) %>% 
  left_join(last_three, by = c('team2' = 'Team', 'season', 'round')) %>% 
  rename(away_wins_last_three = wins_last_three)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Feature for grouped stadiums --------------------------------------------
# perfect multi-colinearity occurring on some stadiums, so the main stadiums will be kept in the venue variable, while the others classed as "Other"
legit_venues <- c("Docklands", "M.C.G.", "Gabba", "S.C.G.", "Kardinia Park", "Adelaide Oval", "Carrara", "Sydney Showground", "Perth Stadium")

afl_premiership_season <- afl_premiership_season %>%
  mutate(venue = ifelse(venue %in% legit_venues, venue, "Other"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save Data For Modelling -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(afl_premiership_season, "data/cleaned_data/afl_model_data.csv", row.names = F)

