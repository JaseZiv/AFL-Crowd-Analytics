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

weeknight_games <- c("Mon", "Tue", "Wed")

# join the day of week the game is played and the time period it's played in
all_data_cleaned <- all_data_cleaned %>% 
  mutate(game_time = paste(weekday, time_period, sep = ' ')) %>%
  mutate(game_time = ifelse(weekday %in% weeknight_games & time_period %in% c("Evening", "Night"), "Weeknight", game_time))



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



# Venues ------------------------------------------------------------------

legitimate_venues <- c("M.C.G.", "Docklands", "Subiaco", "Football Park", "Gabba", "S.C.G.", "Kardinia Park", "Adelaide Oval", "Carrara", "York Park", "Sydney Showground", 
                       "Stadium Australia", "Manuka Oval", "Perth Stadium", "Bellerive Oval")


all_data_cleaned <- all_data_cleaned %>% mutate(venue = ifelse(venue %in% legitimate_venues, venue, "Other"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save Data For Analysis --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(all_data_cleaned, "data/cleaned_data/afl_preprocessed.csv", row.names = F)












