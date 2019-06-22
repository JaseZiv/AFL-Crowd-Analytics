library(rvest)
library(httr)
library(lubridate)
library(tidyverse)


all_data <- readRDS("data/afl_games.rds")

# Data pre-processing -----------------------------------------------------

glimpse(all_data)

# make all variables character type to make splitting and string manipulation easier
all_data <- all_data %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(team1_score = as.numeric(team1_score),
         team2_score = as.numeric(team2_score))


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

# split out the date information to its own variable, remove the time in parentheses and only keep the locat game start time 
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


VenueCity <- c("Adelaide", "Hobart", "Sydney", "Gold Coast", "Cairns", "Melbourne", "Ballarat", "Adelaide", "Brisbane", "China", "Geelong", "Melbourne", "Canberra", "Darwin", "Perth", "Melbourne", "Sydney", "Sydney", "Perth", "Sydney", "Darwin", "Perth", "NZ", "Hobart")
venue <- all_data_cleaned %>% count(venue) %>% pull(venue)

venues_df <- cbind(venue, VenueCity) %>% data.frame() %>% mutate_if(is.factor, as.character)

rm(all_data);gc()

# join to the full DF
all_data_cleaned <- all_data_cleaned %>% 
  left_join(venues_df, by = "venue")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Rain Data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rain_data <- read_csv("data/cleaned_data/preprocessed_rain_data.csv") %>% data.frame()


rain_data$location[rain_data$location == "adl"] <- "Adelaide"
rain_data$location[rain_data$location == "brs"] <- "Brisbane"
rain_data$location[rain_data$location == "gc"] <- "Gold Coast"
rain_data$location[rain_data$location == "melb"] <- "Melbourne"
rain_data$location[rain_data$location == "perth"] <- "Perth"
rain_data$location[rain_data$location == "syd"] <- "Sydney"
rain_data$location[rain_data$location == "tas"] <- "Hobart"
rain_data$location[rain_data$location == "gee"] <- "Geelong"


# select only required variables
rain_data <- rain_data %>% 
  select(weather_date, rainfall_ml = `Rainfall.amount..millimetres.`, days_rainfall_measured = `Period.over.which.rainfall.was.measured..days.`, Quality, VenueCity = location, actual_days_rain)


# Join back to main df
all_data_cleaned <- all_data_cleaned %>% 
  left_join(rain_data, by = c("date" = "weather_date", "VenueCity"))


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
                              ifelse(team1 %in% c("Hawthorn", "Geelong") & team2 %in% c("Hawthorn", "Geelong"), "Rivalry",
                                     ifelse(team1 %in% melbourne_rivals & team2 %in% melbourne_rivals, "Rivalry", "Normal")))))


# Here is some betting features engineering. 
# 1. get the absolute difference between the 
all_data_cleaned <- all_data_cleaned %>% 
  mutate(odds_diff = abs(HomeOddsOpen - AwayOddsOpen),
         rain = ifelse(actual_days_rain > 2, "Yes", "No"),
         HomeTeamFav = ifelse(HomeOddsOpen > AwayOddsOpen, "Yes", "No"))


all_data_cleaned <- all_data_cleaned %>% 
  mutate(game_time = paste(weekday, time_period, sep = ' '))


# Save Data For Analysis --------------------------------------------------

saveRDS(all_data_cleaned, "data/cleaned_data/game_weather_betting_data.rds")












