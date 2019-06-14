library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)


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
  separate(date, into = c(NA, "date"), sep = ", ")
# split out the date variable to separate variables for date and time
all_data_cleaned <- all_data_cleaned %>% 
  separate(date, into = c("date", "start_time"), sep = " ")

all_data_cleaned <- all_data_cleaned %>% 
  mutate(date = ymd(as.Date(date, format="%d-%B-%Y")))



# Feature Engineering -----------------------------------------------------

# create a variable for the season
all_data_cleaned <- all_data_cleaned %>% 
  mutate(season = year(date))

# create a variable that calculates the margin, total combined score and whether the home team won (team_1)
all_data_cleaned <- all_data_cleaned %>% 
  mutate(margin = team1_score - team2_score,
         total_score = team1_score + team2_score,
         winner = ifelse(team1_score > team2_score, "Home", ifelse(team2_score > team1_score, "Away", "Draw"))) %>% 
  mutate_if(is.character, str_squish)



rain_data <- read_csv("data/cleaned_data/preprocessed_rain_data.csv")

all_data_cleaned %>% count(venue) %>% View()

VenueCity <- c("Adelaide", "Hobart", "Sydney", "Gold Coast", "Cairns", "Melbourne", "Ballarat", "Adelaide", "Brisbane", "China", "Geelong", "Melbourne", "Canberra", "Darwin", "Perth", "Melbourne", "Sydney", "Sydney", "Perth", "Sydney", "Darwin", "Perth", "NZ", "Hobart")
Venue <- all_data_cleaned %>% count(venue) %>% pull(venue)

venues_df <- cbind(Venue, VenueCity) %>% data.frame() %>% mutate_if(is.factor, as.character)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------- ANALYSIS ----------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_data_cleaned %>% glimpse()


all_data_cleaned %>% 
  count(winner) %>% 
  mutate(prop = n / sum(n))


all_data_cleaned %>% 
  mutate(margin = abs(margin)) %>% 
  ggplot(aes(x= margin)) +
  geom_density()


summary(abs(all_data_cleaned$margin))


mosaic::favstats(abs(all_data_cleaned$margin) ~ all_data_cleaned$season)


all_data_cleaned %>% 
  group_by(season) %>% 
  summarise(median_margin = median(abs(margin)),
            avg_margin = mean(abs(margin))) %>% 
  ggplot(aes(x= season)) +
  geom_line(aes(y= median_margin))



all_data_cleaned %>% 
  filter(round <= 11) %>% 
  group_by(season) %>% 
  summarise(median_attendance = median(attendance)) %>% 
  ggplot(aes(x= season)) +
  geom_line(aes(y= median_attendance))






all_data_cleaned %>% 
  filter(round <= 11) %>% 
  group_by(season) %>% 
  summarise(avg_total_score = mean(total_score)) %>% 
  ggplot(aes(x= season)) +
  geom_line(aes(y= avg_total_score))
