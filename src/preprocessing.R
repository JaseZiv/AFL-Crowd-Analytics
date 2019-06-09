library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)


all_data <- readRDS("data/afl_games.rds")


glimpse(all_data)


all_data <- all_data %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(t1_score = as.numeric(t1_score),
         t2_score = as.numeric(t2_score),
         t1_qtr_scores = str_squish(t1_qtr_scores),
         t2_qtr_scores = str_squish(t2_qtr_scores))



all_data_cleaned <- all_data %>% 
 separate(col = match_facts, into= c("date", "info"), sep = "Att:") 

all_data_cleaned <- all_data_cleaned %>% 
  separate(col = info, into = c("attendance", "venue"), sep = "Venue:")

all_data_cleaned <- all_data_cleaned %>% 
  separate(date, into = c("date", NA), sep = "\\(") 

# b <- b %>% 
#   separate(date, into = c("game_date", "start_time"), sep = " ")



all_data_cleaned <- all_data_cleaned %>% 
  mutate(attendance = sub(",", "", attendance) %>% as.numeric())


