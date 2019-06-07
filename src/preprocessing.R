library(rvest)
library(httr)
library(dplyr)
library(stringr)


all_data <- readRDS("data/afl_games.rds")


glimpse(all_data)


all_data <- all_data %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(t1_score = as.numeric(t1_score),
         t2_score = as.numeric(t2_score),
         t1_qtr_scores = str_squish(t1_qtr_scores),
         t2_qtr_scores = str_squish(t2_qtr_scores))
