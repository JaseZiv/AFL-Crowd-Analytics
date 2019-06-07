library(rvest)
library(httr)
library(dplyr)



first_part_url <- "https://afltables.com/afl/seas/"

seasons <- 2000:2019

all_data <- data.frame(team1=as.character(), team2=as.character(), t1_qtr_scores=as.character(), t2_qtr_scores=as.character(), t1_score=as.numeric(), t2_score=as.numeric(), match_facts=as.character())

for(i in seasons) {
  
  print(paste0("Scraping Season: ", i, " of ", max(seasons)))
  
  page <- paste0(first_part_url, i, ".html") %>% read_html()
  
  team1 <- html_nodes(page, "td tr:nth-child(1) a:nth-child(1)") %>% html_text()   
  team2 <- html_nodes(page, "td tr+ tr a:nth-child(1)") %>% html_text()
  
  t1_qtr_scores <- html_nodes(page, "td tr:nth-child(1) tt") %>% html_text()
  t2_qtr_scores <- html_nodes(page, "tr+ tr tt") %>% html_text()
  
  t1_score <- html_nodes(page, "td tr:nth-child(1) td:nth-child(3)") %>% html_text()
  t2_score <- html_nodes(page, "td:nth-child(1) tr+ tr td:nth-child(3)") %>% html_text()
  
  match_facts <- html_nodes(page, "td tr:nth-child(1) td:nth-child(4)") %>% html_text()
  
  
  each_season <- cbind(team1, team2, t1_qtr_scores, t2_qtr_scores, t1_score, t2_score, match_facts) 
  
  all_data <- rbind(all_data, each_season)
  
  Sys.sleep(5)
  
}

saveRDS(all_data, "afl_games.rds")



