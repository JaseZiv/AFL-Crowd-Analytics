
# Function to scrape afltables.com ----------------------------------------

# The function takes two parameters - the first _eason you want to scrape, and the last_season you want to scrape to.
# If you only want the one season, use the same year number for both arguments.

afl_scraper <- function(first_season, last_season) {
  
  library(rvest)
  library(httr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  
  
  first_part_url <- "https://afltables.com/afl/seas/"
  
  seasons <- first_season:last_season
  
  all_data <- data.frame(team1=as.character(), team2=as.character(), t1_q1=as.character(), t2_q1=as.character(), t1_q2=as.character(), t2_q2=as.character(),
                         t1_q3=as.character(), t2_q3=as.character(), t1_q4=as.character(), t2_q4=as.character(), team1_score=as.numeric(), team2_score=as.numeric(),
                         match_facts=as.character(), umpires=as.character())
  
  for(i in 1:length(seasons)) {
    
    print(paste0("Scraping Season: ", seasons[i], " of ", max(seasons)))
    
    page <- paste0(first_part_url, seasons[i], ".html") %>% read_html()
    
    all_detail_pages <- html_nodes(page, "tr+ tr b+ a") %>% html_attr('href') %>% str_remove("../")
    
    season_data <- data.frame(team1=as.character(), team2=as.character(), t1_q1=as.character(), t2_q1=as.character(), t1_q2=as.character(), t2_q2=as.character(),
                              t1_q3=as.character(), t2_q3=as.character(), t1_q4=as.character(), t2_q4=as.character(), team1_score=as.numeric(), team2_score=as.numeric(),
                              match_facts=as.character(), umpires=as.character())
    
    for(j in 1:length(all_detail_pages)) {
      
      print(paste0("Scraping Game: ", j, " of ", length(all_detail_pages)))
      
      each_detail_page <- read_html(paste0("https://afltables.com/afl/", all_detail_pages[j]))
      
      team1 <- html_node(each_detail_page, "br+ table tr:nth-child(2) td:nth-child(1)") %>% html_text()
      t1_q1<- html_node(each_detail_page, "br+ table tr:nth-child(2) td:nth-child(2)") %>% html_text()
      t1_q2 <- html_node(each_detail_page, "br+ table tr:nth-child(2) td:nth-child(3)") %>% html_text()
      t1_q3 <- html_node(each_detail_page, "br+ table tr:nth-child(2) td:nth-child(4)") %>% html_text()
      t1_q4 <- html_node(each_detail_page, "br+ table tr:nth-child(2) td:nth-child(5)") %>% html_text()
      team1_score <- sub('.*\\.', '', t1_q4) %>% as.numeric()
      
      team2 <- html_node(each_detail_page, "br+ table tr:nth-child(3) td:nth-child(1)") %>% html_text()
      t2_q1<- html_node(each_detail_page, "br+ table tr:nth-child(3) td:nth-child(2)") %>% html_text()
      t2_q2 <- html_node(each_detail_page, "br+ table tr:nth-child(3) td:nth-child(3)") %>% html_text()
      t2_q3 <- html_node(each_detail_page, "br+ table tr:nth-child(3) td:nth-child(4)") %>% html_text()
      t2_q4 <- html_node(each_detail_page, "br+ table tr:nth-child(3) td:nth-child(5)") %>% html_text()
      team2_score <- sub('.*\\.', '', t2_q4) %>% as.numeric()
      
      match_facts <- html_node(each_detail_page, "br+ table tr:nth-child(1) td:nth-child(2)") %>% html_text()
      
      umpires <- html_node(each_detail_page, "br+ table tr:nth-child(6) td+ td") %>% html_text()
      
      each_game <- cbind(team1, team2, t1_q1, t2_q1, t1_q2, t2_q2, t1_q3, t2_q3, t1_q4, t2_q4, team1_score, team2_score, match_facts, umpires)
      
      season_data <- rbind(season_data, each_game)
      
      Sys.sleep(5)
      
    }
    
    
    all_data <- rbind(all_data, season_data)
    
  }
  
  return(all_data)
  
}