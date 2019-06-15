library(rvest)
library(tidyverse)

ws_daily_temp_melb <- 086038

scrape_years <- 2011:2012


p1_url <- "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=122&p_display_type=dailyDataFile&p_startYear="
p2_url <- "&p_c=-1480549299&p_stn_num=086038"

page <- read_html("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=122&p_display_type=dailyDataFile&p_startYear=2012&p_c=-1480549299&p_stn_num=086038")


t_head <- html_node(page, "#dataTable") %>% html_nodes("thead th") %>% html_text()
t_body <- html_nodes(page, "#dataTable") %>% html_table()


temp_temp <- bind_rows(t_body)

colnames(temp_temp) <- c("Day", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

temp_temp$Year <- scrape_year

temp_temp <- temp_temp %>% 
  filter(Day != "Graph", !str_detect(Day, "daily"), !str_detect(Day, "Monthly"))



temp_temp <- temp_temp %>% gather(key = "Month", value = "Temp_C", -Year, -Day) %>% filter(!is.na(Temp_C))


         