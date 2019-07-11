source("R/afl_scraper.R")


# scrape data between 2000 and 2018
afl_pre_19 <- afl_scraper(first_season = 2000, last_season = 2018)
# save scraped data
write.csv(afl_pre_19, "data/afl_games_pre2019.csv", row.names = F)

# scrape current season
afl2019 <- afl_scraper(first_season = 2019, last_season = 2019)
# save current season data
write.csv(afl2019, "data/afl_games_2019.csv", row.names = F)

