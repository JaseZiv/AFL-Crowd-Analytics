 # The AFL betting data extracted by the below function comes from the Australia Sports Betting website (http://www.aussportsbetting.com),
# The data specifically is contained in the following xlsx file: "http://www.aussportsbetting.com/historical_data/afl.xlsx"
# The raw data contains more columns than is kept in the below function.


return_betting_data <- function() {
  
  library(readxl)
  library(tidyverse)
  
  
  # Read in data ------------------------------------------------------------
  
  url1<-"http://www.aussportsbetting.com/historical_data/afl.xlsx"
  p1f <- tempfile()
  download.file(url1, p1f, mode="wb")
  p1<- read_xlsx(path = p1f, sheet = 1)
  
  
  # Reformat column names - file read in has spurious column header - real header on row 1 of data.
  colnames(p1) <- p1[1,]
  # then delete data
  p1 <- p1[-1,]
  
  
  
  p1$Date <- as.Date(as.numeric(p1$Date), origin = "1899-12-30")
  
  
  # only keep required columns
  p1 <- p1 %>% select(
    Date,
    HomeTeam = `Home Team`,
    AwayTeam = `Away Team`,
    Finals = `Play Off Game?`,
    HomeOdds = `Home Odds`,
    AwayOdds = `Away Odds`,
    HomeOddsOpen = `Home Odds Open`,
    HomeOddsClose = `Home Odds Close`,
    AwayOddsOpen = `Away Odds Open`,
    AwayOddsClose = `Away Odds Close`,
    HomeLineOpen = `Home Line Open`,
    HomeLineClose= `Home Line Close`,
    AwayLineOpen = `Away Line Open`,
    AwayLineClose= `Away Line Close`
  )
  
  
  p1 <- p1 %>% 
    mutate_at(.vars = 5:14, as.numeric)
  
  return(p1)

}

