library(tidyverse)
library(dplyr)
library(lubridate)

# Data URL
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

# Data missing for 2013 so use nearby buoy
url3 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46070h2013.txt.gz&dir=data/historical/stdmet/"
years <- c(1987:2016)

urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("noaa", years, sep = "")
urls[27] <- url3

N <- length(urls)

for (i in 1:N){
  print(urls[i])
  suppressMessages(
    assign(filenames[i], read.table(urls[i], fill = TRUE, comment.char = '~'))
  )
  # add column headers
  file <- get(filenames[i])
  names(file) <- as.matrix(file[1,])
  file <- file[-1,]
  
  # for 2007 and up there's a second row of units
  if (file[1,1] == "#yr") {
    file <- file[-1,]
  }
  
  # Change YY to YYYY
  names(file)[1] <- 'YYYY'
  
  
  # put '19' in front of 2 digit years
  if (nchar(str_c(file[1,1])) == 2) {
    file[1] <- lapply(file[1], function(yy){str_c(19, yy, sep = "")})
  }
  
  # Converts data to numerics
  file$YYYY <- as.numeric(as.character(file$YYYY))
  file$MM <- as.numeric(as.character(file$MM))
  file$DD <- as.numeric(as.character(file$DD))
  file$hh <- as.numeric(as.character(file$hh))
  file$ATMP <- as.numeric(as.character(file$ATMP))
  file$WTMP <- as.numeric(as.character(file$WTMP))

  # one file with all observations, one with only data from noon
  big_file <- file
  file <- filter(file, hh == 12)
  
  # make the key to keep data tidy
  big_file$date = make_datetime(year = big_file$YYYY, month = big_file$MM, day = big_file$DD, hour = big_file$hh)
  file$date = make_datetime(year = file$YYYY, month = file$MM, day = file$DD, hour = file$hh)
  
  # tidy the data 
  file = file %>% select(
    date, ATMP, WTMP
  )
  big_file = big_file %>% select(
    date, ATMP, WTMP
  )
  
  # Create dataframe
  if(i == 1){
    NOAA <- file
    NOAA_2 <- big_file
  }
  
  else{
    NOAA <- rbind.data.frame(NOAA, file)
    NOAA_2 <- rbind.data.frame(NOAA_2, big_file)
  }
}

NOAA$ATMP[NOAA$ATMP == 999] <- NA
NOAA$WTMP[NOAA$WTMP == 999] <- NA
NOAA_2$ATMP[NOAA_2$ATMP == 999] <- NA
NOAA_2$WTMP[NOAA_2$WTMP == 999] <- NA

# Write to CSV file
write.csv(NOAA, "data.csv", row.names = FALSE)
write.csv(NOAA_2, "big_data.csv", row.names = FALSE)

