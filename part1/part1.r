library(tidyr)
library(dplyr)
library(stringr)

#get files 
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1987:2016)

urls <- str_c(url1, years, url2, sep = "")
print(urls)

filenames <- str_c("mr", years, sep = "")
print(filenames)

N <- length(urls)
print(N)


#shortend loop for testing purposes 
#assign data to filenames 
#for (i in 1:N){
for (i in 1:2){
  print(urls[i])
  suppressMessages(
    assign(filenames[i], read.table(urls[i]))
  )
  
  file <- get(filenames[i])
  View(file)
  
  
  # only keep observations at noon 
  file = filter(file, V4 == 12 | V4 == "hh")
  
  # put '19' in front of 2 digit years
  # remove other observations 
  file = file %>% mutate(
    V16 = NULL, V15 = NULL, V12 = NULL, V11 = NULL, V10 = NULL, 
    V9 = NULL, V8 = NULL,V7 = NULL, V6 = NULL, V5 = NULL,
    V1 = 1900 + as.numeric(as.character(V1)))
  file 
  
  # check that all columns are included
  # etc etc etc
  
  if(i == 1){
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  }
}

View(MR)
View(MR$V13)

#not correct yet 
dataTimeSeries <- ts(MR$V13, start = c(1987,1),frequency = 30)
plot.ts(dataTimeSeries)



