# Jonathan Gross
# Pol Sci 5625
# PS7

# NOTE: I discussed with emily on cleaning data usign tidyr functions for tibbles

# Import Libaries
library(dplyr)
library(ggplot2)
library(tidyr)

# Import data as tbl
setwd("C:/Users/jgros/documents/GitHub/PS7")
crimes = tbl_df(read.csv("March2018.csv"))
# Check data
fix(crimes)


## Pre-2.) Clean Data
## I honestly have no idea how you could clean this data only using only dplyr functions. I am using tidyr package instead

#Split DateOccured into Date and Time into two columns
crimes = separate(crimes,DateOccur, into=c("date","time"),sep = " ")
crimes$date = as.Date(crimes$date,format='%m/%d/%Y')

# Only want crimes which actually happened in march 2018 -- take out data from other months
crimes = crimes%>%
  filter(date>=as.Date("2018-03-01")) %>%
  filter(date<=as.Date("2018-03-31"))

# Sort crimes into broader categories (just get main crime)
# Possibly still could be combined more, but much better than it was
# Note: Get some warnings, but everything works fine
crimes = separate(crimes,Description,into = c("crime","crimeDetail"),sep ="-")
crimes = separate(crimes,crime,into = c("crime"),sep =" ")
crimes = separate(crimes,crime,into = c("crime"),sep =",")
crimes = separate(crimes,crime,into = c("crime"),sep ="/")
## 2.) 

#Compute Number of crimes per day. 
by.day = crimes %>%
  group_by(crime,date) %>%
  summarise(count=n()
  )
by.day

## Which Crime Happened Most?
by.crime = crimes %>%
  group_by(crime) %>%
  summarise(count=n()
  )
by.crime