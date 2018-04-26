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
colnames(crimes)

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
  group_by(date,crime) %>%
  summarise(count=n())
by.day

## Which Crime Happened Most?
by.crime = crimes %>%
  group_by(crime) %>%
  summarise(count=n())
by.crime
arrange(by.crime,desc(count))
# Larceny is the most common crime




## 3.) 
#Compute Number of crimes per day by neighborhood. 
by.neighborhood.date = crimes %>%
  group_by(Neighborhood,date,crime) %>%
  summarise(count=n())
by.neighborhood.date

# Which neighborhood has the most crime
by.neighborhood = crimes %>%
  group_by(Neighborhood) %>%
  summarise(count=n())
by.neighborhood
arrange(by.neighborhood,desc(count))
# Neighborhood 35 has the most crime




## 4.) Compute proprtion of robbery by district
## NOTE: I am assuming robbery and burglary are separate categories of crimes, and am thus leaving them as separate crimes. 
# Likewise I am leaving other crimes such as larceny as sepearate as well, and just using crimes specifically listed as robbery

#get number crimes by district
by.district = crimes %>%
  group_by(District) %>%
  summarise(count=n() )
by.district

#get robberies by district
robbery.by.district = crimes %>%
  group_by(District,crime) %>%
  filter(crime=="ROBBERY")%>%
  summarise(count=n())
robbery.by.district

# District 0 has no crime, so remove it
by.district = by.district[-1,]
by.district

# Combine 2 columns using DPLYR's bind_cols()
proportion.robbery.district = bind_cols(robbery.by.district,by.district)%>%
  select(crime,count,count1)
proportion.robbery.district
# Compute Proportion
proportion.robbery.district = mutate(robbery.district,Proportion = count/count1 )
# See which Proportion is largest  
arrange(robbery.district,desc(Proportion))

## District 5 has the highest proportions of robberies




## 5.) Visualize Changes of All types Crime over time
# Tracking crimes by day -> use by.day tibble made in q2

crimes %>%
  group_by(date) %>%
  summarise(count=n())%>%
  ggplot(mapping = aes(date,count))+
  geom_line(mapping = aes(date, count))+
  labs(title= "Crime per day")




## 6.) 

