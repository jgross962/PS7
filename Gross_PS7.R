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

fix(crimes)
## 2.)

## I honestly have no idea how you could clean this data only using only dplyr functions. I am using tidyr package instead
# Only want crimes which actually happened in march -- take out data from other months
crimes = filter(crimes,substring(DateOccur,1,1)=="3")
# Sort crimes into broader categories
crimes = select(crimes, Description) %>%
  mutate()

#Compute Number of crimes per day? Which Crime Happened Most
mtcars %>%
  mutate(mpg=replace(mpg, cyl==4, NA)) %>%
  as.data.frame()


cr
crimes %>%
  mutate(Description = contains("Assault", ignore.case = TRUE), "Assault")


# Add column for just day (delete time of day)
#crimes = mutate(crimes, JustDate = select(crimes,num_range("3",1:5)) )
by.day = crimes %>%
  group_by(substring(DateOccur,1,9)) %>%
  summarise(count=n()
  )
by.day
