#Christine Hoang
#Michael is a weird pet name to me.

install.packages("here")
library("here")

install.packages("skimr")
library("skimr")

install.packages("dplyr")
library("dplyr")

avocado<-read_csv("avocado.csv")
avocado

glimpse(avocado)
head(avocado)

avocado %>%
  rename(Year=year, Type = type, Region=region)


#4.	Add a column named SmallPercent that contains the percentage of small bags out of the number of total bags.
avocado<-(SmallPercent)


avocado_trimmed<-avocado%>%
  select(Date, SmallBags, TotalBags)
avocado_trimmed













