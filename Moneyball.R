#Libraries
library(ggplot2)
library(dplyr)

#Load data
batting <- read.csv('Batting.csv')
sal <- read.csv("Salaries.csv")
head(batting)

batting$BA <- batting$H / batting$AB
tail(batting$BA,5)

#Onbase percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP) / 
  (batting$AB + batting$BB + batting$HBP + batting$SF)

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

#Slugging average
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B)
                + (4 * batting$HR) ) / batting$AB

#Filter to data after 1985
batting <- subset(batting, yearID >= 1985)

#Join & batting stats & salaries
combo <- merge(batting, sal, by = c('playerID', 'yearID'))  



lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players <- subset(lost_players, yearID == 2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]


#Plot
ggplot(combo, aes(x = OBP, y = salary)) +
  geom_point(size = 2)

#Isolate cheap players with OBP
combo <- subset(combo, salary < 8000000 & OBP > 0)
combo <- subset(combo, AB >= 450)
arrange(combo, desc(OBP))

#Proposal
options <- head(arrange(combo, desc(OBP)),10)
options[,c('playerID','AB','salary','OBP')]
