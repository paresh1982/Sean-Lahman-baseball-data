# Sean Lahman's Baseball Database is not just one dataset. Type help("Lahman-package") to get an 
# idea of the data tables available. The batting statistics of players are stored in one table 
# (Batting), while information about people(most of whom are players) is in a different table(Master).
# 
# 1)Confirm that Barry Bonds has the record for most home runs (762) hit in a career. 
# 
# 2)For this, list top 20 players' names with the most home runs, and confirm that Manny is in the top 20. Note that you 
# will need to join the Batting and Master tables together to display the players' name instead of the 
# player ID.
# 
# 3) Name every pitcher in baseball history who has accumulated at least 300 wins (W) and at least 
# 3,000 strikeouts (SO). Use Pitching table.
# 
# 4) Display a table with 10 most recent World Series MVP awardees. Include their names and ages.


#Solution

install.packages("Lahman")
library(Lahman)
library(dplyr)

data("Master")
data("Batting")
data("Pitching")
data("AwardsPlayers")

# Left join Batting and Master by player ID
batting_left <- left_join(Batting, Master, by = "playerID")

# Find player ID for "Barry Bonds"
Master[(Master$nameFirst == "Barry" & Master$nameLast == "Bonds"),]

#Confirming Barry Bonds Home Run hit of 762
sum(batting_left[batting_left$playerID == "bondsba01", "HR"])

#sorting the data table as per most home runs in descending order
HR_summary <- batting_left %>% group_by(playerID, nameFirst) %>% 
  summarise(HR_total = sum(HR)) %>% arrange(desc(HR_total))

#Listing TOP 20
head(HR_summary, 20)

# Yes Manny is in top 20,  at 15th position with 555 home run score


# Left join Pitching and Master by player ID
pitching_left <- left_join(Pitching, Master, by = "playerID")

#Summarising for Win Total and Strikeout total
Pitch_summary <- pitching_left %>% group_by(playerID, nameFirst) %>% 
                summarise(W_total = sum(W), SO_total = sum(SO)) %>% arrange(desc(W_total), desc(SO_total))

# Every Pitcher in the baseball history who has accumulated at least 300 wins (W) and 
#at least 3,000 strikeouts (SO)
Pitch_summary %>% filter(W_total >= 300 & SO_total >= 3000)

# Table with 10 most recent World Series MVP awardees. Include their names and ages
AwardsPlayers %>% filter(awardID == "World Series MVP") %>% arrange(desc(yearID)) %>% head(10) %>% left_join(Master, by = "playerID") %>% mutate(Age = Sys.Date()-birthDate) %>% select(playerID, nameFirst, nameLast, Age, awardID, yearID)

