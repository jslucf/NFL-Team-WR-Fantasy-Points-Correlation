# Jason Laso                                   #
# 2/18/17                                      #
# Team WR PPR Analysis for Rotoviz.com Article #
################################################


library(corrplot)
library(dplyr)
library(ggplot2)

#read in team WR finishes from 2012-16
setwd("C:\\Users\\Jason\\Documents\\R\\NFL data")
teamwr = read.csv(file = "Team WR 14-16.csv", header=T, sep=",")

#read in top 100 WR list from 2016
top100 = read.csv(file = "2016top100wr.csv", header=T, sep=",")

#arrange the table by the teams in alphabetical order
teamwr = teamwr %>% arrange(team)

#create list of all teams
teamfinishes$teamid = as.vector(unique(teamwr$teamid))

#we need to get the finish from each year into one row for each team.
#create vectors to store finishes for each year
teamfinishes = c()
teamfinishes$fin16 = c()
teamfinishes$fin15 = c()
teamfinishes$fin14 = c()
teamfinishes$fin13 = c()
teamfinishes$fin12 = c()


#loop through each team
for(i in teamfinishes$teamid){
  
  #subset teamwr DF for only seasons for iterated team
  sub = teamwr %>% filter(teamwr$teamid == i) %>% select(teamid, finish, year)
  
  #rows are already in descending chronological order so iterate through the five years (as in 5 rows)
  # and append to the appropriate year's vector
  for(n in 1:5){
    if(n==1){
      teamfinishes$fin16 = append(teamfinishes$fin16, as.integer(sub[n,  2]))
    }
        
    else if(n==2){
      teamfinishes$fin15 = append(teamfinishes$fin15, sub[n,  2])
    }
    
    else if(n==3){
      teamfinishes$fin14 = append(teamfinishes$fin14, sub[n,  2])
    }
    else if(n==4){
      teamfinishes$fin13 = append(teamfinishes$fin13, sub[n,  2])
    }
    else if(n==5){
      teamfinishes$fin12 = append(teamfinishes$fin12, sub[n,  2])
    }
  }
}

#turn all of our lists into a data frame
teamfinishes = as.data.frame(teamfinishes)

#check structure to make sure conversions worked
str(teamfinishes)

#Now we can look at correlation

#color palette for corr matrix
color1 <- colorRampPalette(c("#7F0000","red","#FF7F00","white", 
                           "#007FFF", "blue","#00007F"))

#YOY corr matrix for 2012-2016 WR ranks
corrplot(cor(teamfinishes[,2:6]), method="number", type="lower", col=color1(10))

#They say in sabremetrics that when it comes to counting stats and ratios, the teams at the top almost always finish
#lower the next season and the teams at the bottom finish higher ("regression to the mean"). Let's look at that.

#subset just for teams in top 8 finish YOY (top 8 in 2012 in 2013, top 8 in '13 to '14, etc)
#create new matrix to store correlations
corr.top10=matrix(nrow=5, ncol=5)

#To make this easier, let's create a new DF to reorder the finish columns in ascending chronological order
finishesonly = teamfinishes %>% select(fin12, fin13, fin14, fin15, fin16)


for(col1 in 1:5){
  for(col2 in 1:5){
    
    #take top 8 from previous year in WR points
    top10 = finishesonly %>% filter(finishesonly[,col1]%in% 1:8)
    
    #take correlation if next year in iteration came after the test year
    if(col1 < col2) {
        corr.top10[col2, col1] = cor(top10[,col1], top10[,col2])
    
    #no need to test year backwards (don't care about 2013 top 8 in 2012)    
    } else if(col2 < col1) {
        corr.top10[col2, col1] = 0
    
    #identity rows to 1    
    } else if(col1 == col2) {
        corr.top10[col2, col1] = 1
        
    }
  }
}

#rename matrix columns and rows
colnames(corr.top10) = c("fin12", "fin13", "fin14", "fin15", "fin16")
rownames(corr.top10) = c("fin12", "fin13", "fin14", "fin15", "fin16")

#print corr matrix to see if it worked
corr.top10

#now make fancy corr matrix
corrplot(corr.top10,method="number", type="lower", col=color1(10))

# so we see that the correlations for the top 8 YOY lag well behind the entire league (2015 to 16 especially), 
# which lends validity to the argument of regression to the mean

#bottom 8 ranked teams each year to next
corr.bot10=matrix(nrow=5, ncol=5)

for(col1 in 1:5){
  for(col2 in 1:5){
    
    #subset bottom 8 teams from previous year
    bot10 = finishesonly %>% filter(finishesonly[,col1]%in% 25:32)
    
    #take correlation if iterated year came after subset year
    if(col1 < col2){
        corr.bot10[col2, col1] = cor(bot10[,col1], bot10[,col2])
        
    #throw out if subset year was after iterated year
    } else if (col1 > col2){ 
        corr.bot10[col2, col1] = 0
    
    #set identity diagonal
    } else{
        corr.bot10[col2, col1] = 1
    }
  }
}

#rename matrix columns and rows
colnames(corr.bot10) = c("fin12", "fin13", "fin14", "fin15", "fin16")
rownames(corr.bot10) = c("fin12", "fin13", "fin14", "fin15", "fin16")
corr.bot10
corrplot(corr.bot10,method="number", type="lower", col=color1(10))

#Outside of seemingly outlierish 2013 to 14, bottom teams correlation also lags behind the rest of the league



#let's now figure out the top 2 WRs on each team in the top 100 from 2016 and join it with our
#teamfinishes table into a new DF
top2wr.team = top100 %>% group_by(teamid) %>% summarize(wr1 = min(rank), wr2 = nth(rank,2)) %>% 
  inner_join(teamfinishes, by.x=teamid, by.y=teamid) 

cor(top2wr.team$wr2, top2wr.team$fin16)

write.csv(teamfinishes, "teamfinishes.csv")
