#NBA Portland Trail Blazers, modeling Salary and Score Difference using Linear Regression 
#seasons 1999 - 2000, 2003 - 2004 and 2005 - 2006

# R packages used
library(ggplot2)


## Data Cleaning Portland Season 1999-00

setwd("~/Desktop/Sports Analytics/NBA-Player-Profile-analysis")

##reading three seasons gamelog data including
##a new column called HomeAway describing
##if the match is home or away game for Portland Trail Blazers
portland_9900=read.csv('Portland_gamelog_1999_2000.csv')
portland_9900$HomeAway<- ifelse(portland_9900$Sep=='@','Away','Home')

portland_0304=read.csv('Portland_gamelog_2003_2004.csv')
portland_0304$HomeAway<- ifelse(portland_0304$Sep=='@','Away','Home')

portland_0506=read.csv('Portland_gamelog_2005_2006.csv')
portland_0506$HomeAway<- ifelse(portland_0506$Sep=='@','Away','Home')


### Total Team Salaries in season 1999-2000

##this season considers 29 teams each team with their unique tag is listed below
team_tags_9900<-c('MIA','NYK','PHI','ORL','BOS','NJN',
                  'WAS','IND','CHH','TOR','DET','MIL',
                  'CLE','ATL','CHI','UTA','SAS','MIN',
                  'DAL','DEN','HOU','VAN','LAL','POR',
                  'SEA','SAC','GSW','LAC','PHO')
team_total_salary_9900=data.frame('Team'=team_tags_9900,'Salary'=NA)

setwd("~/Desktop/Sports Analytics/NBA-Player-Profile-analysis/salary_9900")
#creating a loop mechanism to compute total salary for each team
for (ind in 1:length(team_tags_9900)){
  
  df<-read.csv(paste0(team_tags_9900[ind],'_9900_salary.csv'))
  team_total_salary_9900[ind,2]=  sum(df[,2])
}

setwd("~/Desktop/Sports Analytics/NBA-Player-Profile-analysis")



### Total Team Salaries in season 2003-2004
## the season has 29 teams playing for NBA
team_tags_0304<-c('ATL','BOS','CHI','CLE','DAL','DEN',
                  'DET','GSW','HOU','IND','LAC','LAL',
                  'MEM','MIA','MIL','MIN','NJN','NOH',
                  'NYK','ORL','PHI','PHO','POR','SAC',
                  'SAS','SEA','TOR','UTA','WAS')
team_total_salary_0304=data.frame('Team'=team_tags_0304,'Salary'=NA)

setwd("~/Desktop/Sports Analytics/NBA-Player-Profile-analysis/salary_0304")

#creating a loop mechanism to compute total salary for each team
for (ind in 1:length(team_tags_0304)){
  
  df<-read.csv(paste0(team_tags_0304[ind],'_0304_salary.csv'))
  team_total_salary_0304[ind,2]=  sum(df[,2])
}
setwd("~/Desktop/Sports Analytics/NBA-Player-Profile-analysis")




### Total Team Salaries in season 2005-2006
team_tags_0506<-c('ATL','BOS','CHA','CHI','CLE','DAL',
                  'DEN','DET','GSW','HOU','IND','LAC',
                  'LAL','MEM','MIA','MIL','MIN','NJN',
                  'NOK','NYK','ORL','PHI','PHO','POR',
                  'SAC','SAS','SEA','TOR','UTA','WAS')
team_total_salary_0506=data.frame('Team'=team_tags_0506,'Salary'=NA)
setwd("~/Desktop/Sports Analytics/NBA-Player-Profile-analysis/salary_0506")

#creating a loop mechanism to compute total salary for each team
for (ind in 1:length(team_tags_0506)){
  
  df<-read.csv(paste0(team_tags_0506[ind],'_0506_salary.csv'))
  team_total_salary_0506[ind,2]=  sum(df[,2])
}
setwd("~/Desktop/Sports Analytics/NBA-Player-Profile-analysis")

###########################################################################################################

#creating a function that takes gamelog and 
#total salary (for each team) and season (string for graphing purpose)
#it return two ggplot object returned as a list 
#that plots linear regression line for Score Difference 
#and Logarithmic salary difference

plot_regression<-function(gamelog,salary,season){
  
  season_gamelog=gamelog
  team_total_salary=salary
  #creating a new variable ScorePM which 
  #indicates difference in score between Portland and Opponent team
  season_gamelog$ScorePM=season_gamelog$TmScore-season_gamelog$OppScore
  
  #creating new variable which is log of ratio of 
  #total salary of Portland and Opponent team
  season_gamelog$log_diff_salary=NA
  for (ind in 1:dim(season_gamelog)[1]) {
    
    #log(Portland_total_salary/ OpponentTeam_total_salary)
    # = log(Portland_total_salary)- log(OpponentTeam_total_salary)
    
    #index of Portland in salary dataframe
    Portland_Index_salary=which(team_total_salary$Team=='POR')
    
    #index of Opposite team in salary dataframe
    Opponent_Index_salary=which(team_total_salary$Team==season_gamelog$Opp[ind])
    
    #calculating logarithmic difference between team salaries
    log_diff_salary=log(team_total_salary$Salary[Portland_Index_salary])-
                log(team_total_salary$Salary[Opponent_Index_salary] ) 
      
    #assigning the logarithmic salary difference to column log_diff_salary
    season_gamelog[ind,c('log_diff_salary')]= log_diff_salary
    
    
  }
  #identifying matches which are home or
  #away and storing their indices for later use
  home_inds=which(season_gamelog$HomeAway=='Home')
  away_inds=which(season_gamelog$HomeAway=='Away')
  
  #using linear regression function lm(), 
  #using log of salary difference as covariate and ScorePM predicator
  
  #linear regression fit for home games
  fit_home<-lm(formula = 
                 ScorePM~log_diff_salary,season_gamelog[home_inds,])
  #ScorePM predictions for home games
  prd_home<-predict(fit_home)
  #ScorePM predictions for away games
  fit_away<-lm(formula = 
                 ScorePM~log_diff_salary,season_gamelog[away_inds,])
  prd_away<-predict(fit_away)
  
  #using ggplot to visualise scatter plot of 
  #logarithmic salary difference and score difference
  #plotting linear regression line to visualise 
  #overall trend of score difference with change in ratio.
  
  #first plot focuses on home games, using 
  #subset function to select only home games 
  #for scatter plot and regression line
  p1<-ggplot(subset(season_gamelog, HomeAway %in% 'Home' ),
             aes(x=log_diff_salary,y=ScorePM))+geom_point()+
    geom_line(aes(log_diff_salary,y=prd_home),colour='red',size=1)+
    xlab('logarithmic difference in salary for home games')+
    ylab('Score Difference')+
    ggtitle(paste0('Portland Trail Blazers Season ',season,' home games'))


  #second plot focuses on away games, using subset 
  #function to select only home games
  #for scatter plot and regression line
  p2<-ggplot(subset(season_gamelog, HomeAway %in% 'Away' ),
             aes(x=log_diff_salary,y=ScorePM,label=Opp))+
    geom_point()+
    geom_text(hjust=0,vjust=1,aes(size=1))+
    geom_line(aes(log_diff_salary,y=prd_away),colour='darkblue',size=1)+ 
    xlab('logarithmic difference in salary for away games')+
    ylab('Score Difference')+
    #using season variable for plotting 
    #season name (in a rather naive fashion!!)
    ggtitle(paste0('Portland Trail Blazers Season ',season,' away games'))

  #assigning plots to a list variable for further analysis
  plots_home_away<-list(p1,p2)
  return(plots_home_away)
  
}

plot_regression(portland_9900,team_total_salary_9900,'1999 - 2000')
plot_regression(portland_0304,team_total_salary_0304,'2003 - 2004')
plot_regression(portland_0506,team_total_salary_0506,'2005 - 2006')
