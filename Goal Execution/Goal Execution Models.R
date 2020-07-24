library(DBI)
library(RMariaDB)
library(tidyverse)
library(lme4)
library(lubridate)

season1 <- '20172018'
season2 <- '20182019'
source('~/R/Hockey/Scraper/CreateDBConnection.R')
### Pull in data
con <- dbCon("Hockey")


shots <- dbGetQuery(con, paste0("select g.*, s.season, s.gameDate
                    from xGoal g
                    inner join Schedule s
                    on g.gamePk = s.gamePk
                    where s.season between ", season1 ," and ", season2))
  

players <- dbGetQuery(con, "select id as playerid, fullName from PlayerInfo")

dbDisconnect(con)

shotsModel <- function(shots){
  ### Examine Achivement by Player 
  reFit <- glmer(goal ~ xGrf + (1 | playerid) + (1 | goalieid), data = shots, family = binomial(link = 'logit'))
  summary(reFit)
  
  
  ref <- ranef(reFit)
  colnames(ref$playerid) <- 'playerCoef'
  colnames(ref$goalieid) <- 'goalieCoef'
  ref$playerid$playerid <- as.numeric(rownames(ref$playerid))
  ref$goalieid$goalieid <- as.numeric(rownames(ref$goalieid))
  
  
  shots <- shots %>% inner_join(ref$goalieid, by = 'goalieid') 
  shots <- shots %>% inner_join(ref$playerid, by = 'playerid')
  
  shots$baseline <- predict(reFit, newdata = shots)
  shots <- shots %>% mutate(
    baselineProb = plogis(baseline),
    noGoalie = plogis(baseline - goalieCoef),
    noShooter = plogis(baseline - playerCoef),
    GoalieAdd = baselineProb - noGoalie,
    ShooterAdd = baselineProb - noShooter) 
  
  
  
  aggPerformance <- shots %>% inner_join(players, by = c('playerid')) %>% 
    group_by(playerid, fullName, season) %>% 
    summarise(shots = n(),
              G = sum(goal),
              xG = sum(xG),
              GAdd = sum(ShooterAdd),
              GAddperShot = GAdd/shots) %>% arrange(desc(xG))
  
  
  aggPerformanceGoalie <- shots %>% inner_join(players, by = c('goalieid' = 'playerid')) %>% 
    group_by(goalieid, fullName, season) %>% 
    summarise(shots = n(),
              G = sum(goal),
              xG = sum(xG),
              GSaved = -sum(GoalieAdd),
              GSaveperShot = GSaved/shots) %>% arrange(desc(xG))
  
  return(list(aggPerformanceGoalie = aggPerformanceGoalie, aggPerformance = aggPerformance))
}



noWeight <- shotsModel(shots %>% filter(season == season2))

### Weight by Time ## 


shots <- shots %>% mutate(daysAgo = as.numeric(Sys.Date() - as.Date(substring(shots$gameDate, 1, 10))),
                          weight = 5/daysAgo)


withWeight <- shotsModel(shots)







## Write output to database
con <- dbCon('Hockey')

# write seasonal unweighted 
dbExecute(con, paste("delete from GoalieSavesAdded where season =", season))
dbExecute(con, paste("delete from ShooterGoalsAdded where season =", season))

dbWriteTable(con, "GoalieSavesAdded", noWeight$aggPerformanceGoalie, append = TRUE)
dbWriteTable(con, "ShooterGoalsAdded", noWeight$aggPerformance, append = TRUE)

# Write Buest Guess
dbExecute(con, paste("truncate table GoalieSavesAdded_BuestGuess"))
dbExecute(con, paste("truncate table ShooterGoalsAdded_BuestGuess"))

dbWriteTable(con, "GoalieSavesAdded_BuestGuess", withWeight$aggPerformanceGoalie, append = FALSE)
dbWriteTable(con, "ShooterGoalsAdded_BuestGuess", withWeight$aggPerformance, append = FALSE)


dbDisconnect(con)
