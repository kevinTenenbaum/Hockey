source('~/R/Hockey/Scraper/CreateDBConnection.R')
library(tidyverse)
library(lme4)

season <- '20182019'


#### Download Data ####
con <- dbCon("Hockey")
# dbGetQuery(con, "select emptyNet, count(*) from Events e where e.event = 'Goal' group by emptyNet")

# dbGetQuery(con, "select * from Schedule limit 5")

events <- dbGetQuery(con, paste0("select e.event
                                 , e.gamePk
                                 , e.eventNum
                                 , e.description
                                 , e.secondaryType
                                 , e.period
                                 , e.periodType
                                 , e.emptyNet
                                 , e.timeSecRemaining
                                 , e.skatersHome
                                 , e.skatersAway
                                 , e.coordx as X
                                 , e.coordy as Y
                                 , teamType
                                 , teamid
                                 , e.teamname as TeamName
                                 , penaltyMinutes
                                 , penaltySeverity
                                 , emptyNet1
                                 , timeSecRemaining
                                 , scorehome
                                 , scoreaway
                                 , ordinalnum as Period
                                 , season
                                 , s.gameDate
                                 , s.homeid
                                 , s.homename
                                 , s.awayid
                                 , s.awayname
                                 from Events e
                                 inner join Schedule s
                                 on e.gamePk = s.gamePk
                                 where e.event in ('Shot','Missed Shot','Goal')
                                 and s.season = ", season))


# schedule <- dbGetQuery(con, "select s.homename, s.awayname, s.gamepk, s.season, s.gameType from Schedule s where s.Season = 20182019 and s.gameType = 'R'")

players <- dbGetQuery(con, paste0("select eventNum, p.gamePk, playerid, playerfullName, playerType
                                  from Players p 
                                  inner join Schedule s 
                                  on s.gamePk = p.gamePk
                                  where playerType in ('Scorer','Shooter', 'Goalie') and s.season = ", season))

teams <- dbGetQuery(con, "select * from Teams")

rosters <- dbGetQuery(con, "select * from PlayerInfo")

dbDisconnect(con)



#### Join Data Sources  ####

 shots <- events %>% mutate(gameDate = as.Date(substring(gameDate, 1, 10))) %>% 
  # left_join(schedule %>% select(gamepk, season), by = c('gamePk' = 'gamepk')) %>%
  inner_join(players %>% filter(playerType %in% c('Scorer','Shooter')), by = c('gamePk','eventNum')) %>% 
  # left_join(players %>% filter(playerType == 'Goalie') %>% rename(goalieid = playerid, goalieName = playerfullName), by = c('gamePk','eventNum')) %>%  
  # inner_join(teams %>% select(id, ), by = c('teamid' = 'id')) %>%
  left_join(rosters %>% mutate(personid = as.integer(id)), by = c('playerid' = 'personid'))  %>% 
  mutate(opponentID = ifelse(teamType == 'home', awayid, homeid),
         opponentName = ifelse(teamType == 'away', awayname, homename))


#### Aggregate data by game ####
teamGame <- shots %>% group_by(teamid, TeamName, gamePk, gameDate, opponentID, opponentName) %>%
  summarise(shots = n())



#### Fit Models #### 
## Fit model to predict mean of shot distribution 
# TODO: Check to make sure we aren't missing out on team interactions here.
# TODO: Compare to a ridge regression model on out of sample performance
# TODO: Add in whether team is home or away as fixed effect.
fit <- lmer(shots ~  (1 | TeamName) + (1 | opponentName), data = teamGame)

teamGame$predShots <- predict(fit)
teamGame <- teamGame %>% mutate(resid = (shots - predShots)^2)

## Fit model to predict variance based on residuals from mean model
fitVar <- lmer(resid ~  (1 | TeamName) + (1 | opponentName), data = teamGame )
summary(fitVar)


#### Predict from Models #### 
# team <- 'Washington Capitals'
# opponent <- 'Tampa Bay Lightning'

# Function to simulate from normal distribution of predicted shots in a game.
simGames <- function(team, opponent, n = 500){
  mu <- predict(fit, newdata = data.frame(TeamName = team, opponentName = opponent))
  sig2 <- predict(fitVar, newdata = data.frame(TeamName = team, opponentName = opponent))
  
  rnorm(n, mean  = mu, sd = sqrt(sig2))
}

checkDist <- function(team, opponent){
  mu <- predict(fit, newdata = data.frame(TeamName = team, opponentName = opponent))
  sig2 <- predict(fitVar, newdata = data.frame(TeamName = team, opponentName = opponent))
  dens <- density(unlist(teamGame[teamGame$TeamName == team, 'shots']))
  
  plot(function(x) dnorm(x, mean  = mu, sd = sqrt(sig2)), xlim = c(0, 80))
  lines(x = dens$x, y = dens$y, add = T, col = 'red')
}



# simGames('Washington Capitals', 'Columbus Blue Jackets')


