library(DBI)
library(RMariaDB)
library(tidyverse)
library(mgcv)
library(gbm)
library(ranger)
library(lme4)


options(stringsAsFactors = FALSE)
#TODO: Use info about last event to inform xG model
# use GBM instead of GAM and RF

season <- '20182019'

con <- dbConnect(RMariaDB::MariaDB(), user='kt1', password="KentP00kieTyler", dbname='Hockey', host='localhost')

# dbGetQuery(con, "select emptyNet, count(*) from Events e where e.event = 'Goal' group by emptyNet")



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
                     , e.teamname as TeamName
                     , penaltyMinutes
                     , penaltySeverity
                     , emptyNet1
                     , timeSecRemaining
                     , scorehome
                     , scoreaway
                     , ordinalnum as Period
                     , season
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

shots <- events %>% 
  # left_join(schedule %>% select(gamepk, season), by = c('gamePk' = 'gamepk')) %>%
  inner_join(players %>% filter(playerType %in% c('Scorer','Shooter')), by = c('gamePk','eventNum')) %>% 
  left_join(players %>% filter(playerType == 'Goalie') %>% rename(goalieid = playerid, goalieName = playerfullName), by = c('gamePk','eventNum')) %>%  
  # inner_join(teams %>% select(id, ), by = c('teamid' = 'id')) %>%
  left_join(rosters %>% mutate(personid = as.integer(id)), by = c('playerid' = 'personid')) %>% 
  mutate(Strength = ifelse(teamType == 'home',
                           paste0(skatersHome, 'v', skatersAway),
                           paste0(skatersAway, 'v', skatersHome)))
  

teamSides <- shots %>% 
  group_by(period, TeamName, gamePk) %>%
  summarise(X = median(X),
            Y = median(Y)) %>%
  arrange(TeamName, period) %>%
  mutate(flip = ifelse(X < 0, -1, 1))

shots <- shots %>% inner_join(teamSides %>% select(period, TeamName, gamePk, flip), by = c('period','gamePk','TeamName') )
shots$X <- ifelse(shots$flip == 1, shots$X*-1, shots$X)
shots$Y <- ifelse(shots$flip == 1, shots$Y*-1, shots$Y)
shots$emptyNet <- replace(shots$emptyNet, is.na(shots$emptyNet), 0)

shots <- shots %>% filter(emptyNet == 0)
shots$goal <- ifelse(shots$event == 'Goal', 1, 0)

circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
{
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(x = center[1] + diameter / 2 * cos(tt), 
             y = center[2] + diameter / 2 * sin(tt))
}

plotRink <- function(){
  # Rink dimensions described here: https://www.usahockeyrulebook.com/page/show/1082185-rule-104-face-off-spots-and-face-off-circles
  crease <- data.frame(x = c(-89, -89, 89, 89),
                       xend = c(-84.5, -84.5, 84.5, 84.5),
                       y = c(-4, 4, -4,4),
                       yend = c(-4, 4, -4, 4))
  
  
  
  ggplot() + theme_classic() + xlim(-100, 100) + ylim(-42.5, 42.5) + coord_equal() +
    geom_vline(xintercept = 0, colour = 'red', lwd = 1) + 
    geom_vline(xintercept = c(-25, 25), colour = 'blue', lwd = 1) +
    geom_vline(xintercept = c(-89, 89), colour = 'red', lwd = 1) +
    geom_polygon(data = circleFun(diameter = 30), aes(x = x, y = y), colour = 'blue', fill = NA) + 
    geom_polygon(data = circleFun(diameter = 1), aes(x = x, y = y), colour = 'red', fill = 'red') + 
    geom_segment(data = crease, aes(x = x, y= y, xend = xend, yend = yend)) + 
    geom_polygon(data = circleFun(center = c(-89, 0), diameter = 12, start = 1.75, end = 2.25), aes(x = x, y = y), colour = 'red', fill = NA) + 
    geom_polygon(data = circleFun(center = c(89, 0), diameter = 12, start = .75, end = 1.25), aes(x = x, y = y), colour = 'red', fill = NA) +
    geom_polygon(data = circleFun(center = c(-69, -22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, -22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') +
    geom_polygon(data = circleFun(center = c(-69, 22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, 22), diameter = 30), aes(x = x, y = y), fill = NA, colour = 'red') +
    geom_polygon(data = circleFun(center = c(-69, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') +
    geom_polygon(data = circleFun(center = c(-69, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(69, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') +
    
    geom_polygon(data = circleFun(center = c(-20, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(20, -22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') +
    geom_polygon(data = circleFun(center = c(-20, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') + 
    geom_polygon(data = circleFun(center = c(20, 22), diameter = 2), aes(x = x, y = y), fill = 'red', colour = 'red') 
  
}


### Impute secondary shot type ### 
shots <- shots %>% filter(!is.na(X) & !is.na(Y) & !is.na(shootsCatches))
secondaryFit <- ranger(as.factor(secondaryType) ~ Strength + teamType + X + Y + timeSecRemaining + shootsCatches, data = shots %>% filter(!is.na(secondaryType) & !is.na(X) & !is.na(Y) & !is.na(shootsCatches)))


shots$secondaryImputed[!is.na(shots$X) & !is.na(shots$Y)] <- predict(secondaryFit, data = shots[!is.na(shots$X) & !is.na(shots$Y),])$predictions %>% as.character()
shots$secondaryUse <- coalesce(shots$secondaryType, shots$secondaryImputed)

### Impute Goalie ID for shots wide of net 

# Take lead and lag as a first pass of next and previous 3 shots to see if we can impute goalieid.
shots <- shots %>% arrange(gamePk, eventNum) %>% group_by(gamePk, teamType) %>% mutate(lagGoalieId = lag(goalieid),
                                                                              leadGoalieId = lead(goalieid),
                                                                              lead2GoalieId = lead(goalieid, 2),
                                                                              lag2GoalieId = lag(goalieid, 2),
                                                                              lead3GoalieId = lead(goalieid, 3),
                                                                              lag3GoalieId = lag(goalieid, 3),
                                                                              goalieid = coalesce(goalieid, leadGoalieId, lagGoalieId, lead2GoalieId, lag2GoalieId, lead3GoalieId, lag3GoalieId)) %>% ungroup()



### Fit xG Models
xGFit <- bam(goal ~ s(X, Y, by = as.factor(shootsCatches)) + shootsCatches + as.factor(period) + Strength + teamType + s(timeSecRemaining, by = period) + secondaryUse, data = shots, family = binomial(link = 'logit'), discrete = TRUE)   
# summary(xGFit)
xGFitrf <- ranger(as.factor(goal) ~ X + Y + shootsCatches + period + Strength + teamType + timeSecRemaining + secondaryUse, data = shots, probability = TRUE)

# Predict out of xG Models
shots$xG <- predict(xGFit, newdata = shots, type = 'response')
shots$xGrf <- predict(xGFitrf, data = shots, type = 'response')$predictions[,'1']


# plotRink() + geom_point(data = shots %>% filter(emptyNet != 1 & shootsCatches == 'L') %>% mutate(goal = event == 'Goal'), aes(x = X, y = Y, colour = xG, alpha = .3)) +
#   scale_color_gradient(low = 'blue', high = 'red') + xlim(-100, 0) + facet_wrap(~secondaryUse)

### Save xG Model Outputs 
con <- dbConnect(RMariaDB::MariaDB(), user='kt1', password="KentP00kieTyler", dbname='Hockey', host='localhost')
dbExecute(con, paste0("delete from xGoal where season = ", season))
dbWriteTable(con, "xGoal", shots %>% select(gamePk, eventNum, season, event, secondaryType = secondaryUse, goalieid, emptyNet, 
                                            playerid, timeSecRemaining, period, X, Y, Strength, teamType, shootsCatches, goal, xG, xGrf), append = TRUE)
dbDisconnect(con)

