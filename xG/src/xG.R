library(DBI)
library(RMariaDB)
library(tidyverse)
library(mgcv)
library(gbm)


options(stringsAsFactors = FALSE)
#TODO: Use info about last event to inform xG model
# Impute and use the secondary shot type variable to inform the model
# use GBM instead of GAM in model 
# Use score differential

con <- dbConnect(RMariaDB::MariaDB(), user='kt1', password="KentP00kieTyler", dbname='Hockey', host='localhost')

# dbGetQuery(con, "select emptyNet, count(*) from Events e where e.event = 'Goal' group by emptyNet")



events <- dbGetQuery(con, "select e.event
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
                     from Events e
                     where e.event in ('Shot','Missed Shot','Goal')")


schedule <- dbGetQuery(con, "select s.homename, s.awayname, s.gamepk, s.season from Schedule s where s.Season = 20182019")

players <- dbGetQuery(con, "select * from Players p 
                      where playerType in ('Scorer','Shooter')")

teams <- dbGetQuery(con, "select * from Teams")

rosters <- dbGetQuery(con, "select * from Rosters")


shots <- events %>% 
  inner_join(schedule %>% select(gamepk, homeName = homename, awayName = awayname, season), by = c('gamePk' = 'gamepk')) %>%
  inner_join(players, by = c('gamePk','eventNum')) %>%
  # inner_join(teams %>% select(id, ), by = c('teamid' = 'id')) %>%
  inner_join(rosters %>% mutate(personid = as.integer(personid)), by = c('playerid' = 'personid'))
  
  
  
  # 
  # 
  # shots <- dbGetQuery(con, "select e.event
  #                         , e.gamePk
  #                         , e.eventNum
  #                         , e.description
  #                         , e.secondaryType
  #                         , e.period
#                         , e.periodType
#                         , e.emptyNet
#                         , e.timeSecRemaining
#                         , e.skatersHome
#                         , e.skatersAway
#                         , e.coordx as X
#                         , e.coordy as Y
#                         , teamType
#                         , s.homename as HomeName
#                         , s.awayname as AwayName
#                         , e.teamname as TeamName
#                         , penaltyMinutes
#                         , penaltySeverity
#                         , playerid as ShooterID
#                         , playerfullName as playerFullName
#                         , r.shootsCatches
#                         , case when e.teamid = s.homeid then 'home' else 'away' end as teamType
# , case when teamType = 'home' then concat(skatersHome, '-', skatersAway)
#                                                       else concat(skatersHome, '-', skatersAway) end as Strength
#            from Events e
#            inner join Schedule s 
#            on s.gamePk = e.gamePk
#            inner join (
#               select * from Players p 
#               where playerType in ('Scorer','Shooter')
#            ) p
#            on p.gamePk = e.gamePk and p.eventNum = e.eventNum
#            inner join Teams t
#            on t.id = e.teamid
#            inner join Rosters r
#            on r.personid = p.playerid
#            where e.event in ('Shot','Missed Shot','Goal')
#            and season = 20192020
#                     limit 20")
# dbGetQuery(con, "select * from Rosters limit 10")

dbDisconnect(con)

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


xGFit <- bam(goal ~ s(X, Y, by = as.factor(shootsCatches)) + shootsCatches + as.factor(period) + Strength + teamType + s(timeSecRemaining, by = period), data = shots, family = binomial(link = 'logit'))
summary(xGFit)


shots$xG <- predict(xGFit, newdata = shots, type = 'response')




plotRink() + geom_point(data = shots %>% filter(emptyNet != 1) %>% mutate(goal = event == 'Goal'), aes(x = X, y = Y, colour = xG, alpha = .3)) +
  scale_color_gradient(low = 'blue', high = 'red') + xlim(-100, 0) + facet_wrap(~shootsCatches)
