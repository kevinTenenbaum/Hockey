library(dplyr)
library(ggplot2)
library(lme4)
source('~/R/Hockey/Scraper/CreateDBConnection.R')

season <- 20182019


con <- dbCon("Hockey")


shifts <- dbGetQuery(con, paste0("select * from Shifts e 
                       inner join Schedule s
                       on s.gamePk = e.gamePk
                       where s.season = ", season, "
                       group by "))

shots <- dbGetQuery(con, paste0("select g.*, s.season, s.awayid, s.awayname, s.homeid, s.homename
                    from xGoal g
                    inner join Schedule s
                    on g.gamePk = s.gamePk
                    where s.season = ", season))


players <- dbGetQuery(con, "select id as playerid, fullName from PlayerInfo")

dbDisconnect(con)

shifts <- shifts %>% mutate(DurationSec = as.numeric(substring(Duration, 1,2))*60 + as.numeric(substring(Duration, 4,5))) 



shots <- shots %>% mutate(opponentID = ifelse(teamType == 'home', awayid, homeid),
                 opponentName = ifelse(teamType == 'away', awayname, homename))





seasonSum <- shots %>% group_by(season, playerid) %>% 
  summarise(shots = n(),
            xGperShot = mean(xG),
            xGperShotrf = mean(xGrf),
            G = sum(goal)) %>% 
  inner_join(players, by = 'playerid') %>% filter(shots >= 20) 




ggplot(seasonSum %>% filter(shots  >= 50), aes(x = xGperShotrf, y = G)) + geom_point() + geom_smooth()

