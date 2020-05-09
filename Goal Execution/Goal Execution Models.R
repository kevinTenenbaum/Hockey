library(DBI)
library(RMariaDB)
library(tidyverse)
library(lme4)

season <- '20182019'

### Pull in data
con <- dbConnect(RMariaDB::MariaDB(), user='kt1', password="KentP00kieTyler", dbname='Hockey', host='localhost')
shots <- dbGetQuery(con, paste0("select g.*, s.season
                    from xGoal g
                    inner join Schedule s
                    on g.gamePk = s.gamePk
                    where s.season = ", season))
  

players <- dbGetQuery(con, "select id as playerid, fullName from PlayerInfo")

dbDisconnect(con)


### Examine Achivement by Player 
reFit <- glmer(goal ~ xG + (1 | playerid) + (1 | goalieid), data = shots, family = binomial(link = 'logit'))
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


## TODO: write this table to database