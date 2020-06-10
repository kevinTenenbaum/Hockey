---
output: 
  html_document:
    keep_md: true
---

# Data Scraping
You can scrape data from the NHL's API using the code found in the Scraper folder. You can pull data based on a start date and end date with the ability to set them as start and end seasonID's. The below code walks through the code's scraping functions and pulls it all into your local R session. 


```r
source('~/R/Hockey/Scraper/ScrapeHockeyFunctions.r')

seasonInfo <- getSeasonInfo()
if(pullFullSeasons){
  startDate <- seasonInfo %>% filter(seasonId == startDate) %>% dplyr::select(regularSeasonStartDate) %>% unlist()
  
  if(endDate == 'max'){
    endDate <- max(seasonInfo$seasonId)
  } 
  
  endDate <- seasonInfo %>% filter(seasonId == endDate) %>% dplyr::select(seasonEndDate) %>% unlist()
}


cat("Pulling roster data ...\n")
teams <- getTeamData() %>% cleanColumnNames()
teams$runID <- runID
rosters <- getAllRosters(teams) %>% cleanColumnNames()
rosters$runID <- runID
seasons <- getSeasons() %>% cleanColumnNames()
seasons$runID <- runID





cat("Pulling team schedules...\n")
schedule <- getSchedule(startDate = startDate, endDate = endDate) %>% cleanColumnNames()
playedGames <- schedule %>% filter(gameState == 'Final' & gameType != 'PR')


cat("Pulling game event data...\n")
gameEvents <- getGameEventsList(playedGames) 
events <- gameEvents$events %>% cleanColumnNames()
events$runID <- runID
players <- gameEvents$players %>% cleanColumnNames()
players$runID <- runID

cat("Pulling Player Stats...\n")
# TODO: Pull in Player splits and career stats. 
curSeason <- unique(playedGames$season)
if(length(curSeason) > 1){
  playerStats <- do.call(rbind, lapply(curSeason, function(s){
    getPlayerStatsList(season = s, players = unique(players$playerid))  
  })) %>% cleanColumnNames()
  
} else{
  playerStats <- getPlayerStatsList(season = curSeason, players = unique(players$playerid)) %>% cleanColumnNames()
}

playerStats$runID <- as.numeric(runID)

cat("Pull Player Info...")
playerInfo <- (lapply(unique(players$playerid), function(x) getPlayerInfo(x)))
playerInfo <- (bind_rows(playerInfo))
playerInfo$runID <- runID

cat("Pulling Shift Data...")
shiftFrame <- getAllShifts((playedGames$gamePk)) %>% cleanColumnNames() 
shiftFrame$runID <- runID
```


# Expected Goals Model



Once we collect the data, we can build an expected goals (xG) model that estimates the probability a shot will be a goal based on a variety of factors. 

## Included Variables
* Shot Location (X, Y)
* Shooter Handedness
* Period
* Time Remaining
* Home/Away Team
* Team  Strength: Even or power play
* Shot Type: We impute this when missing, as the NHL does not report the shot type on shots that are not on net. See below for more details on the imputation method.


## Model Structure
We fit xG models using a generalized additive model (GAM) and a random forest.

### GAM model code 
goal ~ s(X, Y, by = as.factor(shootsCatches)) + shootsCatches + 
    as.factor(period) + Strength + teamType + s(timeSecRemaining, 
    by = period) + secondaryUse

### Random Forest model code
ranger(as.factor(goal) ~ X + Y + shootsCatches + period + Strength + 
    teamType + timeSecRemaining + secondaryUse, data = shots, 
    probability = TRUE)


## Shot Type Imputation
We impute shot types so we can fill in blanks that NHL leaves out when a shot attempt is not on net. We use a random forest for this imputation with the below model specification:

ranger(as.factor(secondaryType) ~ Strength + teamType + X + Y + 
    timeSecRemaining + shootsCatches, data = shots %>% filter(!is.na(secondaryType) & 
    !is.na(X) & !is.na(Y) & !is.na(shootsCatches)))

## Results

We can make some cool plots showing the results of the each xG model using our `plotRink` function.

First we can make an empty rink plot to show what the function outputs:

```r
library(ggplot2)
source('~/R/Hockey/plot functions.R')
plotRink()
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Now that we have out rink plotting function, we can layer some interesting plots on top of the plot. This first plot just shows all of the shots in the 2018-2019 season colored by xG from the random forest model.




```r
plotRink() + geom_point(data = shots %>% filter(emptyNet != 1) %>% mutate(goal = event == 'Goal'), aes(x = X, y = Y, colour = xGrf, alpha = .3)) + scale_color_gradient(low = 'blue', high = 'red') + xlim(-100, 0)
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The next plot does the same thing but facets it out by skater handedness


```r
plotRink() + geom_point(data = shots %>% filter(emptyNet != 1) %>% mutate(goal = event == 'Goal'), aes(x = X, y = Y, colour = xGrf, alpha = .3)) + scale_color_gradient(low = 'blue', high = 'red') + xlim(-100, 0) + facet_wrap(~shootsCatches)
```

![](README_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## Next Steps
* Add an option for XGBoost in addition to our GAM and Random Forest models.
* Create a feature for the last event type and time since the last event and add it to the model
* Evaluate models using k-fold cross validation to determine which model we should use.
* Explore using an ensemble model of our various approaches

# Goal Execution Model
Using the above expected goal model, we can estimate a player's shooting ability independent of his shot selection ability. To do this, we use a logistic random effects regression model to estimate the likelihood of a goal given the xG model from above and the shooter and goalie identiites. See below for the model structure:

## Model Code



glmer(goal ~ xG + (1 | playerid) + (1 | goalieid), data = shots, family = binomial(link = 'logit'))


You can also see the list of best shooters from our model for the 2018-2019 season here:

```
## 
## ======================================================================
##    playerid      fullName       season  shots G   xG  GAdd GAddperShot
## ----------------------------------------------------------------------
## 1  8480012   Elias Pettersson  20182019  422  62 33.6 13.7    0.033   
## 2  8474564    Steven Stamkos   20182019  339  47 28.7 13.5    0.04    
## 3  8477934    Leon Draisaitl   20182019  315  51 31.2 13.2    0.042   
## 4  8476453   Nikita Kucherov   20182019  380  44 26.3  12     0.031   
## 5  8471214    Alex Ovechkin    20182019  540  52 40.9 10.9    0.02    
## 6  8479337    Alex DeBrincat   20182019  300  41 25.4 10.1    0.034   
## 7  8477956    David Pastrnak   20182019  441  46 33.4 9.3     0.021   
## 8  8474141     Patrick Kane    20182019  454  42 30.9 8.9     0.02    
## 9  8475765  Vladimir Tarasenko 20182019  480  44 33.8 8.7     0.018   
## 10 8478420    Mikko Rantanen   20182019  307  36  22  8.7     0.028   
## ----------------------------------------------------------------------
```

We can show the same top performers from our goalie goals saved here:


```
## 
## ============================================================================
##    goalieid      fullName        season  shots  G   xG   GSaved GSaveperShot
## ----------------------------------------------------------------------------
## 1  8471750      Ben Bishop      20182019 2449  117  155   19.3     0.008    
## 2  8476434      John Gibson     20182019 2613  157 194.2  17.5     0.007    
## 3  8476412   Jordan Binnington  20182019 4390  248 280.9  13.2     0.003    
## 4  8475215     Robin Lehner     20182019 2186  113 142.5  12.3     0.006    
## 5  8471695      Tuukka Rask     20182019 2756  160 177.6  10.4     0.004    
## 6  8471306     Thomas Greiss    20182019 1604  88   108   8.5      0.005    
## 7  8471418    Anton Khudobin    20182019 1698  96  111.2  8.4      0.005    
## 8  8478406  Mackenzie Blackwood 20182019 1860  112 133.1   8       0.004    
## 9  8475883   Frederik Andersen  20182019 3131  185 197.8  7.8      0.002    
## 10 8475852      Petr Mrazek     20182019 1965  125 138.7   7       0.004    
## ----------------------------------------------------------------------------
```
