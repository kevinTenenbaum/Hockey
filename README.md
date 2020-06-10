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