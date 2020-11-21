

source('~/R/Hockey/Scraper/ScrapeHockeyFunctions.r')
source('~/R/CreateDatabaseConnection.R')

############### Scrape API ###############
con <- dbCon('Hockey')
runID <- dbGetQuery(con, "select RunID from RunInfo order by updateTime desc limit 1") %>% unlist() + 1
dbDisconnect(con)



pullFullSeasons <- TRUE
startDate <- "20192020"
endDate <- "20192020"
Date <- Sys.Date()
daysBack <- 365
# startDate <- as.character(Date - daysBack)
# endDate <- as.character(Date)


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

cat("Pull Player Info... \n")
playerInfo <- (lapply(unique(players$playerid), function(x) getPlayerInfo(x)))
playerInfo <- (bind_rows(playerInfo))
playerInfo$runID <- runID

cat("Pulling Shift Data... \n")
shiftFrame <- getAllShifts((playedGames$gamePk)) %>% cleanColumnNames() 
shiftFrame$runID <- runID

options(stringsAsFactors = FALSE)

reversePaste <- function(x){
  paste0(rev(x))
}

pNames <- str_split((unique(shiftFrame$PlayerNameNumber)), ' ')
pNames <- lapply((pNames), function(x) rev(x[-1]))

complex <- which(sapply(pNames, length) > 2)
for(i in (complex)){
  nm <- pNames[[i]]
  fName <- nm[1]
  lName <- nm[2]
  mid <- rev(nm[3:length(nm)])
  
  pNames[[i]] <- c(fName, mid, lName)
}

pNames <- str_replace(tolower(unlist(lapply(pNames, paste, collapse = ' '))), ',', '')
pNames <- data.frame(name  = pNames, PlayerNameNumber = unique(shiftFrame$PlayerNameNumber), stringsAsFactors = FALSE)

joined <- playerInfo %>% mutate(name = tolower(fullName)) %>% select(name, id) %>% left_join(pNames, by = 'name') 

shiftFrame <- shiftFrame %>% left_join(joined %>% select(-name), by = c('PlayerNameNumber'))


cat("Writing data to database...\n")
runInfo <- data.frame(runID = runID, 
                      updateTime = Sys.time(),
                      startDate = startDate,
                      endDate = endDate,
                      daysBack = daysBack)



con <- dbCon("Hockey")


# Write Teams to database
dbExecute(con, "Truncate Table Teams")
RMariaDB::dbWriteTable(con, "Teams",  teams, append = TRUE)

#Write Rosers to database
dbExecute(con, "Truncate Table Rosters;")
dbWriteTable(con, "Rosters", rosters, append = TRUE)

# Write to RosterHistory 
dbExecute(con, paste0("delete from RosterHistory where RosterDate = '", Date, "'"))
dbWriteTable(con, "RosterHistory", rosters %>% mutate(RosterDate = Date), append = TRUE)


# Write Schedules 
dbExecute(con, "Drop Table if exists StagingSchedule")
dbWriteTable(con, "StagingSchedule", data.frame(gamePk = schedule$gamePk))

dbExecute(con, "
          delete sc
          from Schedule sc
          left join StagingSchedule s
          on sc.GamePk = s.GamePk
          where s.GamePk is not null")

dbWriteTable(con, "Schedule", schedule, append = TRUE)


# Write Events Data 
dbExecute(con, "
          delete e
          from Events e
          left join StagingSchedule s
          on e.GamePk = s.GamePk
          where s.GamePk is not null")
dbWriteTable(con, "Events", events, append = TRUE)

# Write Players Data
dbExecute(con, "
          delete e
          from Players e
          left join StagingSchedule s
          on e.GamePk = s.GamePk
          where s.GamePk is not null")
dbWriteTable(con, "Players", players, append = TRUE)

# Write Player Info Data
stagePlayerInfo <- dbReadTable(con, "PlayerInfo")
keepPlayerInfo <- stagePlayerInfo %>% anti_join(playerInfo, by = 'id')
for(i in 1:ncol(playerInfo)){
  class(playerInfo[,i]) <- class(keepPlayerInfo[,i])
}
playerInfo <- bind_rows(keepPlayerInfo, playerInfo)

dbWriteTable(con, "PlayerInfo", playerInfo, append = FALSE, overwrite = TRUE)

# Write Player Stats Data
stagePlayerStats <- dbReadTable(con, 'PlayerStats')
for(i in 1:ncol(playerStats)){
  class(playerStats[,i]) <- class(stagePlayerStats[,i])
}
keepPlayerStats <- stagePlayerStats %>% anti_join(playerStats, by = 'playerid')
playerStats <- bind_rows(keepPlayerStats, playerStats)

dbWriteTable(con, "PlayerStats", playerStats, append = TRUE)


# Write Shift Data
dbExecute(con, "
delete e
from Shifts e
left join StagingSchedule s
on e.GamePk = s.GamePk
where s.GamePk is not null")


dbWriteTable(con, "Shifts", shiftFrame, append = TRUE)



dbWriteTable(con, "RunInfo", runInfo, append = TRUE)


dbDisconnect(con)
cat("Shutting Down... \n")

system("sudo poweroff")
