library(DBI)
library(RMariaDB)
library(rvest)
library(jsonlite)
library(httr)
library(dplyr)
library(stringr)
library(XML)
library(doParallel)
library(foreach)

baseURL <- "https://statsapi.web.nhl.com/"

getData <- function(url){
  fromJSON(as.character((GET(url))))
}

combineRows <- function(dfList, allPossibleCols  = FALSE){
  colNums <- sapply(dfList, ncol)
  if(allPossibleCols){
    cols<- unique(do.call(c, sapply(dfList, colnames)))
    shorter <- which(colNums < length(cols))
  } else{
    idx <- which.max(colNums)
    cols <- colnames(dfList[[idx]])
    shorter <- which(colNums < ncol(dfList[[idx]]))
  }
  
  if (length(shorter) > 0){
    for(s in shorter){
      missing <- setdiff(cols, colnames(dfList[[s]]))
      for(col in missing){
        dfList[[s]][,col] <- NA
      }
      
    }  
  }  
  
  
  
  outFrame <- do.call(rbind, lapply(dfList, function(x){
    
    x <- x[,cols]
  }))
  return(outFrame)
}


getTeamData <- function(){
  url <- paste0(baseURL, 'api/v1/teams')
  
  teamDat <- getData(url)$teams 
  teamDat$conference <- teamDat$conference$name
  teamDat$division <- teamDat$division$name
  teamDat$venue.city <- teamDat$venue$city
  teamDat$timeZone <- teamDat$venue$timeZone.offset
  teamDat$venue <- teamDat$venue$name
  teamDat$franchise <- teamDat$franchise$teamName
  
  return(teamDat)
}


getPerson <- function(link){
  url <- paste0(baseURL, link)
  person <- getData(url)$people %>% select(-primaryPosition, -currentTeam, -link, -fullName, -currentAge) 
  if(!'birthStateProvince' %in% colnames(person)){
    person$birthStateProvince <- NA
  }
  return(person)
}

getTeamRoster <- function(teamid){
  url <- paste0(baseURL, "api/v1/teams/", teamid, "/roster")
  
  rosterDat <- getData(url)$roster
  rosterDat <- data.frame(person = rosterDat$person,
                          jerseyNumber = rosterDat$jerseyNumber,
                          position = rosterDat$position,
                          stringsAsFactors = FALSE)
  people <- lapply(rosterDat$person.link, getPerson)
  
  peopleFrame <- combineRows(people)
  
  rosterDat <- bind_cols(rosterDat, peopleFrame)
  
  
  rosterDat$teamid <- teamid
  # rosterDat <- as.matrix(rosterDat)
  return(rosterDat)
}



getAllRosters <- function(teamDat){
  rosterList <- lapply(teamDat$id, getTeamRoster)
  rosters <- do.call(rbind, rosterList) %>% as.data.frame(stringsAsFactors = FALSE) %>% as.matrix() %>% as.data.frame(stringsAsFactors = FALSE)
  rosters$teamid <- as.integer(rosters$teamid)
  return(rosters)
}

# playerid <- 8471214
# season <- 20182019

getPlayerStats <- function(playerid, season){
  url <- paste0(baseURL, "api/v1/people/", playerid, "/stats?stats=statsSingleSeason&season=", season)
  
  playerStats <- getData(url)$stats$splits[[1]]
  playerStats$playerid <- playerid
  playerStats$season <- season
  return(unlist(playerStats))
}


getPlayerStatsList <- function(season, players, verbose = FALSE){
  
  statList <- lapply(players, function(pid){
    if(verbose){
      print(pid)
    }
    getPlayerStats(pid, season)
  })
  
  playerStats <- do.call(rbind, statList) %>% as.data.frame()
  
  # playerStats <- t(sapply(players, function(pid){
  #   if(verbose){
  #     print(pid)
  #   }
  #   getPlayerStats(pid, season)
  # })) %>% as.data.frame()
  # 
  return(playerStats)
}

# for(p in 1:length(unique(players$playerid))){
#   playerid <- unique(players$playerid)[p]
#   pl <- getPlayerInfo(playerid)
# }

getPlayerInfo <- function(playerid){
  url <- paste0(baseURL, "api/v1/people/", playerid)
  
  person <- getData(url)$people
  if(is.null(person$shootsCatches)){
    person$shootsCatches <- NA
  }
  if(is.null(person$birthStateProvince)){
    person$birthStateProvince <- NA
  }
  
  
  pos <- person$primaryPosition
  colnames(pos) <- paste0('Pos.', colnames(pos))
  person <- bind_cols(person[,c('id','fullName','firstName','lastName','birthDate','birthCity','birthStateProvince','birthCountry','nationality',
            'height','weight','active','rookie','rosterStatus', 'shootsCatches')], pos)
  
  return(person)  
}

getSeasons <- function(){
  url <- paste0(baseURL,"api/v1/seasons")
  getData(url)$seasons
}

getCurrentSeason <- function(){
  url <- paste0(baseURL, "api/v1/seasons/current")
  getData(url)$seasons$seasonId
}


# curSeason = '20182019'
getSchedule <- function(startDate = paste0(substring(curSeason, 1,4), "-08-01"), endDate = paste0(substring(curSeason, 5,8), "-07-31")){
  url <- paste0(baseURL, "api/v1/schedule?startDate=", startDate, "&endDate=", endDate)
  
  schedDat <- getData(url)
  schedule <- (schedDat)[['dates']][,'games']
  
  
  # ,'status.detailedState','teams.away.team.id', 'teams.away.team.name','teams.home.team.id','teams.home.team.name','teams.away.score','teams.home.score','venue.name'
  schedule <- lapply(schedule, function(row){
    
    info <- (row)[,c('gamePk','link','gameType','season','gameDate')]
    gameState <- row$status[,c('abstractGameState')]
    awayScore <- row$teams$away$score
    homeScore <- row$teams$home$score
    awayTeam <- row$teams$away$team[,c('id','name')]
    homeTeam <- row$teams$home$team[,c('id','name')]
    
    schedule <- data.frame(info, gameState, awayScore, homeScore, away = awayTeam, home = homeTeam)
    return(schedule)
  })
  
  schedule <- do.call(rbind, (schedule))
  
  return(schedule)
}

trimTime <- function(t){
  ifelse(substring(t, 1, 1) == 0, substring(t, 2,2), t)
}
# gameLink <- "/api/v1/game/2019020001/feed/live"
getGameEvents <- function(gameLink){
  url <- paste0(baseURL, gameLink)
  gameDat <- getData(url) 
  eventDat <- gameDat$liveData$plays$allPlays
  
  if(length(eventDat) > 0){
    
    if(nrow(eventDat$result) > 1){
      
      
      if(is.null(eventDat$result$strength)){
        eventDat$result$strength <- data.frame(code = rep(NA, nrow(eventDat$result)), name = rep(NA, nrow(eventDat$result)))
      }
  
    eventFrame <- data.frame(eventDat$result %>% select(-strength), 
                             eventDat$about %>% select(-goals),
                             score = eventDat$about$goals,
                             strength = eventDat$result$strength,
                             coord = eventDat$coordinates,
                             team = eventDat$team,
                             gamePk = gameDat$gamePk
    )
    eventFrame$eventNum <- 1:nrow(eventFrame)
    eventFrame <- eventFrame %>% mutate(timeSecRemaining = as.numeric(trimTime(substring(periodTimeRemaining, 1, 2)))*60 + as.numeric(trimTime(substring(periodTimeRemaining, 4,5))))
    
    
    if(is.null(eventFrame$penaltySeverity)){
      eventFrame <- bind_cols(bind_cols(eventFrame[,1:which(colnames(eventFrame) == 'emptyNet')], data.frame(penaltySeverity = rep(NA, nrow(eventFrame)), penaltyMinutes = rep(NA, nrow(eventFrame)))),
                eventFrame[,which(colnames(eventFrame) == 'emptyNet'):ncol(eventFrame)])
    }
    
    homeid <- gameDat$gameData$teams$home$id
    awayid <- gameDat$gameData$teams$away$id
    
    eventFrame$teamType <- ifelse(eventFrame$team.id == homeid, 'home',
                                  ifelse(eventFrame$team.id == awayid, 'away', NA_character_))
    
    tms <- schedule %>% filter(link  == gameLink) %>% select(awayid, homeid) %>% unlist()
    names(tms) <- c('skatersAway','skatersHome')
    
    
    eventFrame$skatersHome <- rep(5, nrow(eventFrame))
    eventFrame$skatersAway <- rep(5, nrow(eventFrame))
    
    # Label Penalties
    
    pens <- eventFrame %>% filter(event == 'Penalty') %>% select(timeSecRemaining, team.id, penaltyMinutes, penaltySeverity, period)
    if(nrow(pens) > 0){
      pens$PPID <- 1:nrow(pens)
      
      for(p in 1:nrow(pens)){
        penStamp <- pens[p,'timeSecRemaining']
        penTime <- pens[p,'penaltyMinutes']*60
        per <- pens[p,'period']
        tm <- names(tms)[which(tms == pens[p, 'team.id'])]
        
        
        if(penStamp < penTime){
          evs  <- which(between(eventFrame$timeSecRemaining, penStamp - penTime + 1, penStamp) & eventFrame$period == per)  
          evs2 <- which(between(eventFrame$timeSecRemaining, 1200 - (penTime - penStamp), 1200) & eventFrame$period == per + 1) 
          evs <- sort(c(evs, evs2))
          rm(evs2)
        } else {
          evs <- which(between(eventFrame$timeSecRemaining, penStamp - penTime + 1, penStamp) & eventFrame$period == per)  
        }
        
        eventFrame[evs, tm] <- pmax(3, eventFrame[evs, tm] - 1)
        eventFrame[evs, 'PPID'] <- pens[p,'PPID']
        
        
        eventFrame[evs, 'penaltySeverity'] <- ifelse(is.na(eventFrame[evs, 'penaltySeverity']), pens[p, 'penaltySeverity'], paste0(eventFrame[evs, 'penaltySeverity'], pens[p, 'penaltySeverity']))
      }
    } else{
      eventFrame$PPID <- NA
    }
    
    
    # for(i in 1:nrow(eventFrame)){
    #   tStamp <- eventFrame[i,'timeSecRemaining']
    #   iPeriod <- eventFrame[i,'period']
    #   pens <- eventFrame %>% filter(period == iPeriod & between(timeSecRemaining, tStamp - 120, tStamp) & event == 'Penalty') %>% dplyr::select(team.triCode, timeSecRemaining, penaltyMinutes, penaltySeverity)
    #   nPlayers <- rep(5, 2)
    #   names(nPlayers) <- na.omit(unique(eventFrame$team.triCode))
    #   
    #   for(p in pens %>% ){
    #     penTime <- pens[p, 'timeSecRemaining']
    #     penTime <- pens[p, 'penaltyMinutes']*60
    #   }
    #   
    # }
    
    
    goals <- eventFrame %>% filter(skatersHome < 5 & event == 'Goal' & penaltySeverity == 1) %>% select(event, PPID, timeSecRemaining, penaltySeverity)
    
    if(nrow(goals) > 0){
      for(g in 1:nrow(goals)){
        cond <- eventFrame$PPID == goals[g, 'PPID'] & timeSecRemaining < goals[g,'timeSecRemaining'] & str_detect(goals$penaltySeverity[g], 'minor')
        eventFrame[cond, tm] <- eventFrame[cond, tm] + 1
        eventFrame[cond, 'penaltySeverity'] <- str_replace(eventFrame[cond, 'penaltySeverity'], 'minor', '')
      }
      
      
      teamSides <- eventFrame %>% filter(event %in% c('Shot','Missed Shot','Goal')) %>% 
        group_by(period, teamType) %>%
        summarise(X = median(coord.x),
                  Y = median(coord.y)) %>%
        arrange(teamType, period)
      
      switchTeam <- teamSides %>% filter(period == 1 & X < 0) %>% ungroup() %>% dplyr::select(teamType) %>% unlist()
      otherTeam <- teamSides %>% filter(period == 1 & X > 0) %>% ungroup() %>% dplyr::select(teamType) %>% unlist()
      
      # Change Period 1, 3, 4
      eventFrame[eventFrame$period != 1 & eventFrame$teamType == switchTeam, 'coord.x'] <- eventFrame[eventFrame$period != 1 & eventFrame$teamType == switchTeam, 'coord.x']*(-1)
      eventFrame[eventFrame$period != 1 & eventFrame$teamType == switchTeam, 'coord.y'] <- eventFrame[eventFrame$period != 1 & eventFrame$teamType == switchTeam, 'coord.y']*(-1)
      
      # Change Period 2
      eventFrame[eventFrame$period == 2 & eventFrame$teamType == otherTeam, 'coord.x'] <- eventFrame[eventFrame$period == 2 & eventFrame$teamType == otherTeam, 'coord.x']*(-1)
      eventFrame[eventFrame$period == 2 & eventFrame$teamType == otherTeam, 'coord.y'] <- eventFrame[eventFrame$period == 2 & eventFrame$teamType == otherTeam, 'coord.y']*(-1)
      
      
    }
    
    
    
    
    playerFrame <- do.call(rbind, lapply(1:length(eventDat$players), function(i){
      row <- eventDat$players[[i]]
      
      if(!is.null(row)){
        data.frame(eventNum = rep(i, nrow(row)),
                   player = row$player, 
                   playerType = row$playerType,
                   gamePk = gameDat$gamePk)
                   
      }
      
    }))
    } else {
      eventFrame <- data.frame()
      playerFrame <- data.frame()
    }
  } else{
    eventFrame <- data.frame()
    playerFrame <- data.frame()
  }
  
  
  return(list(events = eventFrame, players = playerFrame))
  
}


getGameEventsList <- function(games){
  # gameEvents <- vector(mode = 'list', length = nrow(games))
  
  
  dl = file("runlog.Rout", open="wt")
  cl <- makeCluster(3)
  registerDoParallel(cl)
  
  # pb <- txtProgressBar(min = 1, max = nrow(games), style = 3)
  gameEvents <- foreach(i = 1:nrow(games), .packages = c('rvest','jsonlite', 'XML','stringr','dplyr','httr'),
                    .export = c('getGameEvents','baseURL','getData', 'trimTime','schedule')) %dopar% {
    sink("runlog.Rout", append=TRUE)
    cat(round(i/nrow(games), 2))
    sink()
    getGameEvents(games$link[i]) %>% return()
    
  }
  
  # for(i in 1:nrow(games)){
  #   setTxtProgressBar(pb, i)
  #   gameEvents[[i]] <- getGameEvents(games$link[i])
  # }
  # gameEvents <- lapply(1:nrow(games), function(i) {
  #   events <- getGameEvents(games$link[i])
  # })
  
  # events <- do.call(rbind, lapply(gameEvents, function(x){
  #   x$events
  # }))
  
  
  events <- lapply(gameEvents, function(x){
      ev <- x$events
      
      if(is.null(ev)){
        return(data.frame())
      } else if(nrow(ev) == 0){
        return(data.frame())  
      } else if(!'emptyNet1'  %in% colnames(ev)){
        ev$emptyNet1 <- 0
      }  else if(!'coord.x' %in% colnames(ev)){
        ev$coord.x <- NA
        ev$coord.y <- NA
      }
    
    return(ev)
  })
  
  # events <- events[sapply(events, ncol) == 35]
  # cols <- colnames(events[[1]])
  
  # events <- lapply(events, function(x) x[,cols])
  
  events <- lapply(events, function(x){
    # x <- x %>% select(-starts_with('empty_net.'))
    colnames(x) <- str_replace(colnames(x), '1', '')
    
    # x[,cols[1:26]]
    x
  })
  
  # cols <- colnames(events[[1]])
  events <- combineRows(events, allPossibleCols = TRUE)
  # events <- lapply(events, function(x) x[,cols])
  # events <- do.call(rbind, events)
  # 
  players <- do.call(rbind, lapply(gameEvents, function(x){
    x$players
  }))
  
  return(list(events = events, players = players))
}

getSeasonInfo <- function(){
  url <- paste0(baseURL, "api/v1/seasons")
  return(getData(url)$seasons)
}


cleanColumnNames <- function(df){
  colnames(df) <- str_replace_all(colnames(df), '[.]', '')
  return(df)
}


dropTable <- function(name){
  con <- dbCon('Hockey')
  dbExecute(con, paste0("drop table if exists ", name))
  dbDisconnect(con)
}

dropAllTables <- function(){
  
  tables <- dbListTables(con)
  tables <- tables[!(tables %in% c('Team.Team','Test.test','Staging.Schedule'))]  
  sapply(tables, dropTable)
}



getShiftData <- function(gamePk, team = 'Home'){
  # url <- paste0("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=", gamePk)
  # url <- paste0("http://www.nhl.com/stats/shiftcharts?id=", gamePk)
  
  homeAwayCode <- ifelse(team == 'Home', 'TH','TV')
  season <- substring(gamePk, 1, 4)
  season <- paste0(season, as.numeric(season) + 1)
  gameCode <- substring(gamePk, 5, )
  
  url <- paste0("http://www.nhl.com/scores/htmlreports/", season,"/", homeAwayCode, gameCode, ".HTM")
  
  shifts <- url %>% read_html() %>% html_table(fill = TRUE)
  shifts <- shifts[[10]] %>% filter(X1 != "") # Filter out blank rows
  shifts <- shifts[,1:6]
  
  headerRows <- which(shifts[,1] == shifts[,2] & between(nchar(shifts[,1]), 4, 100))
  # footerRows <- lead(headerRows) - 1
  # footerRows[length(footerRows)] <- headerRows[length(headerRows)] + which(shifts[headerRows[length(headerRows)]:nrow(shifts),1] == "Per")-2
  footerRows <- which(shifts[,1] == 'Per')-1
  
  
  cNames <- shifts[headerRows[1]+1,]
  players <- shifts[(headerRows), 1]
  
  
  playerShifts <- lapply(1:length(headerRows), function(p){
    shiftDat <- shifts[(headerRows[p]+2):(footerRows[p]-1),]   
    colnames(shiftDat) <- cNames
    shiftDat$PlayerNameNumber <- players[p] 
    return(shiftDat)
  })
  names(playerShifts) <- players
  shifts <- do.call(rbind, playerShifts)
  
  # shifts <- GET(url) %>% content(as = 'text', encoding = 'UTF-8') %>% fromJSON()
  # shifts <- shifts$data
  shifts$gamePk <- gamePk
  return(shifts)
}

getAllShifts <- function(gamePks){
  # shifts <- vector(mode = 'list', length = length(gamePks))
  # pb <- txtProgressBar(min = 1, max = length(gamePks), style = 3)
  # for(i in 1:length(gamePks)){
  dl = file("runlog.Rout", open="wt")
  cl <- makeCluster(3)
  registerDoParallel(cl)
  shifts <- foreach(i = 1:length(gamePks), .packages = c('dplyr','jsonlite','rvest', 'httr'),
                    .export = c('getShiftData', 'gamePks'),
                    .errorhandling = "pass") %dopar% {
    # setTxtProgressBar(pb, i)
    sink("runlog.Rout", append=TRUE)  
    cat(i, '/',length(gamePks), '\n', append = TRUE)
    home <- getShiftData(gamePks[i], team = 'Home')
    home$TeamType <- 'Home'
    away <- getShiftData(gamePks[i], team = 'Away')
    away$TeamType <- 'Away'
    # shifts[[i]] <- rbind(home, away)
    # shifts[[i]] <- getShiftData(gamePks[i])
    sink(type="output")
    return(rbind(home, away))
                    }
  
  
  
  shifts <- do.call(rbind, shifts[sapply(shifts, is.data.frame)])
  return(shifts)
}
