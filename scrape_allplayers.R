library(methods)
library(janitor)
library(tidyverse)
library(readr)
library(rvest)
library(robotstxt)
library(purrr)
library(stringr)

# initialize blank dataframe
all_drafts <- data.frame()
all_players <- data.frame()
for (i in 2006:2020){
  url <- paste("https://www.basketball-reference.com/draft/NBA_", i, ".html", sep="")
  #Check if allowed
  #paths_allowed(url)
  #Scrape Data for drafts
  tables <- url %>%
    read_html() %>%
    html_nodes("table")
  
  
  #Clean data
  draft <- clean_names(html_table(tables[[1]]))%>%
    mutate(Pick = x_2, Team = x_3, Player = round_1, 
           College = round_1_2, Years = x_4, G = totals, 
           MP_Total = totals_2, PTS_Total = totals_3, TRB_Total = totals_4,
           AST_Total = totals_5, FGpct = shooting, TPpct = shooting_2,
           FTpct = shooting_3, MP = per_game, PTS = per_game_2,
           TRB = per_game_3, AST = per_game_4, WS = advanced, 
           `WS/48` = advanced_2, BPM = advanced_3, VORP = advanced_4
           , Draft_Year = i) %>%
    select(Draft_Year, Pick, Team, Player, College, Years, G, MP_Total, PTS_Total, TRB_Total, AST_Total, 
           FGpct, TPpct, FTpct, MP, PTS, TRB, AST, WS, `WS/48`, BPM, VORP) %>%
    filter(Player != "Player", Player != "Round 2")
  #Combine into one dataframe
  all_drafts <- all_drafts %>% 
    bind_rows(draft) 
  #Remove NAs
  all_drafts <- filter(all_drafts, !is.na(all_drafts$Years))
}
#Write to CSV
write_csv(all_drafts, file = "draft_allyears.csv")
#Scrape Player data, photo
for (i in 2006:2020){
  url <- paste("https://www.basketball-reference.com/draft/NBA_", i, ".html", sep="")
  table_player <- url %>%
    read_html() %>%
    html_nodes("td a") %>%
    html_attr("href") %>%
    purrr::pluck() %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(URL = x, Draft_Year = i) %>%
    select(URL, Draft_Year) %>%
    rowid_to_column("index")
  table_player_name <- url %>%
    read_html() %>%
    html_nodes("td a") %>%
    html_text() %>%
    purrr::pluck() %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(Name = x) %>%
    select(Name) %>%
    rowid_to_column("index") %>%
    inner_join(table_player, by=("index"="index"))
  table_final <- filter(table_player_name, grepl("^/players", URL))
  table_final <- table_final %>%
    mutate(URL2 = paste("https://www.basketball-reference.com", URL, sep = ""), 
           Img = paste("https://www.basketball-reference.com/req/202103225/images/players/", 
                       substring(URL, 12, str_length(URL)-5), ".jpg", sep="")) %>%
    select(Name, URL2, Draft_Year, Img)
  #combine datasets
  all_players <- all_players %>% 
    bind_rows(table_final) 
}
#write to csv file
write_csv(all_players, file = "draft_allplayers1.csv")

for (i in 1:length(all_players)){
  player <- all_players[i]
  player_url <- player$URL2
  tables <- player_url %>%
    read_html() %>%
    html_nodes("table")
  data <- clean_names(html_table(tables[[1]]))
}

###what variables in the dataset mean:
# Draft_Year: year player was drafted
# Pick: which pick player was in the draft
# Team: which team player was drafted by
# Player: player's name
# College: where player when to college
# Years: number of years player was in the NBA
# G: games. total number of games played in the NBA
# MP_Total: minutes played total. total minutes played in player's career
# PTS_Total: points total. total of all players's points in career
# TRB_Total: total rebounds total. total # of offensive and defensive rebounds in player's career
# AST_Total: assists total. total # of assists in player's career
# FGpct: career field goal percentage
# TPpct: career three point percentage
# MP: average minutes played per game, averaged over player's career
# PTS: average points scored per game, averaged over player's career
# TRB: average rebounds per game (both offensive and defensive), averaged over player's career
# AST: average assists per game, averaged over player's career
# WS: win shares. "estimate of the number of wins contributed by a player"
# `WS/48`: win shares per 48 mins. 
# "estimate of the number of wins contributed by a player per 48 minutes of play (league avg. ~0.100)"
# BPM: box plus/minus. 
# "a box score estimate of the points per 100 possessions a player 
# contributed above a league average player translated to an average team"
# VORP: value over replacement player. 
# "a box score estimate of the points per 100 TEAM possessions that a player 
# contributed above a replacement level (-2.0) player translated to an average team 
# and prorated to an 82 game season"













