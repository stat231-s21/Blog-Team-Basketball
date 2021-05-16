library(methods)
library(janitor)
library(tidyverse)
library(readr)
library(rvest)
library(robotstxt)
library(purrr)
library(stringr)
library(sjmisc)
teams <- c("https://www.basketball-reference.com/teams/ATL/", "https://www.basketball-reference.com/teams/BOS/",
           "https://www.basketball-reference.com/teams/BRK/",
           "https://www.basketball-reference.com/teams/CHO/", "https://www.basketball-reference.com/teams/CHI/",
           "https://www.basketball-reference.com/teams/CLE/", "https://www.basketball-reference.com/teams/DAL/",
           "https://www.basketball-reference.com/teams/DEN/", "https://www.basketball-reference.com/teams/DET/",
           "https://www.basketball-reference.com/teams/GSW/", "https://www.basketball-reference.com/teams/HOU/",
           "https://www.basketball-reference.com/teams/IND/", "https://www.basketball-reference.com/teams/LAC/",
           "https://www.basketball-reference.com/teams/LAL/", "https://www.basketball-reference.com/teams/MEM/",
           "https://www.basketball-reference.com/teams/MIA/", "https://www.basketball-reference.com/teams/MIL/",
           "https://www.basketball-reference.com/teams/MIN/", "https://www.basketball-reference.com/teams/NOP/",
           "https://www.basketball-reference.com/teams/NYK/", "https://www.basketball-reference.com/teams/OKC/",
           "https://www.basketball-reference.com/teams/ORL/", "https://www.basketball-reference.com/teams/PHI/",
           "https://www.basketball-reference.com/teams/PHO/", "https://www.basketball-reference.com/teams/POR/",
           "https://www.basketball-reference.com/teams/SAC/", "https://www.basketball-reference.com/teams/SAS/",
           "https://www.basketball-reference.com/teams/TOR/", "https://www.basketball-reference.com/teams/UTA/",
           "https://www.basketball-reference.com/teams/WAS/")

all_players <- data.frame()
paths_allowed("https://www.basketball-reference.com")
for(i in 1:length(teams)){
  for(j in 2010:2021){
    url <- ""
    if(j<2015 && substring(teams[i], 44, 46) == "CHO"){
      url <- paste("https://www.basketball-reference.com/teams/CHA/", j, ".html", sep = "")
    }
    else if(j<2014 && substring(teams[i], 44, 46) == "NOP"){
      url <- paste("https://www.basketball-reference.com/teams/NOH/", j, ".html", sep = "")
    }
    else if(j<2013 && substring(teams[i], 44, 46) == "BRK"){
      url <- paste("https://www.basketball-reference.com/teams/NJN/", j, ".html", sep = "")
    }
    else{
      url <- paste(teams[i], j, ".html", sep="")
    }
    print(url)
    tables <- url %>%
      read_html() %>%
      html_nodes("table")
    roster <- clean_names(html_table(tables[[1]])) %>%
      select(player, college) %>%
      mutate(Team = substring(url, 44, 46), Year = j)
    table_player <- url %>%
      read_html() %>%
      html_nodes("td a") %>%
      html_attr("href") %>%
      purrr::pluck() %>%
      as.data.frame() %>%
      clean_names() %>%
      mutate(URL = x) %>%
      select(URL) %>%
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
      inner_join(table_player, by=c("index"="index"))
    table_final <- filter(table_player_name, grepl("^/players", URL), grepl(".html", URL))
    table_final <- table_final %>%
      mutate(Player_URL = paste("https://www.basketball-reference.com", URL, sep = ""))%>%
      select(Name, Player_URL)
    vect_test = Vectorize(get_hs)
    table_final <- table_final %>%
      mutate(HS = vect_test(Player_URL))
    roster <- roster %>% inner_join(table_final, by=c("player" = "Name"))
    
    all_players <- all_players %>%
      bind_rows(roster)
    # all_players <- filter(all_players,
    #                       !is.na(all_players$college), !is.na(all_players$HS),
    #                       college != "", HS != "NA||")
  }
}

write_csv(all_players, file = "allPlayers2010-2021.csv")
# url <- "https://www.basketball-reference.com/teams/ATL/2021.html"
# tables <- url %>%
#   read_html() %>%
#   html_nodes("table")
# draft <- clean_names(html_table(tables[[1]]))%>%
#   select(player)
# table_player <- url %>%
#   read_html() %>%
#   html_nodes("td a") %>%
#   html_attr("href") %>%
#   purrr::pluck() %>%
#   as.data.frame() %>%
#   clean_names() %>%
#   mutate(URL = x) %>%
#   select(URL) %>%
#   rowid_to_column("index")
# table_player_name <- url %>%
#   read_html() %>%
#   html_nodes("td a") %>%
#   html_text() %>%
#   purrr::pluck() %>%
#   as.data.frame() %>%
#   clean_names() %>%
#   mutate(Name = x) %>%
#   select(Name) %>%
#   rowid_to_column("index") %>%
#   inner_join(table_player, by=c("index"="index"))
# table_final <- filter(table_player_name, grepl("^/players", URL), grepl(".html", URL))
# table_final <- table_final %>%
#   mutate(Player_URL = paste("https://www.basketball-reference.com", URL, sep = ""))%>%
#   select(Name, Player_URL)
# vect_test = Vectorize(get_hs)
# table_final <- table_final %>%
#   mutate(HS = vect_test(Player_URL))
# draft <- draft %>% inner_join(table_final, by=c("Player" = "Name"))
# all_players <- all_players %>%
#   bind_rows(draft)
# all_players <- filter(all_players, !is.na(all_players$Years), 
#                       !is.na(all_players$College), !is.na(all_players$HS),
#                       College != "", HS != "NA||")


get_hs <- function(url){
  tables <- url %>%
    read_html() %>%
    html_nodes("p")
  flag = FALSE
  for(i in 1:length(tables)){
    player_hs = vector()
    if(str_contains(html_text(tables[[i]]), "High School")){
      player_hs <- html_text(tables[[i]])
      flag = TRUE
      break
    }
  }
  if(flag){
    y <- unlist(str_split(player_hs, "\n"))
    hs = vector()
    for (i in 1:length(y)){
      if(nchar(y[i])>4){
        if(!str_contains(y[i], "High School")){
          x <- trimws(y[i])
          x <- trimws(x, which = "right", whitespace = ",")
          hs <- c(hs, x)
        }
      }
    }
  }else{
    hs = vector()
  }
  to_add = ""
  for(i in 1:length(hs)){
    to_add = paste0(to_add, hs[i], sep = "|")
  }
  return(to_add)
}
  '
  .'
