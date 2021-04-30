library(methods)
library(janitor)
library(tidyverse)
library(readr)
library(rvest)
library(robotstxt)
library(purrr)
library(stringr)
library(sjmisc)

all_players <- data.frame()
for (i in 2006:2020){
  url <- paste("https://www.basketball-reference.com/draft/NBA_", i, ".html", sep="")
  paths_allowed(url)
   tables <- url %>%
     read_html() %>%
     html_nodes("table")
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
     inner_join(table_player, by=c("index"="index"))
   table_final <- filter(table_player_name, grepl("^/players", URL))
   table_final <- table_final %>%
     mutate(Player_URL = paste("https://www.basketball-reference.com", URL, sep = ""),
            Img_URL = paste("https://www.basketball-reference.com/req/202103225/images/players/",
                        substring(URL, 12, str_length(URL)-5), ".jpg", sep="")) %>%
     select(Name, Player_URL, Img_URL)
   vect_test = Vectorize(get_hs)
   table_final <- table_final %>%
     mutate(HS = vect_test(Player_URL))
   draft <- draft %>% inner_join(table_final, by=c("Player" = "Name"))
   all_players <- all_players %>%
     bind_rows(draft)
   all_players <- filter(all_players, !is.na(all_players$Years), 
                         !is.na(all_players$College), !is.na(all_players$HS),
                         College != "", HS != "NA||")
 }
write_csv(all_players, file = "draft_allplayers.csv")
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








