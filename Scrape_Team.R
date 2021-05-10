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
  for(j in 2019:2020){
    url <- paste(teams[i], j, ".html", sep="")
    if(j<2015 && substring(url, 44, 46) == "CHO"){
      url <- paste("https://www.basketball-reference.com/teams/CHA/", j, ".html", sep = "")
    }
    tables <- url %>%
      read_html() %>%
      html_nodes("table")
    roster <- clean_names(html_table(tables[[1]])) %>%
      select(player) %>%
      mutate(Team = substring(url, 44, 46), Year = j)
    all_players <- all_players %>%
      bind_rows(roster)
  }
}
