#success shiny app


library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(rvest)
library(janitor)
library(shinyWidgets)
library(ggvis)
library(plotly)

#import dataset
path_in <- "/Users/emmagardecki/Documents/junior yr/stat231/Blog-Team-Basketball"
draft <- read_csv(paste0(path_in, "/draft_allplayers.csv"))
players <- read_csv(paste0(path_in, "/allPlayers2010-2021.csv"))
TEAMS0 <- c("ATL", "BOS", "NJN", "BKK", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", 
            "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", 
            "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS", "CHO", "NOH")

###########################################################################################################
ui <- navbarPage(
  theme = shinytheme("yeti"),
  title = "NBA Success",
  
  tabPanel(
    title = "VORP",
    fluidPage(
      fluidRow(
        mainPanel( width = 12,
                   plotlyOutput(outputId = "vorp", height = "350px", width = "100%"))
      )
    )
  ),
  
  tabPanel(
    title = "BPM",
    fluidPage(
      fluidRow(
        mainPanel( width = 12,
                   plotlyOutput(outputId = "bpm", height = "350px", width = "100%")
          
        )
      )
    )
  ),
  
  tabPanel(
    title = "FGpct",
    fluidPage(
      fluidRow(
        mainPanel(width = 12,
                  plotlyOutput(outputId = "fgp", height = "350px", width = "100%")
          
        )
      )
    )
  ),
  
  tabPanel(
    title = "PTS",
    fluidPage(
      fluidRow(
        mainPanel(width = 12,
                  plotlyOutput(outputId = "pts", height = "350px", width = "100%")
          
        )
      )
    )
  ),
  
  tabPanel(
    title = "TRB",
    fluidPage(
      fluidRow(
        mainPanel(width = 12,
                  plotlyOutput(outputId = "trb", height = "350px", width = "100%")
          
        )
      )
    )
  ),
  
  #footer
  hr(),
  p(em("Developed by Emma Gardecki and Eric Ingram")),
  p(em("Updated May 17, 2021"))
)
###########################################################################################################
server <- function(input, output) {
  #vorp
  output$vorp <- renderPlotly({
    plot_ly( draft, x = ~Years, y = ~VORP, type = "scatter", mode = "markers", name = ~Player) %>%
      add_trace(draft, color = ~as.factor(Draft_Year)) %>%
      layout(title = "Career Success - Value Over Replacement Player", showlegend = FALSE)
  })
  
  #bpm
  output$bpm <- renderPlotly({
    plot_ly( draft, x = ~Years, y = ~BPM, type = "scatter", mode = "markers", name = ~Player) %>%
      add_trace(draft, color = ~as.factor(Draft_Year)) %>%
      layout(title = "Career Success - Box Plus/Minus", showlegend = FALSE)
  })
  
  #fgp
  output$fgp <- renderPlotly({
    plot_ly( draft, x = ~Years, y = ~FGpct, type = "scatter", mode = "markers", name = ~Player) %>%
      add_trace(draft, color = ~as.factor(Draft_Year)) %>%
      layout(title = "Career Success - Field Goal %", showlegend = FALSE)
  })
  
  #pts
  output$pts <- renderPlotly({
    plot_ly( draft, x = ~Years, y = ~PTS, type = "scatter", mode = "markers", name = ~Player) %>%
      add_trace(draft, color = ~as.factor(Draft_Year)) %>%
      layout(title = "Career Success - Points per Game", showlegend = FALSE)
  })
  
  #trb
  output$trb <- renderPlotly({
    plot_ly( draft, x = ~Years, y = ~TRB, type = "scatter", mode = "markers", name = ~Player) %>%
      add_trace(draft, color = ~as.factor(Draft_Year)) %>%
      layout(title = "Career Success - Rebounds per Game", showlegend = FALSE)
  })
  
}
###########################################################################################################
shinyApp(ui = ui, server = server)



