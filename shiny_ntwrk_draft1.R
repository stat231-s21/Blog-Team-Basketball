## STAT231-02 PUG Blog Project: Blog-Basketball 
# Emma Gardecki, Eric Ingram

# interactive networks app

##load needed libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(rvest)
library(janitor)
library(shinyWidgets)
library(ggvis)

library(ggplot2)
library(ggiraph)
library(network)
library(sna)
library(ggnetwork)


###############
# import data #
###############
path_in <- "/Users/emmagardecki/Documents/junior yr/stat231/Blog-Team-Basketball"
draft <- read_csv(paste0(path_in, "/draft_allplayers.csv"))
players <- read_csv(paste0(path_in, "/allPlayers2010-2021.csv"))

#explicit commands to view vars as numeric
draft$Pick <- as.numeric(draft$Pick) 
draft$Years <- as.numeric(draft$Years)
draft$G <- as.numeric(draft$G)
draft$MP_Total <- as.numeric(draft$MP_Total)
draft$PTS_Total <- as.numeric(draft$PTS_Total)
draft$TRB_Total <- as.numeric(draft$TRB_Total)
draft$AST_Total <- as.numeric(draft$AST_Total)
draft$FGpct <- as.numeric(draft$FGpct)
draft$TPpct <- as.numeric(draft$TPpct)
draft$FTpct <- as.numeric(draft$FTpct)
draft$MP <- as.numeric(draft$MP)
draft$PTS <- as.numeric(draft$PTS)
draft$TRB <- as.numeric(draft$TRB)
draft$AST <- as.numeric(draft$AST)
draft$WS <- as.numeric(draft$WS)
draft$`WS/48` <- as.numeric(draft$`WS/48`)
draft$BPM <- as.numeric(draft$BPM)
draft$VORP <- as.numeric(draft$VORP)

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################

# for NBA network widgets:

# for year choice 
year_choices <- unique(draft$Draft_Year)


############
#    ui    #
############
ui <- navbarPage(
  
  theme = shinytheme("yeti"),
  
  title="NBA Connections",
  
  tabPanel(
    title = "NBA Connections by Season",
    fluidPage(
      # fluidRow(
        # column(12, 
        #        selectInput(inputId = "yrvar"
        #                    , label = "Choose a season:"
        #                    , choices = year_choices
        #                    , selected = "2010"))
        # column(4, 
        #        selectInput(inputId = "histvar"
        #                    , label = "Choose a variable of interest to plot:"
        #                    , choices = hist_choice_values
        #                    , selected = "College")),
        # column(4, 
        #        # selectInput(
        #        # inputId = "teaminput"
        #        #                  , label = "Include Teams:"
        #        #                  , choices = team_choices
        #        #                  , selected = "BOS",
        #        #                 multiple = TRUE)))
        #        uiOutput("team")
        # ),
        mainPanel( width = 12,
                   plotOutput(outputId = "hist", height = "350px", width = "100%")
        )
      )
  ),
  
  #footer
  hr(),
  p(em("Developed by Emma Gardecki and Eric Ingram")),
  p(em("Updated May 15, 2021"))
)

############
# server   #
############
server <- function(input,output){
  
  # TAB 2: NBA network
  output$team <- renderUI({
    yr <- draft[draft$Draft_Year == input$yrvar, "Team"]
    # selectInput(inputId = "teaminput"
    #             , label = "Include Teams:"
    #             , choices = unique(yr)
    #             , selected = "BOS"
    #             , multiple = TRUE)
  })
  
# draft_nba <- players %>%
#   select(player, Team, Year)
#     #build prelim nba network
#     draft_nba <- reactive({
#       filter(draft_nba, Year %in% input$yrvar) 
#   })
#   nba_graph <- graph_from_data_frame(draft_nba, directed = FALSE)
#   
#   nba_ntwrk <- ggnetwork(nba_graph)
#   
#   output$hist <- renderPlot({
#     
#     # nba_graph <- graph_from_data_frame(draft_nba, directed = FALSE)
#     # 
#     # nba_ntwrk <- ggnetwork(nba_graph)
#     
#     ggplot(data = nba_ntwrk
#            , aes(x = x, y = y, xend = xend, yend = yend)) +
#       geom_edges(arrow=arrow(type="closed", length=unit(6,"pt"))
#                  , color = "lightgray") +
#       geom_nodes() +
#       geom_nodelabel(aes(label = name)) +
#       theme_blank()
#   })
  
  nba <- players %>% 
    select(player, Team, Year) 
  
  # nba <- reactive({
  #   filter( nba, Year %in% input$yrvar)
  # })
  
  nbag <- graph_from_data_frame(nba, directed = FALSE)
  
  nbanet <- ggnetwork( nbag )
  nbanet$tooltip <- paste0("Player = ", nbanet$name)
  
  output$hist <- renderPlot({ 
    ggplot(data = nbanet
           , aes(x = x, y = y, xend = xend, yend = yend, tooltip = tooltip)) +
    geom_edges(color = "green") +
    geom_nodes(color = "gold", size = 4) +
    theme_blank() +
    # geom_nodetext(aes(label = name)) +
    geom_point_interactive(size=2) %>%
      ggiraph(code = {print()})
  
  # # htmlwidget call
  # ggiraph(code = {print(g01)})
  })
} 

#################### 
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

