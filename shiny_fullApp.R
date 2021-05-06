## STAT231-02 PUG Project: Shiny-Basketball 
# Emma Gardecki, Eric Ingram

# fullApp.R: final app draft 

##load needed libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(rvest)
library(janitor)
library(shinyWidgets)
# library(ggvis)


###############
# import data #
###############
draft <- read_csv("draft_allyears.csv")
draft <- draft %>%
  filter(!is.na(Pick)) %>%
  filter( !is.na(Years)) ##removes all players with no stats

player_data <- read_csv("draft_allplayers.csv")

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

# draft %>%
#   filter(Team == "BOS") %>%
#   count()

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################

# for TAB 2 (HISTOGRAM) widgets: 
# for selectInput, 'choices' object should be a NAMED LIST
hist_choice_values <- c("College","Years","`WS`", "VORP")
hist_choice_names <- c("College","Years Played","Win Shares","VORP")
names(hist_choice_values) <- hist_choice_names

# for team choice
team_choices <-  unique(draft$Team)

# for year choice 
year_choices <- unique(draft$Draft_Year)

# for TAB 3 (SCATTERPLOT) widgets: 
# for radio button in scatterplot tab
size_choice_values <- c("Pick", "G")
size_choice_names <- c("Draft Pick", "Games Played in Career")
names(size_choice_values) <- size_choice_names

# Y axis choices -- looking at per game stats
yax_choice_values <- c("PTS", "AST", "TRB", "MP")
yax_choice_names <- c(" Avg. Points Per Game", "Avg. Assists Per Game", 
                      "Avg. Total Rebounds per Game", "Avg. Minutes Played Per Game")
names(yax_choice_values) <- yax_choice_names

# for selectizeInput choices for player name, pull directly from data
name_choices <- unique(draft$Player)

# for TAB 4 (TABLE) widgets: 
# for selectizeInput choices for team name, pull directly from data
table_choices <- unique(draft$Team)

############
#    ui    #
############
ui <- navbarPage(
  
  theme = shinytheme("yeti"),
  
  title="NBA Players by Draft Class",
  
  tabPanel(
    title = "",
    # title = "Home",
    mainPanel(
      h1(strong("NBA Players by Draft Class")),
      h2(""),
      imageOutput("myImage", height = "200px"),
      
      tags$blockquote(
        h2(strong("Background", style = "font-family: font-si16pt")),
        p("The NBA Draft is a yearly event in which NBA teams can draft players who are allowed to enter the league.
        Typically, the best players available are picked first, and many high draft picks go on and become extremely successful. 
        However, this isn't always the case. Many low pick players go on to be successful and some high pick players
         go on to be a flop. In this project, we examine players drafted into the NBA.")),
      
      tags$blockquote(
        h2(strong("Questions of Interest", style = "font-family: font-si16pt")),
        p("We wanted to explore the following questions:"), 
        p("Who were the top draft picks from each year? Were they successful NBA players?"), 
        p("How do players with similar draft positions compare statistically 
          both in the same draft and across different years?"), 
        p("Were some draft classes stronger than others? 
          Are there any interesting observations about certain draft classes?")),
      
      tags$blockquote(
        h2(strong("Data Source", style = "font-family: font-si16pt")),
        p("Our data was scraped from basketball-reference.com. We scraped the career stats from every 
        draft class since 1980, which was around when statistical data starts to look similar to today's data. 
        As a result, we get 40 years of data. We also scraped each player's yearly stats to compare players.")),
      
      tags$blockquote(
        h4("Variables explained:"),
        h5(span(em("Draft Year:")), " the year the player was drafted"), 
        h5(span(em("Pick:")), " when the player was selected in the draft"), 
        h5(span(em("Team:")), " which team the player was drafted by"), 
        h5(span(em("Player:")), " player's name"),
        h5(span(em("College:")), " where the player went to college"), 
        h5(span(em("Years:")), " the number of years the player played in the NBA"), 
        h5(span(em("G:")), " the number of games the player played in their NBA career"), 
        h5(span(em("MP Total:")), " the total number of minutes played in the player's career"), 
        h5(span(em("PTS Total:")), " the total number of points scored in the player's career"), 
        h5(span(em("TRB Total:")), " the total number of offensive and defensive rebounds 
        the player got in their career"), 
        h5(span(em("AST Total:")), " the total number of assists the player dished out in 
        their career"), 
        h5(span(em("FGpct:")), " the player's career field goal percentage"), 
        h5(span(em("TPpct:")), " the player's career 3-point field goal percentage"), 
        h5(span(em("MP:")), " the average number of minutes played per game over the course 
        of a player's career"), 
        h5(span(em("PTS:")), " the average number of points scored per game over the course 
        of a player's career"), 
        h5(span(em("TRB:")), " the average number of rebounds collected per game over the 
        course of a player's career"), 
        h5(span(em("AST:")), " the average number of assists per game over the course of 
        a player's career"), 
        h5(span(em("WS:")), " win shares. 'estimate of the number of wins contributed by 
        a player' (basketball-reference.com)"), 
        h5(span(em("WS/48:")), " win shares per 48 minutes. 'estimate of the number of wins 
        contributed by a player per 48 minutes of play, league average is 0.100' 
        (basketball-reference.com)"), 
        h5(span(em("BPM:")), " box plus/minus. 'box score estimate of the points per 100 
        possessions a player contributed above a league average player translated to 
        an average team' (basketball-reference.com)"), 
        h5(span(em("VORP:")), " value over replacement player. 'box score estimate of the 
        points per 100 Team possessions that a player contributed to above a replacement level 
        (-2.0) player translated to an average team and prorated to an 82 game season' 
        (basketball-reference.com)")
      )
    )
    , icon = icon("fas fa-home")),
  
  tabPanel(
    title = "Histogram",
    fluidPage(
      fluidRow(
        column(4, 
               selectInput(inputId = "histyearvar"
                           , label = "Choose a draft class:"
                           , choices = year_choices
                           , selected = "1996")),
        column(4, 
               selectInput(inputId = "histvar"
                           , label = "Choose a variable of interest to plot:"
                           , choices = hist_choice_values
                           , selected = "College")),
        column(4, 
               # selectInput(
               # inputId = "teaminput"
               #                  , label = "Include Teams:"
               #                  , choices = team_choices
               #                  , selected = "BOS",
               #                 multiple = TRUE)))
               uiOutput("team")
        ),
        mainPanel( width = 12,
                   plotOutput(outputId = "hist", height = "350px", width = "100%")
        )
      )
    )
  ),
  
  # tabPanel(
  #   title = "Scatterplot",
  #   fluidPage(
  #     fluidRow(
  #       column(3, 
  #              selectInput(inputId = "plotyearvar"
  #                          , label = "Choose a draft class:"
  #                          , choices = year_choices
  #                          , selected = "1996")),
  #       column(3, 
  #              selectInput(inputId = "yaxchoices"
  #                          , label = "Y axis:"
  #                          , choices = yax_choice_values
  #                          , selected = "PTS")),
  #       column(3, 
  #              selectInput(inputId = "sizechoices"
  #                          , label = "Size points by: "
  #                          , choices = size_choice_values
  #                          , selected = "Pick")),
  #       column(3, 
  #              # selectizeInput(inputId = "id_name"
  #              #                , label = "Identify player(s) in the scatterplot:"
  #              #                , choices = name_choices
  #              #                , selected = NULL
  #              #                , multiple = TRUE))
  #              uiOutput("playerid")
  #       ),
  #       mainPanel( width = 12,
  #                  plotOutput(outputId = "scatter", height = "350px", width = "100%")
  #       )
  #     )
  #   )
  # ),
  
    plotlyOutput("plot"),
    verbatimTextOutput("hover"),
    verbatimTextOutput("click"),
    verbatimTextOutput("brush"),
    verbatimTextOutput("zoom"),
    
    title="NBA Players by Draft Class",
    
    tabPanel(
      title = "Scatterplot",
      fluidPage(
        fluidRow(
          column(4,
                 selectInput(inputId = "plotyearvar"
                             , label = "Choose a draft class:"
                             , choices = year_choices
                             , selected = "1996")),
          column(4,
                 selectInput(inputId = "yaxchoices"
                             , label = "Y axis:"
                             , choices = yax_choice_values
                             , selected = "PTS")),
          column(4,
                 selectInput(inputId = "sizechoices"
                             , label = "Size points by: "
                             , choices = size_choice_values
                             , selected = "Pick"))
        )
      )
    ),
  
  tabPanel(
    title = "Table",
    
    fluidPage(
      fluidRow(
        column(6, 
               selectInput(inputId = "tableyearvar"
                           , label = "Choose a draft class:"
                           , choices = year_choices
                           , selected = "1996")),
        column(6, 
               # selectizeInput(inputId = "tbl"
               #                , label = "Choose one or more teams:"
               #                , choices = table_choices
               #                , selected = c("ATL", "BOS", "BRK", "CHA", "CHH", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW",
               #                               "HOU", "IND", "KCK", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NJN", "NOH", "NOK",
               #                               "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "SDC", "SEA", "TOR", 
               #                               "UTA", "VAN", "WAS", "WSB")
               #                , multiple = TRUE))
               uiOutput("tblteams"))
      ),
      mainPanel( width = 12,
                 DT::dataTableOutput(outputId = "table", height = "350px", width = "100%")
      )
    )
  ), 
  tabPanel(
    title = "Compare Players",
    
    fluidPage(
      fluidRow(
        column(6, 
               selectInput(inputId = "compareplayervar"
                           , label = "Choose Player 1:"
                           , choices = name_choices
                           , selected = "Kobe Bryant")),
        column(6, 
               selectInput(inputId = "compareplayer2var"
                           , label = "Choose Player 2:"
                           , choices = name_choices
                           , selected = "Allen Iverson"))
      ),
      mainPanel( width = 12,
                 htmlOutput("Player1"),
                 DT::dataTableOutput(outputId = "table1", height = "350px", width = "100%"),
                 br(), br(), br(), br(),
                 htmlOutput("Player2"),
                 DT::dataTableOutput(outputId = "table2", height = "350px", width = "100%")
      )
    )
  ),
  
  #footer
  hr(),
  p(em("Developed by Emma Gardecki and Eric Ingram")),
  p(em("Updated Apr. 12, 2021"))
)

############
# server   #
############
server <- function(input,output){
  
  output$myImage <- renderImage({
    filename <- normalizePath(file.path('./images',
                                        paste('main', '.jpg', sep='')))
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = "image1")
  }, deleteFile = FALSE)
  
  # TAB 2: HISTOGRAM
  output$team <- renderUI({
    yr <- draft[draft$Draft_Year == input$histyearvar, "Team"]
    selectInput(inputId = "teaminput"
                , label = "Include Teams:"
                , choices = unique(yr)
                , selected = "BOS"
                , multiple = TRUE)
  })
  
  data_for_hist <- reactive({
    filter(draft, Team %in% input$teaminput)
  })
  
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), 
           aes_string(x = input$histvar)) +
      geom_histogram(stat = "count", color = "firebrick2", fill = "royalblue2", alpha = 0.7) +
      labs(x = hist_choice_names[hist_choice_values == input$histvar]
           , y = "Number of Players") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(breaks = c(1:340) ) 
  })
  
  # TAB 3: INTERACTIVE SCATTERPLOT 
  # output$playerid <- renderUI({
  #   yearid <- draft[draft$Draft_Year == input$plotyearvar, "Player"]
  #   selectizeInput(inputId = "id_name", 
  #                  label = "Identify player(s) in the scatterplot:", 
  #                  choices = unique(yearid), 
  #                  selected = NULL, 
  #                  multiple = TRUE)
  # })
  # 
  # output$scatter <- renderPlot({
  #   p <- draft %>%
  #     filter(Draft_Year == input$plotyearvar) %>%
  #     ggplot(aes_(x="Years", y= input$yaxchoices, size = input$sizechoices)) +
  #     geom_point(aes( color = "firebrick2"), position = "jitter") 
  #   
  #   p + geom_label_repel(draft, aes(label = "Player")) +
  #     
  #     labs(x = "Length of career", y = yax_choice_names[yax_choice_values == input$yaxchoices]
  #          , title = paste0("Career ", yax_choice_names[yax_choice_values == input$yaxchoices])
  #          , subtitle = paste0(input$plotyearvar, " Draft Class")
  #          , size = (size_choice_names[size_choice_values == input$sizechoices])) +
  #     guides(color = FALSE, color = guide_legend(override.aes = list(shape = 5))) +
  #     theme(legend.title = element_text(face = "bold"))
  #   
  # })
  
  get_data <- reactive({
    event.data <- event_data("plotly_selected", source = "subset")
    data <- draft %>% mutate(show_id = FALSE)
    if (!is.null(event.data)) {
      data$show_id[event.data$pointNumber + 1] <- TRUE
    }
    data
  })
  
  output$plot <- renderPlotly({
    data <- get_data()
    p <- 
      # filter(data$Draft_Year == input$plotyearvar) %>%
      ggplot(data, aes(x = Years, y = input$yaxchoices, size = Pick, key = Player)) +
      geom_point(aes(color = "royalblue2"), position = "jitter") + 
      geom_text(data=subset(data, show_id),aes( Years, PTS, label= Player)
                , position = position_jitter(width = 20,height = 20), show.legend = FALSE) +
      labs(x = "Length of career", y = yax_choice_names[yax_choice_values == input$yaxchoices]
           , title = paste0("Career ", yax_choice_names[yax_choice_values == input$yaxchoices])
           , subtitle = paste0(input$plotyearvar, " Draft Class")
           , size = (size_choice_names[size_choice_values == input$sizechoices])) +
      guides(color = FALSE) +
      theme( axis.title = element_text(face = "bold"))
    ggplotly(p, source = "subset") %>% layout(dragmode = "select")
  })
  
  # TAB 4: TABLE
  output$tblteams <- renderUI({
    tblyr <- draft[draft$Draft_Year == input$tableyearvar, "Team"]
    selectizeInput(inputId = "tbl"
                   , label = "Choose one or more teams:"
                   , choices = unique(tblyr)
                   , selected = c("ATL", "BOS", "BRK", "CHA", "CHH", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW",
                                  "HOU", "IND", "KCK", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NJN", "NOH", "NOK",
                                  "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "SDC", "SEA", "TOR", 
                                  "UTA", "VAN", "WAS", "WSB")
                   , multiple = TRUE)
  })
  
  
  data_for_table <- reactive({
    # filter(draft, Draft_Year %in% input$tableyearvar)
    filter(draft, Team %in% input$tbl)
  })
  
  output$table <- renderDataTable({
    data_for_table()
  })
  
  
  #TAB 5: COMPARE PLAYERS
  #Isolate the data for player 1
  data_for_player_1 <- reactive({
    data <- filter(player_data, Name %in% input$compareplayervar)
    player_url <- data$URL2
    tables <- player_url %>%
      read_html() %>%
      html_nodes("table")
    data <- clean_names(html_table(tables[[1]]))
  })
  #Isolate the URL for player 1's photo
  url_for_photo_1 <- reactive({
    data <- filter(player_data, Name %in% input$compareplayervar)
    url_1 <- data$Img
  })
  #output player 1's name and Photo
  output$Player1 <- renderUI({
    str1 <- paste("<h1>Player 1:", input$compareplayervar, "</h1>")
    str2 <- paste("<img src =", url_for_photo_1(), ">", sep = "")
    HTML(str1, str2)
  })
  #display player 1's data
  output$table1 <- DT::renderDataTable({ 
    data_for_player_1()
  })
  #Isolate the data for player 2
  data_for_player_2 <- reactive({
    data <- filter(player_data, Name %in% input$compareplayer2var)
    player_url <- data$URL2
    tables <- player_url %>%
      read_html() %>%
      html_nodes("table")
    data <- clean_names(html_table(tables[[1]]))
    # %>%
    #   mutate(Season = season, Age = age, Team = tm, Position = pos, G = g, 
    #          GS = gs, PTS = mp, FGpct = fg_percent, TPpct = x3_percent)
  })
  #Isolate the URL for player 2's photo
  url_for_photo_2 <- reactive({
    data <- filter(player_data, Name %in% input$compareplayer2var)
    url_2 <- data$Img
  })
  #display player 2's name and Photo
  output$Player2 <- renderUI({
    str1 <- paste("<h1>Player 2:", input$compareplayer2var, "</h1>")
    str2 <- paste("<img src =", url_for_photo_2(), ">", sep = "")
    HTML(str1, str2)
  })
  #display player 2's data
  output$table2 <- DT::renderDataTable({ 
    data_for_player_2()
  })
} 

#################### 
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

