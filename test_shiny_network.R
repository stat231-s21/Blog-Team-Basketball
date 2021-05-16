# http://curleylab.psych.columbia.edu/netviz/netviz1.html#/45

library(ggplot2)
library(ggiraph)
library(network)
library(sna)
library(ggnetwork)

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

draft_nba <- players %>%
  select(player, Team, Year) %>%
  filter(Year == "2010") 

nba_graph <- graph_from_data_frame(draft_nba, directed = FALSE)

nba_ntwrk <- ggnetwork( nba_graph, layout = "fruchtermanreingold"
                       , cell.jitter = 0.75 )
nba_ntwrk$tooltip <- paste0("betweenness = ", round(betweenness(n)[df$vertex.names],2))
  
gg_point_1 <- ggplot(data = nba_ntwrk
       , aes(x = x, y = y, xend = xend, yend = yend, tooltip = tooltip)) +
  geom_edges(aes(linetype = type), color = "grey50") +
  # geom_nodes(color = "black", size = 8) +
  theme_blank() +
  geom_nodetext(aes(label = LETTERS[names]), fontface = "bold") +
  # geom_nodelabel()
  geom_point_interactive(size=5)

# htmlwidget call
ggiraph(code = {print(gg_point_1)})

# girafe(ggobj = gg_point_1,
#        options = list(
#          opts_sizing(width = .7),
#          opts_toolbar(position = "bottomright") )
# )
##########################################################################################################
# n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
# n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
# e <- network.edgecount(n)
# set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))
# 
# 
# df<-ggnetwork(n, layout = "fruchtermanreingold", cell.jitter = 0.75)
# df$tooltip <- paste0("Betweenness = ", round(betweenness(n)[df$vertex.names],2))

# gg_point_1 <- ggplot(df, aes(x = x, y = y, xend=xend, color=family, yend=yend, tooltip = tooltip) )  +
#   geom_edges(aes(linetype = type), color = "grey50") +
#   geom_nodes(color = "black", size = 8) +
#   theme_blank() +
#   geom_nodetext(aes(label = LETTERS[vertex.names]), fontface = "bold") + 
#   geom_point_interactive(size=5)
# 
# # htmlwidget call
# ggiraph(code = {print(gg_point_1)}) 

