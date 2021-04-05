library(readxl)
library(tidyverse)
library(dplyr)
library(plotly)
##Read in ID csv
IDS_0406 <- read_excel("IDS/IDS-0406.xlsx")

IDS_0406 <- read_excel("IDS/IDS-0407.xlsx")

##join ID csv
IDS <- bind_rows(IDS_0406, IDS_0407)

#Filter out Source IP 6667 for April 6

sourcePort_6667_0406 <- IDS_0406%>%
  filter(sourcePort == "6667")

#Filter out the non-6667 source IPs
non_sourcePort_6667 <- IDS_0406%>%
  filter(sourcePort != "6667")

#Filter out Source IP 6667 for April 7

sourcePort_6667_0407 <- IDS_0407%>%
  filter(sourcePort == "6667")

#write csv for my girl Isabel 
#write_csv(sourcePort_6667_0407, "port_6667_0407.csv")
--------------------------------------------------------------------------------------------------------------
#Types of classification 
  
#April 6

  ggplot(IDS_0406, aes(x=classification, fill=as.factor(priority))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Classification and Number of Error Messages on April 6th ", 
       y = "Count", 
       x = "Classification Type") +
  scale_fill_discrete(
    name = "Priority Level")

#Timeline April 6
ggplot(IDS_0406, aes(x=time, y= classification, color=as.factor(priority))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Timeline of Error Messages on April 6th ", 
       y = "Classification Type", 
       x = "Time",
       fill = "Priority Level") 
  
#April 7 
ggplot(IDS_0407, aes(x=classification, fill=as.factor(priority))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Classification and Number of Error Messages on April 6th - April 7th ", 
       y = "Count", 
       x = "Classification Type") +
  scale_fill_discrete(
    name = "Priority Level")

#Classification for entire Timeline
ggplot(IDS, aes(x=classification, fill=as.factor(priority))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Classification and Number of Error Messages on April 5 - 7 ", 
       y = "Count", 
       x = "Classification Type") +
  scale_fill_discrete(
    name = "Priority Level")

#Timeline April 7
ggplot(IDS_0407, aes(x=time, y= classification, color=as.factor(priority))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Timeline of Error Messages on April 7th ", 
       y = "Classification Type", 
       x = "Time",
       fill = "Priority Level") 

#Timeline 
t <- ggplot(IDS, aes(x=time, y= classification, color=as.factor(priority))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Timeline of High Priority Syslog Messages on April 5 - 7 ", 
       y = "Classification Type", 
       x = "Time",
       fill = "Priority Level") 

ggplotly(t)

#filter port 6667 by classification type 
sourcePort_6667_0406 <- IDS_0406%>%
  filter(sourcePort == "6667")

classification_type <- unique(sourcePort_6667_0406[c("classification")])

#Destination port info
destination_port <- unique(sourcePort_6667_0406[c("destPort")])

#source port info
source_port <- unique(sourcePort_6667_0406[c("sourcePort")])
------------------------------------------------------------------------------------------------------------------
#visualize relationship between source and destination port 

library(hrbrthemes)
library(viridis)

bubble <- ggplot(IDS_0406, aes(sourcePort, destPort, color = classification)) +  
  geom_point(alpha=0.5) 

ggplotly(bubble)
----------------------------------------------------------------------------------------------------------------------
#Network Graph 
  
  library(igraph)

#create data 
links = data.frame(
  source = IDS_0406$sourcePort,
  target = IDS_0406$destPort
  )

#turn it into igraph object
network <- graph_from_data_frame(d=links, directed=T) 

# Count the number of degree for each node:
deg <- degree(network, mode="all")

# Plot
plot(network)

---------------------------------------------------------------------------------------------------------------------
  
  #plotly Network graph
  
  library(networkD3)

#create data 
links = data.frame(
  source = IDS_0406$sourcePort,
  target = IDS_0406$destPort
)

# Plot
p <- simpleNetwork(links, height="100px", width="100px", 
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 30,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)


p

# save the widget
setwd("/Users/viviennemaxwell/Documents/Spring2021/Visual Analytics/Bankworld")

library(htmlwidgets)
saveWidget(p, file=paste0( getwd(), "/HtmlWidget/networkInteractive2.html"))