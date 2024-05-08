library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyverse)

data <- read.csv("Tree.csv")
data$Level.3[data$Level.3 == "-"] <- NA
data$Level.2[data$Level.2 == "-"] <- NA
data$Level.1[data$Level.1 == "-"] <- NA
data$Category[data$Category == "-"] <- NA
data$Cluster[data$Cluster == "-"] <- NA
data$Level.3[data$Level.3 == ""] <- NA
data$Level.2[data$Level.2 == ""] <- NA
data$Level.1[data$Level.1 == ""] <- NA
data$Category[data$Category == ""] <- NA
data$Cluster[data$Cluster == ""] <- NA

data <- data.frame(data$Category,data$Level.3,data$Level.2, data$Level.1)

links.df <- data %>%
  mutate(row = row_number()) %>%                                       
  pivot_longer(cols = -row, names_to = "column", values_to = "source") 


links.df <- links.df %>%
  mutate(column = match(column, names(data))) %>%   
  group_by(row) %>% 
  mutate(target = lead(source, order_by = column)) %>% 
  filter(!is.na(target)) %>%                           
  ungroup() 


links.df <- links.df[!is.na(links.df$source),]
###
links <- as.data.frame(links.df[3:4])
links <- unique(links)
links$value <- 1
data <- list()
nodes <- data.frame(name = unique(c(links$source, links$target)))
nodes$node <- c(1:nrow(nodes))

links <- left_join(links, nodes, by = c("source" = "name"))  %>%
  select(-source) %>%
  rename(source = node)

links <- left_join(links, nodes, by = c("target" = "name"))  %>%
  select(-target) %>%
  rename(target = node)

data[[1]] <- nodes
data[[2]] <- links
attr(data , "names" ) <- c("nodes","links")

library(jsonlite)
ListJSON=toJSON(data,pretty=F,auto_unbox=T)
ListJSON

save(ListJSON, file="test.json")


write.csv(test, "test.csv")

links.df <-
  links.df %>%
  mutate(source = paste0(source, '_', column)) %>%
  mutate(target = paste0(target, '_', column + 1)) %>% # target is the same as the 
  select(row, column, source, target)                  # source for the next column 


nodes.df <- data.frame(name = unique(c(links.df$source, links.df$target)))
nodes.df$label <- sub('_[0-9]*$', '', nodes.df$name) # what we use as NodeID


links.df$source_id <- match(links.df$source, nodes.df$name) - 1 # Create source_id column
links.df$target_id <- match(links.df$target, nodes.df$name) - 1 # Create target_id column
links.df$value <- 1                                             # Assign link values


sankeyNetwork(Links = links.df,     # from Step 2
                             Nodes = nodes.df,     # from Step 3
                             Source = 'source_id', # from Step 4
                             Target = 'target_id', # from Step 4
                             Value = 'value',      # from Step 4
                             NodeID = 'label',     # from Step 3
                             fontSize = 15,
              iterations = 0
              # change font size
)       # iterations = 0, prevent diagram layout changes


######## New format

links <- read.csv("TestFormatVis.csv")
links <- links[1:10,]
data <- list()
nodes <- data.frame(name = unique(c(links$source, links$target)))
nodes$node <- c(1:nrow(nodes))

links <- left_join(links, nodes, by = c("source" = "name"))  %>%
  select(-source) %>%
  rename(source = node)

links <- left_join(links, nodes, by = c("target" = "name"))  %>%
  select(-target) %>%
  rename(target = node)

data[[1]] <- nodes
data[[2]] <- links
attr(data , "names" ) <- c("nodes","links")

library(jsonlite)
ListJSON=toJSON(data,pretty=F,auto_unbox=T)
ListJSON

save(ListJSON, file="test.json")
