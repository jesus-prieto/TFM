
library(stringr)
library(dplyr)
library(igraph)


from_data_to_graph <- function(data, wos=T, keyword="[1-9]|WOS"){


  data <- unique(data[order(data$ut),])
  freq <- as.data.frame(table(data$country))
  n <- nrow(data)
  
  if (n<10000){
    graph_data <- graph.data.frame(data,directed=FALSE)
    V(graph_data)$type <- str_detect(V(graph_data)$name, keyword)
    graph_data.pr <- bipartite.projection(graph_data)
    graph_data.pr <- graph_data.pr$proj1
    d <- get.data.frame(graph_data.pr)
    d <- d %>% group_by(from, to) %>% summarise(weight = sum(weight))
    return(list(d = d, freq = freq))
  }
  
  from <- seq(1, n, by = 1000) 
  to <- c(from[-1] - 1, n) 
  
  while (data[to[1], 1] == data[from[2], 1]){
    
    to[1] = to[1] + 1
    from[2] = from[2] + 1
    
  }
  
  subset <- data[from[1]:to[1],]
  
  graph_data <- graph.data.frame(subset,directed=FALSE)

  V(graph_data)$type <- str_detect(V(graph_data)$name, keyword)
  
  graph_data.pr <- bipartite.projection(graph_data)
  graph_data.pr <- graph_data.pr$proj1
  d <- get.data.frame(graph_data.pr)
  
  m <- length(from)
  
  for (i in 2:(m)){
    
    while (data[to[i], 1] == data[from[i + 1], 1] & i < m){
      
      to[i] = to[i] + 1
      from[i + 1] = from[i + 1] + 1
      
    }
    
    subset <- data[from[i]:to[i],]
    
    graph_data <- graph.data.frame(subset,directed=FALSE)

    V(graph_data)$type <- str_detect(V(graph_data)$name, keyword)

    graph_data.pr <- bipartite.projection(graph_data)
    graph_data.pr <- graph_data.pr$proj1
    d1 <- get.data.frame(graph_data.pr)
    
    d <- merge(d, d1, by = c("from", "to"), all.x = TRUE)
    d$weight.x <- rowSums(d[,c("weight.x", "weight.y")], na.rm=TRUE)
    d_int_d1 <- d[complete.cases(d$weight.y), c(1 , 2, 4)]
    d <- d[,-4]
    colnames(d)[3] <- "weight"
    d1 <- merge(d1, d_int_d1, by = c("from", "to"), all.x = TRUE)
    d1 <- d1[is.na(d1$weight.y), -4]
    colnames(d1)[1:2] <- c("to", "from")
    d <- rbind(d, d1)
    d <- d %>% group_by(from, to) %>% summarise(weight = sum(weight))
    
  }
  return(list(d = d, freq = freq))

}  

compute_deg <- function(d){
  
  graph <- graph_from_data_frame(d, directed = FALSE)
  deg <- degree(graph)
  deg <- data.frame(table(deg))
  deg <- deg %>% mutate(deg = as.numeric(as.character(deg)))
  deg_1 <- data.frame(deg = 1:max(deg$deg))
  deg <- merge(deg_1,deg, by = "deg", all.x = TRUE)
  deg[is.na(deg)] <- 0
  
  return(deg)
  
}
