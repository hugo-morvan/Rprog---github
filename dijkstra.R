dijkstra <- function(graph, init.node){
    #graph is a data.frame with columns v1, v2, w
    #init.node is the starting node
    #returns the shortest path from init.node to all other nodes

    #pseudo code (source = https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)

 dist <- c()
    prev <- c()
    Q <- c()
    
    for (i in 1:nrow(graph)){
        dist[i] <- Inf
        prev[i] <- NA
        Q[i] <- i
    }
    dist[init.node] <- 0
    current <- init.node

    while( length(Q) > 0){
        #find the node in Q with the smallest dist
        min.dist <- Inf
        for (i in 1:length(Q)){
            if (dist[Q[i]] < min.dist){
                min.dist <- dist[Q[i]]
                current <- Q[i]
            }
        }
        #remove current from Q
        Q <- Q[Q != current]
        #for each neighbor of current
        for (i in 1:nrow(graph)){
            if (graph[i,1] == current){
                neighbor <- graph[i,2]
                alt <- dist[current] + graph[i,3]
                if (alt < dist[neighbor]){
                    dist[neighbor] <- alt
                    prev[neighbor] <- current
                }
            }
        }
    }
return (list(dist=dist, prev=prev))
}

wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)
