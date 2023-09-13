#' Dijkstra's Algorithm
#'
#' @param graph a data frame with columns v1 (from), v2 (to), w (weight)
#' @param init.node the starting node
#' @return the shortest path from init.node to all other nodes
#' @export
#' @examples
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'                        v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'                       w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)


dijkstra <- function(graph, init.node) {

    #pseudo code (source = https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)

    #Checking input
    if (nrow(graph) == 0) {
        stop("The graph is empty")
    }
    if (init.node > nrow(graph)){
        stop("The initial node is not in the graph")
    }

    dist <- c()
    prev <- c()
    my_queue <- c()

    #initialize dist and prev
    for (i in 1:nrow(graph)){
        dist[i] <- Inf
        prev[i] <- NA
        my_queue[i] <- i
    }
    #Set the distance to zero for the initial node
    dist[init.node] <- 0
    #Set the current node to the initial node
    current <- init.node

    while(length(my_queue) > 0) {
        #find the node in the queue with the smallest dist
        min_dist <- Inf
        for (i in 1:length(my_queue)){
            if (dist[my_queue[i]] < min_dist) {
                min_dist <- dist[my_queue[i]]
                current <- my_queue[i]
            }
        }
        #remove current from Q
        my_queue <- my_queue[my_queue != current]
        #for each neighbor of current
        for (i in 1:nrow(graph)) {
            if (graph[i, 1] == current) {
                neighbor <- graph[i, 2]
                alt <- dist[current] + graph[i, 3]
                if (alt < dist[neighbor]) {
                    dist[neighbor] <- alt
                    prev[neighbor] <- current
                }
            }
        }
    }
    #dist is a vector of the shortest distance from the init node to each node
    #ie dist[i] is the shortest distance from the initial node to node i
    return(list(dist = dist))
}

wiki_graph <-
data.frame(v1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6),
           v2 = c(2, 3, 6, 1, 3, 4, 1, 2, 4, 6, 2, 3, 5, 4, 6, 1, 3, 5),
            w = c(7, 9, 14, 7, 10, 15, 9, 10, 11, 2, 15, 11, 6, 6, 9, 14, 2, 9))

print(dijkstra(wiki_graph, 1))
