dijkstra <- function(graph, init.node){
    #graph is a data.frame with columns v1, v2, w
    #init.node is the starting node
    #returns the shortest path from init.node to all other nodes

    #pseudo code (source = https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)

    #1 Mark all nodes unvisited. Create a set of all the unvisited nodes called the unvisited set.

    dist <- c()
    prev <- c()
    Q <- c()
    
    #2 Assign to every node a tentative distance value: 
    #  set it to zero for our initial node and to infinity for all other nodes. 
    # Set the initial node as current.

    for (i in 1:nrow(graph)){
        dist[i] <- Inf
        prev[i] <- NA
        Q[i] <- i
    }
    dist[init.node] <- 0
    current <- init.node

    #3 For the current node, consider all of its unvisited neighbors and calculate their tentative distances.
    # Compare the newly calculated tentative distance to the current assigned value and assign the smaller one.
    # Otherwise the current assigned value will be kept.


    #4 When we are done considering all of the unvisited neighbors of the current node,
    # mark the current node as visited and remove it from the unvisited set.
    # A visited node will never be checked again.



    #5 If the destination node has been marked visited (when planning a route between two specific nodes)
    # or if the smallest tentative distance among the nodes in the unvisited set is infinity (when planning
    # a complete traversal; occurs when there is no connection between the initial node and remaining unvisited nodes),
    # then stop. The algorithm has finished.



    #6 Otherwise, select the unvisited node that is marked with the smallest tentative distance,
    # set it as the new "current node", and go back to step 3.


}

wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)