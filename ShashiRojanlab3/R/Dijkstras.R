dijkstra <- function(graph, init_node) {
  
  # check for input integrity
  if (!is.data.frame(graph) || !is.numeric(init_node)) {
    stop("The Graph must be a data frame and Init Node must be an numeric scalar")
  } else if(!all(tolower(sort(colnames(graph))) == c("v1","v2", "w"))) {
    stop("Structure the Graph Input to have 3 columns: v1, v2 and w")
  } else if (!all(sapply(colnames(graph), function(x) {is.numeric(graph[, x])}))) {
    stop("Graph Input must be numeric")
  } else if (!(init_node %in% graph$v1)) {
    stop("The Initial Node must be on the graph!")
  }
  
  # initialise variables
  all_nodes <- unique(graph$v1)
  dist <- vector(mode="numeric", length=length(all_nodes))
  names(dist) <- all_nodes
  temp_vertex_set <- all_nodes
  prev <- vector()

  for (node in all_nodes) {
    dist[node] <- Inf
    prev[node] <- "UNDEFINED"
  }
  
  dist[init_node] <- 0

  # search for shortest path
  while(length(temp_vertex_set) > 0) {
    dist_temp_vertex_set <- dist[temp_vertex_set]
    u <- names(dist_temp_vertex_set[dist_temp_vertex_set==min(dist_temp_vertex_set)][1]) # set 'u' to vertex amongst temp_vertex_set with the least distance
    temp_vertex_set <- temp_vertex_set[temp_vertex_set != u] # remove 'u' from temp_vertex_set

    for (v in graph[graph$v1 == u, 2]) { # graph[graph$v1 == u, 2] is the set of all neighbours of node 'u'
      alt <- dist[u] + df[df$v1 == u & df$v2 == v, 3]
      if (alt < dist[v]) {
        dist[v] <- alt
        prev[v] <- u
      }
    }
  }

  return(unname(dist))
  
}