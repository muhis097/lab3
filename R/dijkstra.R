#' 1.1.2 Dijkstra algorithm implementation
#'
#' This function calculates shortest distance from an initial node to all other nodes in a given data frame.
#'
#' @param graph = This takes a data frame as a parameter which includes three variable named v1, v2, w.
#'  Here v1 and v2 contains edges and w contains weighted distance from v1 to v2 edges.
#'
#' @param init_node = It takes a numeric integer value from where we will calculate the shortest distance to every other nodes in our given graph.
#'
#' @return  The function returns a vector with the shortest distances from init_node.
#'
#' @export
#'
#' @examples
#' wiki_graph <-  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#'
#'@references \url{en.wikipedia.org/wiki/Dijkstra_algorithm}


dijkstra<- function(graph, init_node){
  if(ncol(graph) != 3)stop()
  if(any(colnames(graph) != c("v1", "v2", "w")))stop()
  if(!any(unique(graph[['v1']]==init_node)))stop()
  if(!is.data.frame(graph) | !is.numeric(init_node))stop()

  dist=c()
  not_visited=c()
  q=unique(graph$v1)

  for (i in q) {
    if(i==init_node){
      dist[i]=0
      not_visited[i]=q[i]
    }else{
      dist[i]=Inf
      not_visited[i]=q[i]
    }
  }
  temp=data.frame(nodes=q, shortest_distance=dist, visited= rep(FALSE, length(q)))


  while(length(not_visited)!=0){

    w= min(temp[temp$visited==FALSE, ]$shortest_distance)
    u= temp[temp$shortest_distance==w, ]$nodes[1]
    temp[temp$nodes==u, ]$visited <- TRUE


    for (v in graph[graph$v1==u,]$v2) {

      alt=temp[which(temp$nodes==u), ]$shortest_distance+ graph[graph$v1==u & graph$v2==v, ]$w

      if(alt<temp[which(temp$nodes==v), ]$shortest_distance){
        temp[which(temp$nodes==v), ]$shortest_distance=alt

      }
    }
    not_visited=not_visited[-which(not_visited==u)]
  }

  return(temp$shortest_distance)
}


