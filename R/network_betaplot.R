#' @title Plot a pair of network to highlight their differences
network_betaplot <- function(A, B, na='skyblue', nb='palegreen', ns='lightgrey', ...){
   M <- metaweb(list(A,B))$web
   s_a <- unique(c(colnames(A),rownames(A)))
   s_b <- unique(c(colnames(B),rownames(B)))
   G <- graph.adjacency(M)
   ## VERTICES
   vert_color <- rep(ns, length(V(G)))
   names(vert_color) <- V(G)$name
   for(v in V(G)$name)
   {
      if ((v %in% s_a) && !(v %in% s_b)) vert_color[v] = na
      if (!(v %in% s_a) && (v %in% s_b)) vert_color[v] = nb
   }
   ##
   plot(G, vertex.color = vert_color, ...)
}
