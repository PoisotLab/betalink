#' @title Plot a pair of network to highlight their differences
network_betaplot <- function(n1, n2, na='skyblue', nb='palegreen', ns='lightgrey', ...){
   M <- metaweb(list(n1,n2))
   s_a <- unique(c(colnames(A),rownames(A)))
   s_b <- unique(c(colnames(B),rownames(B)))
   ## VERTICES
   vert_color <- rep(ns, length(V(M)))
   names(vert_color) <- V(M)$name
   for(v in V(G)$name)
   {
      if ((v %in% s_a) && !(v %in% s_b)) vert_color[v] = na
      if (!(v %in% s_a) && (v %in% s_b)) vert_color[v] = nb
   }
   ##
   plot(G, vertex.color = vert_color, ...)
}
