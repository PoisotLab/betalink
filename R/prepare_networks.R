#' @title Prepare networks
#' @description
#' Taking a list of networks as matrices, returns a list of igraph objects
#'
#' @param w A list of network matrices
#' @param directed whether the edges are directed or not
#'
#' @export
#' @examples
#' data(clownfishes)
#' networks <- prepare_networks(clownfishes, TRUE)
#' print(V(networks$Timur))
prepare_networks <- function(w, directed = TRUE)
{
   if(is.null(names(w))) warning("It is recommended to give names to your networks")
   interactions_df <- plyr::llply(w, df_from_A)
   networks <- plyr::llply(interactions_df, function(x) igraph::graph.data.frame(x, directed = directed))
   class(networks) <- "econetwork"
   return(networks)
}

#' @title data.frame from adjancency matrix
#' @param A an adjacency matrix
df_from_A <- function(A)
{
   A[A>0] <- 1
   if(is.null(colnames(A))) stop("The input matrices must have named columns")
   if(is.null(rownames(A))) stop("The input matrices must have named rows")
   A_df <- NULL
   for(i in c(1:NROW(A)))
   {
      for(j in c(1:NCOL(A)))
      {
         if(A[i,j] == 1) A_df <- rbind(A_df, c(rownames(A)[i], colnames(A)[j]))
      }
   }
   return(A_df)
}
