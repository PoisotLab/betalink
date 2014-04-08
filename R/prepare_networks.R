#' @title Prepare networks
#' @description
#' Taking a list of networks as matrices, returns a list of igraph objects
#' 
#' @param w A list of network matrices
#' @param mode the mode of interaction (see \code{?graph.adjacency})
#' 
#' @export
#' @examples
#' data(clownfishes)
#' networks <- prepare_networks(clownfishes, "undirected")
#' networks[[1]]
prepare_networks <- function(w, mode = "directed")
{
   if(is.null(names(w))) warning("It is recommended to give names to your networks")
   interactions_df <- llply(w, df_from_A)
   return(llply(interactions_df, function(x) graph.adjacency(x, mode = mode)))
}

#' @title data.frame from adjancency matrix
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

