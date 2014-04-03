#' @title Returns a metaweb given a list of networks
#' @description
#' Given a list of networks, this function returns the metaweb, and informations related to co-occurrence
#'
#' @param W a \code{list} of matrices
#' @export
metaweb <- function(W){
	Lo <- unique(unlist(lapply(W,colnames)))
	Up <- unique(unlist(lapply(W,rownames)))
	meta <- matrix(0,ncol=length(Lo),nrow=length(Up))
	colnames(meta) <- Lo
	rownames(meta) <- Up
	cooc <- meta
	for(w in W){
		w[w>0] <- 1
		meta[rownames(w),colnames(w)] <- meta[rownames(w),colnames(w)] + w
		cooc[rownames(w),colnames(w)] <- cooc[rownames(w),colnames(w)] + 1
	}
   adjacency <- meta
	adjacency[adjacency>0] <- 1
	return(list(
               web = adjacency,
               n_cooc = cooc,
               n_int = meta
               ))
}
