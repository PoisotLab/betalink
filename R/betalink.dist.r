#' @title Components of beta-diversity for a list of networks
#' @description
#' Given a list of networks, returns the pairwise beta-diversity components
#' @export
network_betadiversity <- function(W, ...){
	dWN <- matrix(NA,ncol=length(W),nrow=length(W))
	colnames(dWN) <- names(W)
	rownames(dWN) <- names(W)
	dOS <- dWN
	dS <- dWN
	dST <- dWN
	dContrib <- dWN
	for(i in c(1:(length(W)-1))){
		for(j in c((i+1):(length(W)))){
			partition <- betalink(W[[i]], W[[j]], ...)
			dWN[j,i] <- partition$WN
			dOS[j,i] <- partition$OS
			dS[j,i] <- partition$S
			dST[j,i] <- partition$ST
			dContrib[j,i] <- partition$contrib
		}
	}
	return(llply(list(WN=dWN, OS=dOS, S=dS, ST=dST, contrib=dContrib), as.dist))
}
