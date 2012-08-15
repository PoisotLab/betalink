generate.metaweb = function(U,L,k){
	Nzeroes = U*L - k
	ZDS = TRUE
	while(ZDS == TRUE){
		Adj = sample(c(rep(0,Nzeroes),rep(1,k)),replace=FALSE)
		W = matrix(Adj,nrow=U)
		ZDS = sum(colSums(W) == 0)+sum(rowSums(W) == 0) > 0
	}
	colnames(W) = paste('l',c(1:ncol(W)), sep='')
	rownames(W) = paste('u',c(1:nrow(W)), sep='')
	return(W)
}