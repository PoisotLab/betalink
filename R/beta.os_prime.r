beta.os_prime = function(W,...){
	metaweb = metaweb(W)$web
	os_prime = NULL
	for(w in W){
		partitions = betalink(w,metaweb[rownames(w),colnames(w)],...)
		os_prime = c(os_prime,partitions$OS)
	}
	return(os_prime)
}
