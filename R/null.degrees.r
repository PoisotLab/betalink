null.degrees = function(template,reps)
{
	template[template>0] <- 1
	## Get proba
	margin <- ifelse(ncol(template)<nrow(template),2,1)
	currentweb <- matrix(0,ncol=ncol(template),nrow=nrow(template))
	pc <- colMeans(template)
	pr <- rowMeans(template)
	if(margin==2)
	{
		for(i in 1:ncol(template))
		{
			currentweb[,i] <- (pc[i]+pr)/2
		}
	} else {
		for(i in 1:nrow(template))
		{
			currentweb[i,] <- (pr[i]+pc)/2
		}
	}
	#
	LNulls = NULL
	n = 0
	while(n < reps)
	{
		Void = TRUE
		while(Void)
		{
			tNull = apply(currentweb,margin,function(x)rbinom(length(x),1,x))
		if ((sum(colSums(tNull) == 0)+sum(rowSums(tNull) == 0)) == 0) Void = FALSE
		}
		n = n+1
		colnames(tNull) = colnames(template)
		rownames(tNull) = rownames(template)
		LNulls[[n]] = tNull
	}
	
	return(LNulls)	
}