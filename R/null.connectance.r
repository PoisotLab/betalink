null.connectance = function(template,reps)
{
	template[template>0] <- 1
	LNulls = NULL
	n = 0
	while(n < reps)
	{
		Void = TRUE
		while(Void)
		{
			tNull = matrix(sample(template),nrow=nrow(template))
		if ((sum(colSums(tNull) == 0)+sum(rowSums(tNull) == 0)) == 0) Void = FALSE
		}
		n = n+1
		colnames(tNull) = colnames(template)
		rownames(tNull) = rownames(template)
		LNulls[[n]] = tNull
	}
	return(LNulls)	
}