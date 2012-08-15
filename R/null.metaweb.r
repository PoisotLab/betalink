null.metaweb = function(template,reps)
{
	LNulls = NULL
	n = 0
	while(n < reps)
	{
		Void = TRUE
		while(Void)
		{
			tNull = matrix(rbinom(prod(dim(template)),1,template),ncol=ncol(template),byrow=TRUE)
			if ((sum(colSums(tNull) == 0)+sum(rowSums(tNull) == 0)) == 0) Void = FALSE
		}
		n = n+1
		colnames(tNull) = colnames(template)
		rownames(tNull) = rownames(template)
		LNulls[[n]] = tNull
	}
	return(LNulls)
}