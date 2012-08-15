betalink = function(w1,w2,bf=B01){
	pmb = function(A,B) list(b=sum(!(A %in% B)),c=sum(!(B %in% A)),a=sum(B %in% A))
	sp1 = list(top=rownames(w1),bottom=colnames(w1),all=unique(c(colnames(w1),rownames(w1))))
	sp2 = list(top=rownames(w2),bottom=colnames(w2),all=unique(c(colnames(w2),rownames(w2))))
	beta_U = bf(pmb(sp1$top,sp2$top))
	beta_L = bf(pmb(sp1$bottom,sp2$bottom))
	beta_S = bf(pmb(sp1$all,sp2$all))
	# Common species
	Csp = sp1$all[sp1$all %in% sp2$all]
	CUsp = sp1$top[sp1$top %in% sp2$top]
	CLsp = sp1$bottom[sp1$bottom %in% sp2$bottom]
	if((length(CUsp)>0) & (length(CLsp)>0))
	{
		w1Con = w1[CUsp,CLsp]
		w2Con = w2[CUsp,CLsp]
		nCon = sum((w1Con == w2Con) & (w1Con == 1))
		pmBos = list(b=sum(w1Con)-nCon,c=sum(w2Con)-nCon,a=nCon)
		pmBwn = list(b=sum(w1)-nCon,c=sum(w2)-nCon,a=nCon)
		beta_OS = bf(pmBos)
		beta_WN = bf(pmBwn)
		if(is.na(beta_OS)) beta_OS = 0
		if(is.na(beta_WN)) beta_WN = 0
		beta_ST = beta_WN - beta_OS
		if(beta_WN > 0){
			b_contrib = beta_ST / beta_WN
		} else {
			b_contrib = 0
		}
	} else {
		beta_WN = 0
		beta_OS = 0
		beta_ST = 0
		b_contrib = 0
	}
	
	return(list(U = beta_U, L = beta_L, S = beta_S, OS = beta_OS, WN = beta_WN, ST = beta_ST, contrib = b_contrib))
}
