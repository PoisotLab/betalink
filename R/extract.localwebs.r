extract.localwebs = function(template,u,l,i){
	ZDS = TRUE
	while(ZDS){
		w1 = template[sample(c(1:nrow(template)),u),sample(c(1:ncol(template)),l)]
		w2 = template[sample(c(1:nrow(template)),u),sample(c(1:ncol(template)),l)]
		## Link inconsistency
		for(uu in 1:u) for(ll in 1:l){
			if(w1[uu,ll] == 1){
				if(rbinom(1,1,i) == 0){
					if(runif(1,0,1)<0.5){
						w1[uu,ll] = 0
					} else {
						w2[uu,ll] = 0
					}
				}
			}
		}
		ZDS = sum(colSums(w1) == 0)+sum(rowSums(w1) == 0)+sum(colSums(w2) == 0)+sum(rowSums(w2) == 0) > 0
	}
	return(list(m1=w1,m2=w2))
}