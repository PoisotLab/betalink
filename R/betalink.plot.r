betalink.plot = function(m1,m2,by.unique=TRUE){
	up.1 = rownames(m1)
	up.2 = rownames(m2)
	lo.1 = colnames(m1)
	lo.2 = colnames(m2)
	up.shared = up.1[up.1%in%up.2]
	lo.shared = lo.1[lo.1%in%lo.2]
	up.un.1 = up.1[!(up.1%in%up.shared)]
	up.un.2 = up.2[!(up.2%in%up.shared)]
	lo.un.1 = lo.1[!(lo.1%in%lo.shared)]
	lo.un.2 = lo.2[!(lo.2%in%lo.shared)]
	m = aggregate.metaweb(list(m1,m2))$web
	if(by.unique){
		oCol = rep(0,ncol(m))
		oCol[colnames(m)%in%lo.un.1] = c(1:length(lo.un.1))
		oCol[colnames(m)%in%lo.shared] = max(oCol)+c(1:length(lo.shared))
		oCol[colnames(m)%in%lo.un.2] = max(oCol)+c(1:length(lo.un.2))
		oRow = rep(0,nrow(m))
		oRow[rownames(m)%in%up.un.1] = c(1:length(up.un.1))
		oRow[rownames(m)%in%up.shared] = max(oRow)+c(1:length(up.shared))
		oRow[rownames(m)%in%up.un.2] = max(oRow)+c(1:length(up.un.2))
	} else {
		oCol = ncol(m)-rank(colSums(m),ties.method='r')+1
		oRow = nrow(m)-rank(rowSums(m),ties.method='r')+1
	}
	plot(0, pch=NA, xlim=c(-3,nrow(m)), ylim=c(-3,ncol(m)), asp=1, ylab='',xlab='', yaxt='n', xaxt='n', bty='n')
	text(y=-0.4,x=oRow+0.5,rownames(m),pos=2,srt=90,cex=0.75)
	text(x=-0.4,y=oCol-0.5,colnames(m),pos=2,cex=0.75)
	for(ro in 1:nrow(m)){
		current.up = rownames(m)[ro]
		for(co in 1:ncol(m)){
			current.low = colnames(m)[co]
			c.BG = 'lightgrey'
			if((current.up%in%up.un.1) | (current.low%in%lo.un.1)){c.BG = 'palegreen'}
			if((current.up%in%up.un.2) | (current.low%in%lo.un.2)){c.BG = 'skyblue'}
			if((current.up%in%up.shared) & (current.low%in%lo.shared)){
				if(m1[current.up,current.low]!=m2[current.up,current.low]){
					c.BG = 'orange'
				}
			}
			if(m[current.up,current.low] == 0){c.BG = 'white'}
			rect(oRow[ro]-0.9,oCol[co]-0.9,oRow[ro]-0.1,oCol[co]-0.1,col=c.BG,border=NA)
		}
	}
}
