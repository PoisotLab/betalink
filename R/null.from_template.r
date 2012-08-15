null.from_template = function(w,template,reps){
	temp = template[rownames(w),colnames(w)]
	temp = temp / max(temp)
	nulls_mw = null.metaweb(temp,reps)
	nulls_de = null.degrees(temp,reps)
	nulls_co = null.connectance(temp,reps)
	return(I = nulls_co, II = nulls_de, MW = nulls_mw)
}