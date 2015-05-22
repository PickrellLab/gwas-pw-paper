library(mvtnorm)

a = commandArgs(T)

infile = a[1]
outfile = a[2]
modelp = c(as.numeric(a[3]), as.numeric(a[4]), as.numeric(a[5]), as.numeric(a[6]))
c = as.numeric(a[7])


d = read.table(infile, as.is = T, head = T)

model = vector(length = ncol(d))
for (i in 5:length(model)){
	ru  = runif(1)
	if (ru < modelp[1]){
		model[i] = 1
	} 
	else if (ru < (modelp[2]+modelp[1])){
			model[i] = 2
	}
	else if (ru < (modelp[1]+modelp[2]+modelp[2])){
			model[i] = 3
	}else{
			model[i] = 4
	}
}

print(model)
toprint = data.frame(matrix(nrow = nrow(d), ncol = 6))
for (i in 1:nrow(d)){
	#print(i)
	toprint[i,1] = d[i,1]
	toprint[i,2] = d[i,2]
	m1 = 0
	m2 = 0
	CM = matrix(nrow = 2, ncol = 2)
	CM[1,] = 0
	CM[2,] = 0
	CM[1,2] = c*0.1
	CM[2,1] = c*0.1
	CM[1,1] = 0.1
	CM[2,2] = 0.1
	# effect on phenotype 1 depends on counts of the causal genotype 1 if models are 1, 3, or 4
	tosum1 = d[i,which(model == 1 | model == 3 | model == 4)]
	# effect on phenotype 2 depends on counts of causal genotype 1 if models are 2 or 3
	tosum2 = d[i,which(model ==2 | model ==3)]
	# effect on phenotype 2 depends on counts of causal genotype 2 if model is 4
	tosum3 = d[i,which(model == 4),]
	c1 = c(10, 11, 12)
	c2 = c(20, 21, 22)
	c01 = c(1, 11, 21)
	c02 = c(2, 12, 22)
	sum1 = sum(tosum1 %in% c1)+ 2*sum(tosum1 %in% c2) 
	sum2 = sum(tosum2 %in% c1)+ 2*sum(tosum2 %in% c2)+ sum(tosum3 %in% c01)+ 2*sum(tosum3 %in% c02)
	
	#print(paste(sum1, sum2))
	m1 = sum1*0.3
	m2 = sum2*0.3
	p = rmvnorm(1, mean = c(m1, m2), sigma = CM)
	if (sum (modelp) < 0.001){
		p =  rmvnorm(1, mean = c(0, 0), sigma = CM)
	}
	toprint[i,3] = p[1]
	toprint[i,4] = p[2]
	toprint[i,5] = sum1
	toprint[i,6] = sum2
}
toprint[,3] = qqnorm(toprint[,3], plot = F)$x
toprint[,4] = qqnorm(toprint[,4], plot = F)$x

write.table(toprint, file = outfile, quote = F, row.names = F, col.names = F)

toprint2 = data.frame(matrix(nrow = length(model)-4, ncol = 3))
toprint2[,1] = names(d)[5:ncol(d)]
toprint2[,2] = 0:(length(model)-5)
toprint2[,3] = model[5:length(model)]

write.table(toprint2, file = paste0(outfile, ".model"), quote = F, row.names = F, col.names = F)
