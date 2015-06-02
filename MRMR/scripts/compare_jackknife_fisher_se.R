

d = read.table("all_bffiles", as.is = T)
npheno = nrow(d)
npair = npheno*(npheno-1)/2


jackknife_fisherZ_se = function(d){
	leftout = vector(length = nrow(d))
	cc = cor.test(d$B1, d$B2, method = "sp")
	cc = cc$estimate
	for (i in 1:nrow(d)){
		tmp = d[-i,]
		tmp$R1 = rank(tmp$B1)
		tmp$R2 = rank(tmp$B2)
		c = cor.test(tmp$R1, tmp$R2)
		e = c$estimate
                z = 0.5*log ( (1+e)/(1-e))
		leftout[i] = z
	}
	m = mean(leftout)
	v = sum( (leftout -m)^2)
	v = v*( (nrow(d)-1) / nrow(d))
	se = sqrt(v)
	return(list (mean = m, se = se, rho = cc)) 
}

toreturn = data.frame(matrix(nrow = npair, ncol = 12))
index = 1
for (i in 1:(npheno-1)){
	p1 = d[i,1]
	for (j in (i+1):(npheno)){
		print(paste(i, j))
		p2 =  d[j,1]
		print(paste(p1, p2))
		toreturn[index,1] = p1
		toreturn[index,2] = p2
		f1 = paste0("../../overlaps/data/", p1, "_", p2, ".overlap_wbetas")
		tmp = read.table(f1, as.is = T, head = T)
		mhc =which(tmp$chr == "chr6" & tmp$pos >= 26000000 & tmp$pos <=34000000)
		
		if(length(mhc)>0){
			tmp = tmp[-mhc,]
		}
		tmp$B1 = tmp$Z_1*sqrt(tmp$V_1)
		tmp$B2 = tmp$Z*sqrt(tmp$V)
		fZse1 = jackknife_fisherZ_se(tmp)
		#tmp$R1 = rank(tmp$B1)
		#tmp$R2 = rank(tmp$B2)
		N1 = nrow(tmp)
		#c1 = cor.test(tmp$R1, tmp$R2)
		e1 = fZse1$rho
		z1 = fZse1$mean
		se1 = fZse1$se
		#z1 = 0.5*log ( (1+e1)/(1-e1))
		fisherse1 = 1/( sqrt(nrow(tmp)-3))


                f2 = paste0("../../overlaps/data/", p2, "_", p1, ".overlap_wbetas")
                tmp = read.table(f2, as.is = T, head = T)

                mhc =which(tmp$chr == "chr6" & tmp$pos >= 26000000 & tmp$pos <=34000000)
                
              	if(length(mhc)>0){
                        tmp = tmp[-mhc,]
                }
                tmp$B1 = tmp$Z_1*sqrt(tmp$V_1)
                tmp$B2 = tmp$Z*sqrt(tmp$V)
		fZse2 = jackknife_fisherZ_se(tmp)
                #tmp$R1 = rank(tmp$B1)
                #tmp$R2 = rank(tmp$B2)
		N2 = nrow(tmp)
                #c2 = cor.test(tmp$R1, tmp$R2)
                e2 = fZse2$rho
		z2 = fZse2$mean
                se2 = fZse2$se
                #z2 = 0.5*log ( (1+e2)/(1-e2))
                fisherse2 = 1/( sqrt(nrow(tmp)-3))	
		toreturn[index,3]= N1
		toreturn[index,4] = N2
		toreturn[index,5] = e1
		toreturn[index,6] = e2
		toreturn[index,7] = z1
		toreturn[index,8] = z2
		toreturn[index,9] = se1
		toreturn[index,10] = se2
		toreturn[index,11] = fisherse1
		toreturn[index,12] = fisherse2
		index = index+1
	}
}

names(toreturn) = c("P1", "P2", "N1", "N2", "RHO1", "RHO2", "FZ1", "FZ2", "SE1", "SE2", "FISHERSE1", "FISHERSE2")
toreturn = toreturn[!is.na(toreturn[,5]),]
toreturn = toreturn[!is.na(toreturn[,6]),]
toreturn = toreturn[!is.na(toreturn[,7]),]
toreturn = toreturn[!is.na(toreturn[,8]),]
toreturn = toreturn[!is.na(toreturn[,9]),]
toreturn = toreturn[is.finite(toreturn$FZ2) & is.finite(toreturn$FZ1),]


write.table(toreturn, file = "all_compare_se", quote = F, row.names = F)
 






