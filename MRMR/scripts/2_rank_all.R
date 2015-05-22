

optimsame = function(x){
	o = optimize(lksame, interval = c(-0.999, 0.999), d = x, maximum = T)
	return(o$objective)
}

lksame = function(rho, d){
	z1 = d[1]
	z2 =d[2]
	se1 = d[3]
	se2 = d[4]
	z_rho = 0.5* log ( (1+rho)/ (1-rho))
	l1 = dnorm(z1, mean = z_rho, sd = se1, log = T)
	l2 = dnorm(z2, mean = z_rho, sd = se2, log = T)
	return(l1+l2)
}


d = read.table("all_rho", as.is = T, head = T)




d$LLK_1_cause_2 = dnorm(0, mean = 0, sd = d$SE1, log = T) + dnorm(d$FZ2, mean = 0, sd = d$SE2, log = T)
d$LLK_2_cause_1 = dnorm(d$FZ1, mean = 0, sd = d$SE1, log = T) + dnorm(0 , mean = 0, sd = d$SE2, log = T)
d$LLK_nothing = dnorm(d$FZ1, mean = 0, sd = d$SE1, log = T) + dnorm(d$FZ2, mean = 0, sd = d$SE2, log = T)
d$LLK_same = apply(d[,c("FZ1", "FZ2", "SE1", "SE2")], 1, FUN = optimsame)



d$bestcause = apply(d[,c("LLK_1_cause_2", "LLK_2_cause_1")], 1, max)
d$ncause = apply(d[,c("LLK_nothing", "LLK_same")], 1, max)
d$ratio = d$bestcause -d$ncause



d$AIC_1_cause_2 = 2- 2*d$LLK_1_cause_2
d$AIC_2_cause_1 = 2- 2*d$LLK_2_cause_1
d$AIC_nothing = -2*d$LLK_nothing
d$AIC_same = 2- 2*d$LLK_same

d$bestcause = apply(d[,c("AIC_1_cause_2", "AIC_2_cause_1")], 1, min)
d$ncause = apply(d[,c("AIC_nothing", "AIC_same")], 1, min)
d$ratio =exp( (d$bestcause-d$ncause)/2)
d = d[order(d$ratio),]
write.table(d, file = "all_ranked", quote = F, row.names = F)





