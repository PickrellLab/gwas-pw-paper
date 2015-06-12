
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
#	print(leftout)
        m = mean(leftout)
        v = sum( (leftout -m)^2)
        v = v*( (nrow(d)-1) / nrow(d))
        se = sqrt(v)
        return(list (mean = m, se = se, rho = cc))
}

