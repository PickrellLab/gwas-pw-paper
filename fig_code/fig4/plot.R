pdf("plot_betas.pdf")


d = read.table("all_bffiles", as.is = T)
s = d[,1]

toplot = data.frame(matrix(nrow = length(s), ncol = length(s)))
names(toplot) = s
rownames(toplot) = s

for (i in 1:(length(s)-1)){
	for(j in (i+1):length(s)){
		tmp = paste("../../overlaps/data/", s[i], "_", s[j], ".overlap_wbetas", sep = "")
		tmp = read.table(tmp, as.is = T, head = T)
		mhc =which(tmp$chr == "chr6" & tmp$pos >= 26000000 & tmp$pos <=34000000)

                if(length(mhc)>0){
                        tmp = tmp[-mhc,]
                }
		b1 = tmp$Z_1*sqrt(tmp$V_1)
		b2 = tmp$Z*sqrt(tmp$V)
		ct  = cor.test(b1, b2, method = "sp")
		p = ct$p.value
		if (p < 1e-16){ p = 1e-16}
		rho = ct$estimate
		Z = qnorm(p/2)
		if (rho > 0){ Z = -Z}
		l = summary(lm(b1 ~ b2))
		
		e = l$coef[2,1]
		sd = l$coef[2,2]

		print(paste(s[i], s[j], p, rho, Z, cor(b1, b2)))
		toplot[i,j] = Z

		tmp = paste("../../overlaps/data/", s[j], "_", s[i], ".overlap_wbetas", sep = "")
                tmp = read.table(tmp, as.is = T, head = T)
                mhc =which(tmp$chr == "chr6" & tmp$pos >= 26000000 & tmp$pos <=34000000)

                if(length(mhc)>0){
                        tmp = tmp[-mhc,]
                }
		b1 = tmp$Z_1*sqrt(tmp$V_1)
                b2 = tmp$Z*sqrt(tmp$V)
           	ct  = cor.test(b1, b2, method = "sp")
                p = ct$p.value
		if (p < 1e-16){p = 1e-16}
                rho = ct$estimate
                Z = qnorm(p/2)
                if (rho > 0){ Z = -Z}                
		l = summary(lm(b1 ~ b2))
                
		e = l$coef[2,1]
                sd = l$coef[2,2]
		print(paste(s[j], s[i], p, rho, Z, cor(b1, b2)))
		toplot[j,i] = Z

	}
}
toplot[is.na(toplot)] = 10
write.table(toplot, file = "tmp")
dev.off()
pdf("heatmap_betas.pdf")

l = matrix(nrow = 2, ncol = 2)
l[1,1] = 1
l[2,1] = 1
l[1,2] = 2
l[2,2] = 3
layout(l, widths = c(10,1))

orders = c("AVD_23", "AAM", "AAM_23",  "CAD", "TG", "HDL", "TC", "LDL", "HB", "MCV", "RBC", "MCHC", "MPV", "PLT", "FNBMD", "LSBMD", "TS_23", "CEI_23", "ALL_23", "ATH_23", "HTHY_23", "CD","RA", "T2D", "FG", "BMI_2015", "CUP_23", "WHR", "MPB_23", "UB_23", "DIMP_23", "NOSE_23", "HEIGHT", "BHM_23", "NST_23", "AD", "MIGR_23", "PS_23", "SCZ", "PD_23")

plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = F, xlab = "", ylab = "")
mtext("Effect size correlations across all pairs of traits", adj = 0)
for (i in 1:(length(orders))){
	n = orders[i]
	if (n == "BMI_2015") { n = "BMI"}
	if (n == "AAM_23") { n = "AAM (23)"}
	n = gsub("_23", "", n)
	mtext(n, las = 2, side = 1, at = (i-1)*(1/nrow(toplot)) + 0.5/nrow(toplot), cex = 0.6)
	mtext(n, las = 2, side = 2, at = 1-(i-1)*(1/nrow(toplot)) - 0.5/nrow(toplot), cex = 0.6)
        for(j in (i):length(orders)){
		x1 = (i-1)*(1/nrow(toplot))
		x2 = x1+ 1/nrow(toplot)
		y1 = 1-((j-1)*(1/nrow(toplot)))
		y2 = y1 -1/nrow(toplot)
		print(paste(orders[i], orders[j]))
		v = toplot[ which(rownames(toplot) == orders[j]), which(rownames(toplot) == orders[i]) ]
		tc = "white"
		P = pnorm(-abs(v))/2
		logP = -log10(P)
		if (logP > 10){ logP = 10}
		print(paste(v, P))
		if ( v > 2.57){
			tc = rgb(255, 305 - logP*25, 305-logP*25, maxColorValue = 255)
		}
		if ( v < -2.57){
			tc = rgb(305 - logP*25, 305-logP*25, 255, maxColorValue = 255)
		}
		#if (v < -3.29) {tc = "lightblue"}
		#if (v < -4.42) {tc = "darkblue"}
		#if (v>  3.29){ tc = "pink"}
		#if (v > 4.42){tc = "red"}
		if (i == j){ tc = "lightgrey"}
		rect(x1, y2, x2, y1, col = tc, border = "lightgrey")
	
		x1 = (j-1)*(1/nrow(toplot))
                x2 = x1 + 1/nrow(toplot)
                y1 =  1-((i-1)*(1/nrow(toplot)))
                y2 = y1 - 1/nrow(toplot)
		print(paste(orders[j], orders[i]))
                v = toplot[ which(rownames(toplot) == orders[i]), which(rownames(toplot) == orders[j]) ]
                print(v)
		tc = "white"
		P = pnorm(-abs(v))/2
                logP = -log10(P)
		if (logP >10) {logP  = 10}
                print(paste(v, P))
		if ( v >2.57){
                        tc = rgb(255, 305 - logP*25, 305-logP*25, maxColorValue = 255)
                }
                if ( v < -2.57){
                        tc = rgb(305 - logP*25, 305-logP*25, 255, maxColorValue = 255)
                }
		if (i == j) { tc = "lightgrey"}
		rect(x1, y2, x2, y1, col = tc, border = "lightgrey")
	}
}

tp = seq(2, 10, 0.5)
cc = rgb(255, 305 - tp*25, 305-tp*25, maxColorValue = 255)

par(mar = c(0,0,0,0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = F, ylab = "", xlab = "")
for (i in 1:length(cc)){
        x1 = 0
        x2 = 0.5
        h = 0.3/length(cc)
        y1 = 0.35+((i-1)*h)
        y2 = 0.35+(i*h)
        rect(x1, y1, x2, y2, col = cc[i], border = cc[i])

}

axis(2, at = c(0.35, 0.5, 0.65), lab = c("0.01", "1e-5", "1e-10"), cex = 0.8)
text(0.25, 0.75, lab = "P", cex = 0.75)
text(0.25, 0.7, lab = expression("["*rho*" > 0]"), cex = 0.75)
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = F, ylab = "", xlab = "")
tp = seq(2, 10, 0.5)
cc = rgb(305 - tp*25, 305-tp*25, 255, maxColorValue = 255)

for (i in 1:length(cc)){
        x1 = 0
        x2 = 0.5
        h = 0.3/length(cc)
        y1 = 0.35+((i-1)*h)
        y2 = 0.35+(i*h)
        rect(x1, y1, x2, y2, col = cc[i], border = cc[i])

}
axis(2, at = c(0.35, 0.5, 0.65), lab = c("0.01", "1e-5", "1e-10"), cex = 0.8)

text(0.25, 0.75, lab = "P", cex = 0.75)
text(0.25, 0.7, lab = expression("["*rho*" < 0]"), cex = 0.75)
dev.off()
