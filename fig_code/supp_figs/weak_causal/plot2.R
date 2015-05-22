

pdf("causal_supp.pdf")
d = read.table("top_causal_01_05", as.is = T, head = T)

l = matrix(nrow = 3, ncol = 3)
l[1,1] = 1
l[2,1] = 1
l[3,1] = 1
l[1,2] = 2
l[1,3] = 3
l[2,2] = 4
l[2,3] = 5
l[3,2] = 6
l[3,3] = 7
layout(l, widths = c(0.9, 1,1))

plot(NA, axes = F, xlim = c(0, 1), ylim = c(0,1), xlab = "", ylab = "")
mtext("A. Putative causally-related traits", adj = 0, cex = 0.6)
h = 1/nrow(d)
for (i in 1:nrow(d)){
	p1 = d[i,]$P1
	p2 = d[i,]$P2
	y = 1-(h*i)
	p1 = gsub("AAM_23", "AAM (23)", p1)
	p2 = gsub("AAM_23", "AAM (23)", p2)

	p1 = gsub("_23", "", p1)
	p2 = gsub("_23", "", p2)
	
	p1 = gsub("_2015", "", p1)
	p2 = gsub("_2015", "", p2)

	rhoc = d[i,]$RHO1
	zc = d[i,]$FZ1
	znc = d[i,]$FZ2
	zcse = d[i,]$SE1
	znse = d[i,]$SE2
	if (d[i,]$AIC_2_cause_1 < d[i,]$AIC_1_cause_2){
		tmp = p1
		p1 = p2
		p2 = tmp
		rhoc = d[i,]$RHO2
		zc = d[i,]$FZ2
        	znc = d[i,]$FZ1
        	zcse = d[i,]$SE2
        	znse = d[i,]$SE1
	}
	col = "red"
	if (rhoc < 0){
		col = "blue"
	}
	arrows(0.42, y, 0.58, y, col = col, length = 0.08)
	text(0.39, y, adj = c(1, 0.5), lab = p1)
	text(0.61, y, adj = c(0, 0.5), lab = p2)
}
legend("topleft", legend = c("positive effect", "negative effect"), lty = 1, col = c("red", "blue"), bty = "n", cex = 0.8)

# plot AAM (23) HEIGHT

d = read.table("../../../overlaps/data/AAM_23_HEIGHT.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../../overlaps/data/HEIGHT_AAM_23.overlap_wbetas",  as.is = T, head = T)

mhc =which(d$chr == "chr6" & d$pos >= 26000000 & d$pos <=34000000)
if (length(mhc) >0){
	d = d[-mhc,]
}

mhc =which(d2$chr == "chr6" & d2$pos >= 26000000 & d2$pos <=34000000)
if (length(mhc) >0){
        d2 = d2[-mhc,]
}



d$B1 = d$Z_1*sqrt(d$V_1)
d$B2 = d$Z*sqrt(d$V)
d2$B1 = d2$Z_1*sqrt(d2$V_1)
d2$B2 = d2$Z*sqrt(d2$V)

plot(d$B1, d$B2, xlab = "Effect size on AAM (23)", ylab = "Effect size on HEIGHT", pch = 20, xlim = c(-0.10, 0.1), ylim = c(-0.05, 0.05))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("B. AAM v. HEIGHT (AAM ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on AAM (23)", ylab = "Effect size on HEIGHT", xlim = c(-0.08, 0.05), ylim = c(-0.1, 0.1))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("C. AAM v. HEIGHT (HEIGHT ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

# plot HB T2D
#d = read.table("../../../overlaps/data/BMI_ATH_23.overlap_wbetas", as.is = T, head = T)
#d2 = read.table("../../../overlaps/data/ATH_23_BMI.overlap_wbetas",  as.is = T, head = T)
d = read.table("../../../overlaps/data/HB_T2D.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../../overlaps/data/T2D_HB.overlap_wbetas",  as.is = T, head = T)


mhc =which(d$chr == "chr6" & d$pos >= 26000000 & d$pos <=34000000)
if (length(mhc) >0){
        d = d[-mhc,]
}

mhc =which(d2$chr == "chr6" & d2$pos >= 26000000 & d2$pos <=34000000)
if (length(mhc) >0){
        d2 = d2[-mhc,]
}

d$B1 = d$Z_1*sqrt(d$V_1)
d$B2 = d$Z*sqrt(d$V)
d2$B1 = d2$Z_1*sqrt(d2$V_1)
d2$B2 = d2$Z*sqrt(d2$V)

plot(d$B1, d$B2, xlab = "Effect size on HB", ylab = "Effect size on T2D", pch = 20, xlim = c(-0.15, 0.15), ylim = c(-0.08, 0.06))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("D. HB v. T2D (HB ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on HB", ylab = "Effect size on T2D", xlim =c(-0.04, 0.03), ylim = c(-0.15, 0.27))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("E. HB v. T2D (T2D ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")


# plot BHM LSBMD
#d = read.table("../../../overlaps/data/BMI_ATH_23.overlap_wbetas", as.is = T, head = T)
#d2 = read.table("../../../overlaps/data/ATH_23_BMI.overlap_wbetas",  as.is = T, head = T)
d = read.table("../../../overlaps/data/BHM_23_LSBMD.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../../overlaps/data/LSBMD_BHM_23.overlap_wbetas",  as.is = T, head = T)


mhc =which(d$chr == "chr6" & d$pos >= 26000000 & d$pos <=34000000)
if (length(mhc) >0){
        d = d[-mhc,]
}

mhc =which(d2$chr == "chr6" & d2$pos >= 26000000 & d2$pos <=34000000)
if (length(mhc) >0){
        d2 = d2[-mhc,]
}

d$B1 = d$Z_1*sqrt(d$V_1)
d$B2 = d$Z*sqrt(d$V)
d2$B1 = d2$Z_1*sqrt(d2$V_1)
d2$B2 = d2$Z*sqrt(d2$V)

plot(d$B1, d$B2, xlab = "Effect size on BHM", ylab = "Effect size on LSBMD", pch = 20, xlim = c(-0.2, 0.1), ylim = c(-0.04, 0.04))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("F. BHM v. LSBMD (BHM ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on BHM", ylab = "Effect size on LSBMD", xlim = c(-0.03, 0.03), ylim = c(-0.13, 0.13))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("G. BHM v. LSBMD (LSBMD ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

dev.off()
