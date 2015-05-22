

pdf("bmi_expanded.pdf", height = 3.5)
par(mfrow = c(1,2))
# plot BMI TG


d = read.table("../../../overlaps/data/BMI_2015_noimpute_TG.overlap_wbetas", as.is = T, head = T)

mhc =which(d$chr == "chr6" & d$pos >= 26000000 & d$pos <=34000000)
if (length(mhc) >0){
        d = d[-mhc,]
}
d$B1 = d$Z_1*sqrt(d$V_1)
d$B2 = d$Z*sqrt(d$V)

plot(d$B1, d$B2, xlab = "Effect size on BMI (expanded set)", ylab = "Effect size on TG", pch = 20, xlim = c(-0.05, 0.09), ylim = c(-0.03, 0.07))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)
tmp = d[d$B2 > 0.06,]
points(tmp$B1, tmp$B2, pch = 20, col = "red")
text(tmp$B1+0.01, tmp$B2, adj = 0, lab = paste(tmp[1,]$id, "(near APOE)"), cex = 0.6, col = "red")

mtext("A. BMI v. TG (expanded BMI set)", adj = 0, cex = 1)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")


#plot BMI T2D
d = read.table("../../../overlaps/data/BMI_2015_noimpute_T2D.overlap_wbetas", as.is = T, head = T)

mhc =which(d$chr == "chr6" & d$pos >= 26000000 & d$pos <=34000000)
if (length(mhc) >0){
        d = d[-mhc,]
}
d$B1 = d$Z_1*sqrt(d$V_1)
d$B2 = d$Z*sqrt(d$V)

plot(d$B1, d$B2, xlab = "Effect size on BMI (expanded set)", ylab = "Effect size on T2D", pch = 20, xlim = c(-0.05, 0.09), ylim = c(-0.07, 0.3))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)
tmp = d[d$B2 > 0.2,]
points(tmp$B1, tmp$B2, pch = 20, col = "red")
text(tmp$B1+0.01, tmp$B2, adj = 0, lab = paste(tmp[1,]$id, "(near TCF7L2)"), cex = 0.6, col = "red")

mtext("A. BMI v. T2D (expanded BMI set)", adj = 0, cex = 1)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")


dev.off()
