

pdf("cad_ra.pdf")
par(mfrow = c(2,2))
# plot CAD RA


d = read.table("../../../overlaps/data/CAD_RA.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../../overlaps/data/RA_CAD.overlap_wbetas",  as.is = T, head = T)

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

plot(d$B1, d$B2, xlab = "Effect size on CAD", ylab = "Effect size on RA", pch = 20, xlim = c(-0.15, 0.17), ylim = c(-0.05, 0.11))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("A. CAD v. RA (CAD ascertainment)", adj = 0, cex = 1)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on CAD", ylab = "Effect size on RA", xlim = c(-0.08, 0.08), ylim = c(-0.25, 0.65))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("B. CAD v. RA (RA ascertainment)", adj = 0, cex = 1)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

# plot new CAD 

d = read.table("../../../overlaps/data/CAD_2013_RA.overlap_wbetas", as.is = T, head = T)
mhc =which(d$chr == "chr6" & d$pos >= 26000000 & d$pos <=34000000)
if (length(mhc) >0){
        d = d[-mhc,]
}
d$B1 = d$Z_1*sqrt(d$V_1)
d$B2 = d$Z*sqrt(d$V)

plot(d$B1, d$B2, xlab = "Effect size on CAD (expanded set)", ylab = "Effect size on RA", pch = 20, xlim = c(-0.15, 0.22), ylim = c(-0.1, 0.11))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("C. CAD v. RA (expanded CAD set)", adj = 0, cex = 1)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

dev.off()
