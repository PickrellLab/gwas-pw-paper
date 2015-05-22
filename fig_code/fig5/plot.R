

pdf("causal2.pdf", height = 7*(4/3))
d = read.table("top_causal_01", as.is = T, head = T)

l = matrix(nrow = 4, ncol = 3)
l[1,1] = 1
l[2,1] = 1
l[3,1] = 1
l[4,1] = 1
l[1,2] = 2
l[1,3] = 3
l[2,2] = 4
l[2,3] = 5
l[3,2] = 6
l[3,3] = 7
l[4,2] = 8
l[4,3] = 9
layout(l, widths = c(0.9, 1,1))
par(mar = c(4, 4, 3, 2))
plot(NA, axes = F, xlim = c(0, 1), ylim = c(0,1), xlab = "", ylab = "")
mtext("Putative causally-related traits", adj = 0, cex = 1, line = 1)

y = 0.95
arrows(0.42, y, 0.58, y, col = "red", length = 0.08)
text(0.39, y, adj = c(1, 0.5), lab = "BMI")
text(0.61, y, adj = c(0, 0.5), lab = "TG")

y = 0.65
arrows(0.42, y, 0.58, y, col = "red", length = 0.08)
text(0.39, y, adj = c(1, 0.5), lab = "LDL")
text(0.61, y, adj = c(0, 0.5), lab = "CAD")

y = 0.35
arrows(0.42, y, 0.58, y, col = "red", length = 0.08)
text(0.39, y, adj = c(1, 0.5), lab = "BMI")
text(0.61, y, adj = c(0, 0.5), lab = "T2D")

y = 0.05
arrows(0.42, y, 0.58, y, col = "blue", length = 0.08)
text(0.39, y, adj = c(1, 0.5), lab = "HTHY")
text(0.61, y, adj = c(0, 0.5), lab = "HEIGHT")

legend("topleft", legend = c("positive effect", "negative effect"), lty = 1, col = c("red", "blue"), bty = "n", cex = 0.8)

# plot BMI TG

d = read.table("../../overlaps/data/BMI_2015_TG.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../overlaps/data/TG_BMI_2015.overlap_wbetas",  as.is = T, head = T)

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

plot(d$B1, d$B2, xlab = "Effect size on BMI [s.d.]", ylab = "Effect size on TG [s.d.]", pch = 20, xlim = c(-0.035, 0.07), ylim = c(-0.02, 0.03))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("A. BMI v. TG (BMI ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on BMI [s.d.]", ylab = "Effect size on TG [s.d.]", xlim = c(-0.03, 0.015), ylim = c(-0.3, 0.15))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("B. BMI v. TG (TG ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

# plot LDL CAD

d = read.table("../../overlaps/data/LDL_CAD.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../overlaps/data/CAD_LDL.overlap_wbetas",  as.is = T, head = T)

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

plot(d$B1, d$B2, xlab = "Effect size on LDL [s.d.]", ylab = "Effect size on CAD [log-odds]", pch = 20, xlim = c(-0.12, 0.16), ylim = c(-0.15, 0.13))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("C. LDL v. CAD (LDL ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on LDL [s.d.]", ylab = "Effect size on CAD [log-odds]", xlim = c(-0.07, 0.16), ylim = c(-0.14, 0.16))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("D. LDL v. CAD (CAD ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

# plot BMI T2D
d = read.table("../../overlaps/data/BMI_2015_T2D.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../overlaps/data/T2D_BMI_2015.overlap_wbetas",  as.is = T, head = T)


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

plot(d$B1, d$B2, xlab = "Effect size on BMI [s.d.]", ylab = "Effect size on T2D [log-odds]", pch = 20, xlim = c(-0.035, 0.065), ylim = c(-0.06, 0.11))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("E. BMI v. T2D (BMI ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on BMI [s.d.]", ylab = "Effect size on T2D [log-odds]", xlim = c(-0.02, 0.06), ylim = c(-0.14, 0.27))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("F. BMI v. T2D (T2D ascertainment)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")


# plot HTHY HEIGHT
d = read.table("../../overlaps/data/HTHY_23_HEIGHT.overlap_wbetas", as.is = T, head = T)
d2 = read.table("../../overlaps/data/HEIGHT_HTHY_23.overlap_wbetas",  as.is = T, head = T)


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

plot(d$B1, d$B2, xlab = "Effect size on HTHY [log-odds]", ylab = "Effect size on HEIGHT [s.d.]", pch = 20, xlim = c(-0.3, 0.35), ylim = c(-0.015, 0.02))
for (i in 1:nrow(d)){
        se1 = sqrt(d[i,]$V_1)
        se2 = sqrt(d[i,]$V)
        lines(c(d[i,]$B1 - se1, d[i,]$B1+se1), c(d[i,]$B2, d[i,]$B2), col = "darkgrey")
        lines( c(d[i,]$B1, d[i,]$B1), c(d[i,]$B2 - se2, d[i,]$B2+se2), col = "darkgrey")

}
points(d$B1, d$B2, pch = 20)

mtext("G. HTHY v. HEIGHT (HTHY asc.)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

plot(d2$B2, d2$B1, pch = 20, xlab = "Effect size on HTHY [log-odds]", ylab = "Effect size on HEIGHT [s.d.]", xlim = c(-0.09, 0.13),ylim = c(-0.1, 0.1))

for (i in 1:nrow(d2)){
        se1 = sqrt(d2[i,]$V_1)
        se2 = sqrt(d2[i,]$V)
        lines( c(d2[i,]$B2-se2, d2[i,]$B2+se2), c(d2[i,]$B1, d2[i,]$B1), col = "darkgrey")
        lines( c(d2[i,]$B2, d2[i,]$B2), c(d2[i,]$B1-se1, d2[i,]$B1+se1), col = "darkgrey")
}
points(d2$B2, d2$B1, pch = 20)
mtext("H. HTHY v. HEIGHT (HEIGHT asc.)", adj = 0, cex = 0.6)
lines(c(-10, 10), c(0,0), col = "grey")
lines(c(0,0), c(-10,10), col = "grey")

dev.off()
