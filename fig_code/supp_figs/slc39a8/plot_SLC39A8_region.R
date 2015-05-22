
source("../../fig3/scripts/plotting_funcs.R")


d = read.table("chr_102000000_104000000", as.is = T, head = T)
d = d[d$POS > 102500000 & d$POS < 103500000,]
ens = read.table( "../../fig3/refGene_10_16_2014.txt.gz", as.is = T)
tmp = d[d$SNPID == "rs13107325",]


pdf("SLC39A8_region.pdf", height= 6, width = 8)
l = matrix(nrow = 3, ncol = 2)
l[1,1] = 1
l[2,1] = 2
l[3,1] = 3
l[,2]  = 4
layout(l, height = c(1.5, 1, 1.5), width = c(2, 1))

par(mar = c(0, 4, 4, 2))
plot(d$POS, -log10( pnorm(-abs(d$Z_SCZ))*2), axes = F, xlab = "", ylab = expression('-log'[10]*'(P) [schizophrenia]'), pch = 20, col = "black")
points(tmp$POS, -log10( pnorm(-abs(tmp$Z_SCZ))*2), pch = 20, col = "red")
mtext("A. Regional associations", adj = 0, cex = 0.8)
axis(2)
#text(tmp$pos + 3000, -log10( pnorm(-abs(tmp$Z_CAD))*2), adj = 0, lab = "rs635634")

par(mar = c(2, 4, 4, 2))
e = plotgenesinregion(ens, d[1,]$CHR, d[1,]$POS, d[nrow(d),]$POS)
print(e)
mtext("Position on chr4 (Mb)", side = 1, line = 2.2, cex = 0.8)
for (i in 1:nrow(e)){
	y = i/nrow(e) + 0.1
	mi = min(d$POS)
	ma = max(d$POS)
	x = e[i,5]/2+e[i,6]/2
	if (e[i,6] > ma){ 
	 x = e[i,5]/2+ma/2
	}
	if (y > 1){
		y = i/nrow(e) - 0.5
	}
	print(paste(x, y))
	text(x, y, lab = e[i,13], cex = 0.8)
}


par(mar = c(2, 4, 4, 2))
plot(d$POS,  log10( pnorm(-abs(d$Z_PD))*2), axes = F, xlab = "", ylab = expression('log'[10]*'(P) [PD]'), ylim = c(-7, 0), pch = 20)
points(tmp$POS, log10( pnorm(-abs(tmp$Z_PD))*2), pch = 20, col = "red")
axis(2, at = c(0, -2, -4, -6, -8), lab = c("0", "-2", "-4", "-6", "-8"))

#
# plot effects
#

par(mar = c(5, 1, 4, 1))
d = read.table("rs13107325.pheno2", as.is = T, head = T)

p = read.table("../../fig3/allpheno", as.is = T)

toplot = data.frame(matrix(nrow = nrow(p), ncol = 5))
for (i in 1:nrow(p)){
        toplot[i,1] = p[i,1]
        f = paste0("Z_", toplot[i,1])
        f2 = paste0("V_", toplot[i,1])
        print(f)
        w = which(names(d) == f)
        w2 = which(names(d) == f2)

        toplot[i,2] = d[1,w]
        toplot[i,3] = d[1,w2]
        toplot[i,5] = p[i,2]
}

toplot[,4] = toplot[,2]*sqrt(toplot[,3])

toplot = toplot[!is.na(toplot[,2]),]

toplot[toplot[,1] == "SCZ_2014",1] = "SCZ"
toplot[toplot[,1] == "HEIGHT_2014",1] = "HEIGHT"
toplot[toplot[,1] == "CD_NEW_062314",1] = "CD"

tmp = toplot[toplot[,5] == "Q",]
tmp2 = toplot[toplot[,5] == "CC",]

tmp = tmp[order(-abs(tmp[,2])),]
tmp2 = tmp2[order(abs(tmp2[,2])),]
tmp[tmp[,1] == "AAM_23", 1] = "AAM (23)"
tmp[,1] = gsub("_23", "", tmp[,1])
tmp2[,1] = gsub("_23", "", tmp2[,1])


plot(NA, xlim = c(-0.2, 0.25), ylim = c(0, 1), axes = F, ylab = "", xlab = "Effect size (s.d./ln[odds])")
axis(1)

mtext("B. rs13107325 C->T, all effects", adj = 0, cex = 0.8)

lines( c(0, 0), c(0, 1), col = "grey")
for (i in 1:nrow(tmp)){
        ys = i/(nrow(tmp)+nrow(tmp2)+2)
        y = 1-ys
	c = "black"
	if (abs(tmp[i,2]) > 2.575829){
		c = "pink"
	}
	if (abs(tmp[i,2]) > 5.45){
		c = "red"
	}
        points(tmp[i,4], y, pch = 20, col = c)
        se = sqrt(tmp[i,3])
        points( c(tmp[i,4]-1.96*se, tmp[i,4]+1.96*se), c(y, y), typ = "l")
        text(tmp[i,4]+1.96*se+0.01, y, adj = 0, lab = tmp[i,1], cex =0.8)
}

yy = (nrow(tmp)+1)/(nrow(tmp)+nrow(tmp2)+2)
yy = 1-yy
lines(c(-1, 1), c(yy, yy), lty = 2, col = "grey")
text(0.2, yy+0.01, adj = c(1, 0), lab = "Quantitative traits", col = "grey", cex = 0.7)
text(0.2, yy-0.01, adj = c(1, 1), lab = "Case/control traits", col = "grey", cex = 0.7)


for (i in 1:nrow(tmp2)){
        ys = i/(nrow(tmp)+nrow(tmp2)+2)
        y = ys
       c = "black"
        if (abs(tmp2[i,2]) > 2.575829){
                c = "pink"
        }
        if (abs(tmp2[i,2]) > 5.45){
                c = "red"
        }
        points(tmp2[i,4], y, pch = 20, col = c)
        se = sqrt(tmp2[i,3])
        points( c(tmp2[i,4]-1.96*se, tmp2[i,4]+1.96*se), c(y, y), typ = "l")
        text(tmp2[i,4]+1.96*se+0.01, y, adj = 0, lab = tmp2[i,1], cex = 0.8)
}
legend("bottomright", legend = c(expression('P < 5 x 10'^-8), "P < 0.01", "P > 0.01"), pch = 20, cex = 0.95, col = c("red", "pink", "black"), bty = "n")


dev.off()


