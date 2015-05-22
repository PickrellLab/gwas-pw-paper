pdf("plot.pdf")



toplot = read.table("overlap_matrix", as.is = T)

toplot[is.na(toplot)] = 1
h = hclust(as.dist(toplot), method = "complete")
hm = heatmap(as.matrix(toplot), Rowv=h$order, symm = T, cexRow = 0.75, cexCol = 0.75)
o = hm$rowInd
print(o)
dev.off()
pdf("heatmap.pdf")
cc = rev(heat.colors(41))
cc[1] = "white"

l = matrix(nrow = 1, ncol = 2)
l[1,1] = 1
l[1,2] = 2
layout(l, widths = c(10,1))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = F, xlab = "", ylab = "")
for (i in 1:(length(o))){
	n = names(toplot)[o[i]]
	if (n == "CD_NEW_062314"){ n = "CD"}
	if (n == "HEIGHT_2014"){ n = "HEIGHT"}
	if (n == "BMI_2015"){ n = "BMI"}
	if (n == "SCZ_2014"){ n = "SCZ"}
	if (n == "AAM_23"){ n = "AAM (23)"}
	n = gsub("_23", "", n)
	mtext(n, las = 2, side = 1, at = (i-1)*(1/nrow(toplot)) + 0.5/nrow(toplot), cex = 0.6)
	mtext(n, las = 2, side = 2, at = 1-(i-1)*(1/nrow(toplot)) - 0.5/nrow(toplot), cex =0.6)
        for(j in (i):length(o)){
		x1 = (i-1)*(1/nrow(toplot))
		x2 = x1+ 1/nrow(toplot)
		y1 = 1-((j-1)*(1/nrow(toplot)))
		y2 = y1 -1/nrow(toplot)
		v = toplot[ o[j], o[i] ]
		tc = floor(v *40)+1
		print(tc)
		c  = cc[tc]
		if (i == j) { c= "lightgrey"}
		rect(x1, y2, x2, y1, col = c, border = "lightgrey")
	
		x1 = (j-1)*(1/nrow(toplot))
                x2 = x1 + 1/nrow(toplot)
                y1 =  1-((i-1)*(1/nrow(toplot)))
                y2 = y1 - 1/nrow(toplot)
                v = toplot[ o[i], o[j] ]
                tc = floor(v *40)+1
 		c  = cc[tc]
                if (i == j) { c= "lightgrey"}
                rect(x1, y2, x2, y1, col = c, border = "lightgrey")
	}
}
mtext("Proportion of shared signals across all pairs of traits", adj = 0)
par(mar = c(0,0,0,0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = F, ylab = "", xlab = "")
for (i in 1:length(cc)){
	x1 = 0
	x2 = 0.5
	h = 0.2/41
	y1 = 0.4+((i-1)*h)
	y2 = 0.4+(i*h)
	rect(x1, y1, x2, y2, col = cc[i], border = cc[i])

}
axis(2, at = c(0.4, 0.5, 0.6), lab = c("0", "0.5", "1"))
dev.off()
