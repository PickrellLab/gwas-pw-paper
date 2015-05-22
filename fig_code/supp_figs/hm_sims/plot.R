
pdf("sims.pdf")
par(mfrow = c(2,2))

t = c(0, 0.25, 0.25, 0.25, 0.25)
#
# no correlation, split 
#

toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplotn = data.frame(matrix(nrow = 5, ncol = 3))
for (i in 0:4){
	print(i)
	f = paste0("pi", i, "_m0.split")
	d = read.table(f)
	q = quantile(d[,3], prob = c(0.05, 0.5, 0.95))
	toplot[i+1,]= q
        f2 = paste0("pi", i, "_m0.split.naive")
        d2 = read.table(f2, as.is =T, comment.char = "")
        q2 = quantile(d2[,3], prob = c(0.05, 0.5, 0.95))
	toplotn[i+1,]= q2
}
print(toplot)

plot(NA, xlim = c(0, 1), ylim = c(0, 0.5), axes = F, xlab = "Model", ylab = "Inferred proportion")
axis(2)
x = 1/6*seq(1, 5)
axis(1, at = x, lab = c("0", "1", "2", "3", "4"))
points(x, toplot[,2], pch = 20)
for(i in 1:5){
	xi = x[i]
	y1 = toplot[i,1]
	y2 = toplot[i,3]
	lines(c(xi, xi), c(y1, y2))
	lines(c(xi - 0.03, xi+0.07), c(t[i], t[i]), col = "red")
}

points(x+0.05, toplotn[,2], pch = 20, col = "darkgrey")
for(i in 1:5){
        xi = x[i]+0.05
        y1 = toplotn[i,1]
        y2 = toplotn[i,3]
        lines(c(xi, xi), c(y1, y2), col = "darkgrey")
}
mtext("A. No correlation, separate cohorts", adj  =0, cex = 0.8)

#
# no correlation, nosplit 
#

toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplotn = data.frame(matrix(nrow = 5, ncol = 3))
for (i in 0:4){
        print(i)
        f = paste0("pi", i, "_m0.nosplit")
        d = read.table(f)
        q = quantile(d[,3], prob = c(0.05, 0.5, 0.95))
        toplot[i+1,]= q
        f2 = paste0("pi", i, "_m0.nosplit.naive")
        d2 = read.table(f2, as.is =T, comment.char = "")
        q2 = quantile(d2[,3], prob = c(0.05, 0.5, 0.95))
        toplotn[i+1,]= q2
}
print(toplot)

plot(NA, xlim = c(0, 1), ylim = c(0, 0.5), axes = F, xlab = "Model", ylab = "Inferred proportion")
axis(2)
x = 1/6*seq(1, 5)
axis(1, at = x, lab = c("0", "1", "2", "3", "4"))
points(x, toplot[,2], pch = 20)
for(i in 1:5){
        xi = x[i]
        y1 = toplot[i,1]
        y2 = toplot[i,3]
        lines(c(xi, xi), c(y1, y2))
        lines(c(xi - 0.03, xi+0.07), c(t[i], t[i]), col = "red")
}

points(x+0.05, toplotn[,2], pch = 20, col = "darkgrey")
for(i in 1:5){
        xi = x[i]+0.05
        y1 = toplotn[i,1]
        y2 = toplotn[i,3]
        lines(c(xi, xi), c(y1, y2), col = "darkgrey")
}
legend("topleft", legend = c("hierarchical model", "P-values", "truth"), pch = c(20, 20, NA), lty = c(NA, NA, 1), col = c("black", "darkgrey", "red"), bty = "n")
mtext("B. No correlation, same cohort", adj  =0, cex = 0.8)

#
# correlation, split 
#
        
toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplotn = data.frame(matrix(nrow = 5, ncol = 3))
for (i in 0:4){
        print(i)
        f = paste0("pi", i, "_m5.split")
        d = read.table(f)
        q = quantile(d[,3], prob = c(0.05, 0.5, 0.95))
        toplot[i+1,]= q
        f2 = paste0("pi", i, "_m5.split.naive")
        d2 = read.table(f2, as.is =T, comment.char = "")
        q2 = quantile(d2[,3], prob = c(0.05, 0.5, 0.95))
        toplotn[i+1,]= q2
}
print(toplot)

plot(NA, xlim = c(0, 1), ylim = c(0, 0.5), axes = F, xlab = "Model", ylab = "Inferred proportion")
axis(2)
x = 1/6*seq(1, 5)
axis(1, at = x, lab = c("0", "1", "2", "3", "4"))
points(x, toplot[,2], pch = 20)
for(i in 1:5){
        xi = x[i]
        y1 = toplot[i,1]
        y2 = toplot[i,3]
        lines(c(xi, xi), c(y1, y2))
        lines(c(xi - 0.03, xi+0.07), c(t[i], t[i]), col = "red")
}

points(x+0.05, toplotn[,2], pch = 20, col = "darkgrey")
for(i in 1:5){
        xi = x[i]+0.05
        y1 = toplotn[i,1]
        y2 = toplotn[i,3]
        lines(c(xi, xi), c(y1, y2), col = "darkgrey")
}
mtext("C. Correlation = 0.3, separate cohorts", adj  =0, cex = 0.8)

#
# correlation, nosplit 
#

toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplotn = data.frame(matrix(nrow = 5, ncol = 3))
for (i in 0:4){
        print(i)
        f = paste0("pi", i, "_m5.nosplit")
        d = read.table(f)
        q = quantile(d[,3], prob = c(0.05, 0.5, 0.95))
        toplot[i+1,]= q
        f2 = paste0("pi", i, "_m5.nosplit.naive")
        d2 = read.table(f2, as.is =T, comment.char = "")
        q2 = quantile(d2[,3], prob = c(0.05, 0.5, 0.95))
        toplotn[i+1,]= q2
}
print(toplot)

plot(NA, xlim = c(0, 1), ylim = c(0, 0.5), axes = F, xlab = "Model", ylab = "Inferred proportion")
axis(2)
x = 1/6*seq(1, 5)
axis(1, at = x, lab = c("0", "1", "2", "3", "4"))
points(x, toplot[,2], pch = 20)
for(i in 1:5){
        xi = x[i]
        y1 = toplot[i,1]
        y2 = toplot[i,3]
        lines(c(xi, xi), c(y1, y2))
        lines(c(xi - 0.03, xi+0.07), c(t[i], t[i]), col = "red")
}

points(x+0.05, toplotn[,2], pch = 20, col = "darkgrey")
for(i in 1:5){
        xi = x[i]+0.05
        y1 = toplotn[i,1]
        y2 = toplotn[i,3]
        lines(c(xi, xi), c(y1, y2), col = "darkgrey")
}

mtext("D. Correlation = 0.3, same cohort", adj  =0, cex = 0.8)
dev.off()



	


