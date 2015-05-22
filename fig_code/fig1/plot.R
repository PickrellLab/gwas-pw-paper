

toplot = data.frame(matrix(nrow = 1000, ncol = 4))
toplot[,1] = 5*exp(- abs(1:1000- 500)/80    )
toplot[,3] = 5*exp(- abs(1:1000- 200)/80    )
toplot[,4] = 5*exp(- abs(1:1000- 800)/80    )
toplot[,2] = 0


tmp = toplot
tmp[,1] = toplot[,1]+rnorm(1000, 0, sd =1)
tmp[,2] = rnorm(1000, 0, sd =1)


pdf(file = "schematic.pdf", height = 4, width = 8)

par(mfrow = c(2, 4))
par(mar = c(0, 4, 2, 1))

cc = 1.3
plot(1:1000, -log10(pnorm(-abs(tmp[,1]))*2), pch = 20, ylim = c(0, 15), axes = F, ylab = "-log10(P) [phenotype 1]", col = "darkgrey", xlab = "")
mtext("genomic position", side = 1, cex = 0.5)
tmp[500,1] =max(tmp[,1]-0.1)
points(500, -log10(pnorm(-abs(tmp[500,1]))*2), col = "red", pch = 20, cex = cc)
text(500, 13, lab = "model 1")
axis(2)
axis(1, line = 1, at = c(0, 1000), labels = F)

par(mar = c(0, 0, 2, 1))
tmp[,1] = toplot[,2]+rnorm(1000, 0, sd =1)
plot(1:1000, -log10(pnorm(-abs(tmp[,1]))*2), pch = 20,  ylim = c(0, 15), axes = F, xlab = "", ylab = "", col = "darkgrey")
points(500, -log10(pnorm(-abs(tmp[500,1]))*2), col = "red", pch = 20, cex = cc)
axis(1, line = 1, at = c(0, 1000), labels = F)
text(500, 13, lab = "model 2")
tmp[,1] = toplot[,1]+rnorm(1000, 0, sd =1)
plot(1:1000, -log10(pnorm(-abs(tmp[,1]))*2), pch = 20,  ylim = c(0, 15), axes = F, xlab = "", ylab = "", col = "darkgrey")
tmp[500,1] = max(tmp[,1])-0.1
points(500, -log10(pnorm(-abs(tmp[500,1]))*2), col = "red", pch = 20, cex = cc)
text(500, 13, lab = "model 3")
axis(1, line = 1, at = c(0, 1000), labels = F)
tmp[,1] = toplot[,3]+rnorm(1000, 0, sd =1)
plot(1:1000, -log10(pnorm(-abs(tmp[,1]))*2), pch = 20, ylim = c(0, 15), axes = F, xlab = "" , ylab = "", col = "darkgrey")
tmp[200,1] = max(tmp[,1]) - 0.1
points(200, -log10(pnorm(-abs(tmp[200,1]))*2), col = "red", pch = 20, cex = cc)
points(800, -log10(pnorm(-abs(tmp[800,1]))*2), col = "blue", pch = 20, cex = cc)
text(500, 13, lab = "model 4")
axis(1, line = 1, at = c(0, 1000), labels = F)
par(mar = c(0, 4, 2, 1))
tmp[,1] = toplot[,2]+rnorm(1000, 0, sd =1)
plot(1:1000,  log10(pnorm(-abs(tmp[,1]))*2), pch = 20, ylim = c(-15, 0), axes = F, xlab = "", ylab = "log10(P) [phenotype 2]", col = "darkgrey")
points(500, log10(pnorm(-abs(tmp[500,1])*2)), col = "red", pch = 20, cex = cc)
legend("bottomleft", legend = c("causal SNP", "2nd causal SNP (model 4 only)"), pch = 20, col = c("red", "blue"), bty = "n", cex = 0.7)
axis(2)
par(mar = c(0, 0, 2, 1))
tmp[,1] = toplot[,1]+rnorm(1000, 0, sd =1)
plot(1:1000,  log10(pnorm(-abs(tmp[,1]))*2), pch = 20,  ylim = c(-15, 0), axes = F, xlab = "", col = "darkgrey")
tmp[500,1] = max(tmp[,1])- 0.1
points(500, log10(pnorm(-abs(tmp[500,1]))*2), col = "red", pch = 20, cex = cc)


tmp[,1] = toplot[,1]+rnorm(1000, 0, sd =1)
plot(1:1000,  log10(pnorm(-abs(tmp[,1]))*2), pch = 20, ylim = c(-15, 0), axes = F, xlab = "",  col = "darkgrey")
tmp[500,1] = max(tmp[,1])-0.1
points(500, log10(pnorm(-abs(tmp[500,1]))*2), col = "red", pch = 20, cex = cc)

tmp[,1] = toplot[,4]+rnorm(1000, 0, sd =1)
plot(1:1000,  log10(pnorm(-abs(tmp[,1]))*2), pch = 20, ylim = c(-15, 0), axes = F, xlab = "" , ylab = "", col = "darkgrey")
points(200, log10(pnorm(-abs(tmp[200,1]))*2), col = "red", pch = 20, cex = cc)
tmp[800,1] = max(tmp[,1]) - 0.1
points(800, log10(pnorm(-abs(tmp[800,1]))*2), col = "blue", pch = 20, cex = cc)
dev.off()


