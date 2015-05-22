

d1 = read.table("FNBMD_LSBMD.MLE", as.is = T)
d2 = read.table("FNBMD_LSBMD.wcornold.MLE", as.is = T)
d3 = read.table("FNBMD_LSBMD_wLD.MLE", as.is = T)

toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplot[,1] = d1[,3]
toplot[,2] = d2[,3]
toplot[,3] = d3[,3]

pdf("barplot.pdf")
par(mfrow = c(2,2))

barplot(t(as.matrix(toplot)), beside = T, ylim = c(0, 1), names.arg = c("m0", "m1", "m2", "m3", "m4"), ylab = "proportion", legend.text = c("no correlation, no LD", "correlation, no LD", "correlation and LD"), main = "LSBMD-FNBMD")

d1 = read.table("MCV_RBC.MLE", as.is = T)
d2 = read.table("MCV_RBC.wcornold.MLE", as.is = T)
d3 = read.table("MCV_RBC_wLD.MLE", as.is = T)

toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplot[,1] = d1[,3]
toplot[,2] = d2[,3]
toplot[,3] = d3[,3]

barplot(t(as.matrix(toplot)), beside = T, ylim = c(0, 1), names.arg = c("m0", "m1", "m2", "m3", "m4"), ylab = "proportion", main = "MCV-RBC")

d1 = read.table("HB_PCV.MLE", as.is = T)
d2 = read.table("HB_PCV.wcornold.MLE", as.is = T)
d3 = read.table("HB_PCV_wLD.MLE", as.is = T)

toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplot[,1] = d1[,3]
toplot[,2] = d2[,3]
toplot[,3] = d3[,3]

barplot(t(as.matrix(toplot)), beside = T, ylim = c(0, 1), names.arg = c("m0", "m1", "m2", "m3", "m4"), ylab = "proportion", main = "HB-PCV")

d1 = read.table("HB_RBC.MLE", as.is = T)
d2 = read.table("HB_RBC.wcornold.MLE", as.is = T)
d3 = read.table("HB_RBC_wLD.MLE", as.is = T)

toplot = data.frame(matrix(nrow = 5, ncol = 3))
toplot[,1] = d1[,3]
toplot[,2] = d2[,3]
toplot[,3] = d3[,3]

barplot(t(as.matrix(toplot)), beside = T, ylim = c(0, 1), names.arg = c("m0", "m1", "m2", "m3", "m4"), ylab = "proportion", main = "HB-RBC")


dev.off()




