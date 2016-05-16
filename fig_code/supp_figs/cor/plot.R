
d = read.table("all_pw_wcov_freeze_082515",as.is = T)

d = d[order(d[,3]),]
d[,4] = 1:nrow(d)

pdf("cor.pdf")
plot(d[,4], d[,3], ylim = c(-0.5, 0.9), axes = F, xlab = "", ylab = "correlation")
axis(2)
h = head(d, n = 7)
c = rainbow(7)
for(i in 1:nrow(h)){
	p1 = h[i,1]
	p2 = h[i,2]
	if (p1 == "BMI_2015"){
		p1 = "BMI"
	}
	if (p2 == "BMI_2015"){
		p2 = "BMI"
	}
	if (p1 == "AAM_23"){
		p1 = "AAM (23)"
	}
	if (p2 == "AAM_23"){
		p2 = "AAM (23)"
	}
	p1 = gsub("_23", "", p1)
	p2 = gsub("_23", "", p2)
	
	t = paste(p1, p2, sep = "-")
	points(h[i,4], h[i,3], pch = 20, col = c[i])
	print(i)
	if (i > 1){
		if ((h[i,3]-h[(i-1),3]) < 0.02){
			h[i,3] = h[i,3]+0.01
		}
	}
	text(h[i,4]+20, h[i,3], lab = t, col = c[i], adj = 0, cex = 0.6)
}

t= tail(d, n = 11)
c = rainbow(11)
for(i in 1:nrow(t)){
        p1 = t[i,1]
        p2 = t[i,2]
        if (p1 == "BMI_2015"){
                p1 = "BMI"
        }
        if (p2 == "BMI_2015"){
                p2 = "BMI"
        }
        if (p1 == "AAM_23"){
                p1 = "AAM (23)"
        }
        if (p2 == "AAM_23"){
                p2 = "AAM (23)"
        }
        p1 = gsub("_23", "", p1)
        p2 = gsub("_23", "", p2)

        nn = paste(p1, p2, sep = "-")
        points(t[i,4], t[i,3], pch = 20, col = c[i])
        print(i)
        if (i > 1){
                if ((t[i,3]-t[(i-1),3]) < 0.02){
                        t[i,3] = t[i,3]+0.01
                }
        }
        text(t[i,4]-20, t[i,3], lab = nn, col = c[i], adj = 1, cex = 0.6)
}

tmp = d[d[,1]=="AAM" & d[,2]== "AVD_23",]

points(tmp[1,4], tmp[1,3], pch = 20, col = "red")
text(tmp[1,4]-20, tmp[1,3], lab = "AAM-AVD", col = "red", adj = 1, cex = 0.6)
dev.off()

