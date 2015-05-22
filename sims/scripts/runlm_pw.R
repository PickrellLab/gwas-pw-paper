
a = commandArgs(T)

infile = a[1]
legendfile = a[2]
phenofile = a[3]
outfile = a[4]
index = a[5]
split = a[6]
print(split)
tmp = strsplit(infile, split = "[.]")[[1]]
print(tmp)
chrom = tmp[length(tmp)-6]
print(chrom)

#d = read.table(infile, as.is = T)
tmp = strsplit(infile, split = "[.]")[[1]]
s = read.table(legendfile, as.is = T, head = T)
p = read.table(phenofile, as.is = T)
#geno = data.frame(matrix(nrow = nrow(d), ncol = ncol(d)/2))
geno = read.table(paste0(infile, ".geno.gz"), as.is = T)

#for (i in 1:(ncol(d)/2)){
#	print (i)
#	geno[,i] = d[,(i*2-1)] + d[,(i*2)]
#}

#write.table(geno, file = gzfile(paste0(infile, ".geno.gz")), quote = F, row.names = F, col.names = F)
toprint= data.frame(matrix(nrow = nrow(s), ncol = 10))
colnames(toprint) = c("SNPID", "CHR", "POS","F", "Z_1", "V_1", "Z_2", "V_2", "N", "SEGNUMBER")
for (i in 1:nrow(geno)){
	print(i)
        g = as.numeric(geno[i,])
	l1 = NA
	l2 = NA
	if (split == "0"){
		l1 = lm(p[,3] ~ g)
		l2 = lm(p[,4] ~ g)
	}
	else if (split == "1"){
		l1 = lm(p[1:2500,3]~g[1:2500])
		l2 = lm(p[2501:5000,4] ~g[2501:5000])
	}
	c1 = summary(l1)$coef
	print(c1)
	m1 = NA
	se1 = NA
	Z1 = NA
	if (nrow(c1) > 1){
		m1 = c1[2,1]
		se1 = c1[2,2]
		Z1 = m1/se1
	}
		
        c2 = summary(l2)$coef
        print(c2)
        m2 = NA
        se2 = NA
        Z2 = NA
        if (nrow(c2) > 1){
               m2 = c2[2,1]
               se2 = c2[2,2]
               Z2 = m2/se2
        }
	f = sum(g)/(2*ncol(geno))
	toprint[i,] = c(s[i,1], chrom, s[i,2], f, Z1, se1*se1, Z2, se2*se2, "5000", index)
}

write.table(toprint, file = outfile, quote = F, row.names = F)
