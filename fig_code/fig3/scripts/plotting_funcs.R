#
# plotting functions for RNA-seq data
#

plotgenesinregion = function(ensembl, chr, st, stp, plot = T, ln = vector(), exons = data.frame(), regions = data.frame(), juncs = data.frame(), polya = data.frame()){
  tmp = ensembl[ensembl[,3] ==chr,]
  ex = vector()
  tmp  = tmp[(tmp[,5]>st & tmp[,6]<stp) | (tmp[,5]<st & tmp[,6]>st) | (tmp[,5]<stp & tmp[,6]>stp),]

  tab = table(tmp[,13])

  tmp2 = data.frame(matrix(nrow = length(tab), ncol = ncol(tmp)))
  for (i in 1:length(tab)){
	tt = tmp[tmp[,13]== names(tab)[i],]
	l = tt[,6]-tt[,5]
	ttt = tt[l == max(l),]
	tmp2[i,] = ttt[1,]
  }
  tmp = tmp2
  #tmp = tmp[order(tmp[,5]),]
  print(tmp)
  if (plot){
  	plot("NA", xaxs = "i", xlim = c(st,stp), ylim = c(0, 1), axes = F, ylab = "", xlab = "")
	tck = axTicks(1)
	tcklab = format(tck/1000000)
	axis(1, at = tck, lab = tcklab)
   } 
  if (nrow(tmp)==0){return("NA")} 
  for (i in 1:nrow(tmp)){
    y = i*(1/(nrow(tmp)+1))
    if (nrow(tmp)==1){
	y = 0.5
    }
    col = "black"
    if (tmp[i,4]=="-"){col ="indianred"}
    if (plot){
    	rect(tmp[i,5], y- 0.02,  tmp[i,6], y+0.02, col = col, border = col)
     }
    exst = as.numeric(strsplit(tmp[i,10], ",")[[1]])
    exen = as.numeric(strsplit(tmp[i,11], ",")[[1]])
    for (j in 1:length(exst)){
      ex = append(ex, seq(exst[j], exen[j]-1))
      #print(ex)
      if (plot){
      rect(exst[j], y-0.04,exen[j], y+0.04, col = "cadetblue", border = "cadetblue")
    }}
  }
  if (length(ln)>0 & plot){
	for (i in 1:length(ln)){
		p = ln[i]
		print("printing lines")
		lines(c(p, p), c(-5, 5), col = "grey")
	}
  }
  if (plot){
	add_exons(exons)
  }
  ex = unique(ex)
  return(tmp)
}
  
add_exons = function(d){
	  #print(d)
	  if (nrow(d)==0){
	     return(0)
	  }
	  #print(nrow(d))
	  for (i in 1:nrow(d)){
	  #    arrows(c(d[i,2], d[i,2]), c(0.5, 0.5), code = d[i,3], lwd = 7, col = "blue")
	       #print(d[1,])
	       lines(c(d[i,2], d[i,3]), c(0.5, 0.5), lwd = 5, col = "darkblue")
	  }

}
plotgene = function(gene, ensembl, st = 0, stp = 0){
  tmp = ensembl[ensembl[,13]==gene,]
  chr = tmp[1,3]
  if (st ==0){
    st = min(tmp[,5])-1000
  }
  if(stp==0){
    stp = max(tmp[,6])+1000
  }
  plot("NA", xlim = c(st,stp), ylim = c(0, 1), axes = F, ylab = "", xlab = "")
  for (i in 1:nrow(tmp)){
    y = i*(1/nrow(tmp))
    col = "black"
    if (tmp[i,4]=="-"){col ="red"}
    lines(c(tmp[i,5], tmp[i,6]), c(y,y), col = col, lwd = 3)
    exst = as.numeric(strsplit(tmp[i,10], ",")[[1]])
    exen = as.numeric(strsplit(tmp[i,11], ",")[[1]])
    for (j in 1:length(exst)){
      lines(c(exst[j],exen[j]), c(y, y), lwd = 7, col = "black")
    }
  }
  return(c(chr, st, stp))
}

plotreads = function(d, chr, start, end, xl = F){
  tmp = d[d[,2] ==chr & d[,3]>start & d[,3]< end,]
  plot("NA", xlim = c(start, end), ylim = c(0, 1), axes = F, ylab = "", xlab = "")
  if (xl ==T){
    axis(1, lab = T)
  }
  print(end)
  for (i in 1:nrow(tmp)){
    y = runif(1)
    st = tmp[i,3]
    stp = st+24
    col = "black"
    if (tmp[i,4]=="-"){
      stp = st-24
      col = "red"
    }
    lines(c(st, stp), c(y,y), col = col, lwd = 3)
  }
}
readhist = function(ind, chr, st, stp, ex = vector(), center = "yale", cell = "090112_YOAV_FC30MN9", dir = "/data/users/pickrell/eqtl/hapseq/genemapped_data/"){
	posfile = paste(dir, center, "/", cell, "/pos2read/", ind, ".", chr, ".out.gz", sep = "")
	d = read.table(gzfile(posfile), as.is = T)
	d = d[d[,2]>st & d[,2]<stp,] 
  	if (nrow(d)==0){
		print("no reads")
    	 	return(0)
  	}
	d = d[,c(2,3)]
	d[,3] = 0
	names(d) = c("pos", "count", "inex")
	pos = st:stp
  	tmp = data.frame(matrix(nrow = length(pos), ncol = 3))
	names(tmp) = names(d)
	tmp[,1] = pos
	tmp[,2] = 0
	t_not = tmp[tmp[,1] %in% d[,1]==F,]
  	tmp = rbind(t_not, d)
	tmp = tmp[order(tmp[,1]),]

	m = max(tmp[,2])
  	tmp2 = tmp
  	tmp[tmp[,1] %in% ex,3] = 1
  	tmp[tmp[,1] %in% ex == F,3] = 0
  	tmp2[tmp2[,1] %in% ex,3] = 1
	tmp2[tmp2[,2] %in% ex==F,3] = 0
	tmp[tmp[,3] ==0,2] = 0
  	tmp2[tmp2[,3]==1,2] = 0
  	row.names(tmp2) = as.character(tmp[,1])
	row.names(tmp) = as.character(tmp[,1])
  	barplot(tmp2[,2], xaxs = "i", col = "black", xlab = "", names.arg = F, ylim = c(0, m))
  	barplot(tmp[,2], col = "cadetblue", border = "cadetblue", add = T, xlab = "", names.arg = F)
}

plotregion = function(ind, ensembl, chr, st, stp, center = "yale", cell = "090112_YOAV_FC30MN9", dir = "/data/users/pickrell/eqtl/hapseq/genemapped_data/"){
  par(mfrow = c(2,1), mar = c(1,1,1,1), oma = c(5,4,1,1))
  ex = plotgenesinregion(ensembl, chr, st, stp, plot = F)
  r = readhist(ind, chr, st, stp, ex, center = center, cell = cell, dir = dir)
  ex = plotgenesinregion(ensembl, chr, st, stp)
}

plot.gene.region = function(ind, gene, ensembl, cell = "090112_YOAV_FC30MN9", dir = "/data/users/pickrell/eqtl/hapseq/genemapped_data/"){
  tmp = ensembl[ensembl[,13]==gene,]
  chr = tmp[1,3]
  st = min(tmp[,5])-1000
  stp = max(tmp[,6])+1000
  #print("here")
  t = plotregion(ind, ensembl, chr, st, stp, cell = cell, dir = dir)
  #return(t)
}

plot.by.geno = function(d, ensembl, chr, ln = vector(), maximum = 10){
	  counts = d[1,2:4]
	  counts = paste(counts[1], counts[2], counts[3])
	  d = d[-1,]
	  mi = min(as.numeric(d[,1]))
	  ma = max(as.numeric(d[,1]))
	  m = apply(d[,c(2,3,4)], 2, max)
	  m = max(m)
	  m = min(m, maximum)
  	  ex = plotgenesinregion(ensembl, chr, mi, ma, plot = F)
	  par(mfrow = c(4,1), mar = c(3,3,3,2), oma = c(3, 3, 1, 2))
	  geno_barplot(d[,c(1,2)], ex, m)
	  geno_barplot(d[,c(1,3)], ex, m)
	  geno_barplot(d[,c(1,4)], ex, m)
  	  ex = plotgenesinregion(ensembl, chr, mi, ma, ln = ln)
	  print(counts)
	  mtext(counts, side = 1, line = 3)
}

plot_from_dens = function(d, ensembl, chr, ln= vector(), scale = F, exons = data.frame()){
	       mi = min(d[,1])
       	       ma = max(d[,1])
	       m = max(d[,2])
	       if (scale){
	       	  m = m*1000000
	       }
	       par(mfrow = c(2,1))
	       ex = plotgenesinregion(ensembl, chr,mi, ma, plot = F)
	       ret = geno_barplot(d, ex, m, scale = scale)
	       ex = plotgenesinregion(ensembl, chr,mi, ma, ln= ln, plot = T, exons = exons)
	       return(ret)
}

#geno_barplot_rect = function(d, ex, m, scale = F){
#		  d[,3] = 0
#		  ylab = "mean fraction of reads"
#	     	  if (scale){
#	            d[,2] = d[,2]*1000000
#		    ylab = "mean rate (reads/million)"
#	     	    }
#	     	  d[d[,1] %in% ex,3] = d[d[,1] %in% ex,2]
#	     	  d[d[,1] %in% ex ==T,2] = 0
		  
geno_barplot =function(d, ex, m, scale = F, yl = T){
	     d[,3] = 0
	     ylab = "mean fraction of reads"
	     if (scale){
	         d[,2] = d[,2]*1000000
		 ylab = "mean rate (reads/million)"
	     }
	     d[d[,1] %in% ex,3] = d[d[,1] %in% ex,2]
	     d[d[,1] %in% ex ==T,2] = 0
	     row.names(d) = as.character(d[,1])
	     if (yl ==F){ ylab = ""}
  	     toret = barplot(d[,3], xaxs = "i", col = "cadetblue",border = "cadetblue",  xlab = "", names.arg = F, ylim = c(0, m), cex.lab = 0.9, ylab = ylab)
  	     barplot(d[,2], col = "black", border = "black", add = T, xlab = "", names.arg = F)	  
	     return(toret)
}

polya_plot = function(d, chr, st, sp){
	   d = d[d[,1]==chr & d[,2]>= st & d[,2] <sp,,drop = F]
	   if (nrow(d) > 0){
	      toplot = matrix(nrow = sp-st+1, ncol = 3)
	      toplot[,1] = st:sp
	      toplot[,2] = 0
	      toplot[,3] = 0
	      s = log10(d[,6])
	      m = max(s)
	      f = d[,7]/d[,6]
	      for(i in 1:nrow(d)){
	      	    if (f[i]>0.5){
	      	       toplot[toplot[,1]==d[i,2],2] = s[i]
 		    }else{
		       toplot[toplot[,1]==d[i,2],3] = s[i]
		    }
		}	    
	      barplot(toplot[,2], ylab = "log10(supporting reads)", xaxs = "i", ylim = c(0, m), col = "red", border = "red")
	      barplot(toplot[,3], add = T)
	      return(toplot)
	      }
	   
	  # return(d)
}
polya_andgene_plot = function(d, ens, chr, st, sp){
	  par(mfrow = c(2,1), mar = c(3,5,3,2), oma = c(3, 3, 1, 2))
          tmp = polya_plot(d, chr, st, sp)
 	  ex = plotgenesinregion(ens, chr, st, sp)
	  #return(tmp)
} 

polya_rna_plot = function(d, polya, ens, chr){
	  par(mfrow = c(3,1), mar = c(3,5,3,2), oma = c(3, 3, 1, 2))

	  d = d[-1,]
	  mi = min(as.numeric(d[,1]))
	  ma = max(as.numeric(d[,1]))
	  ex = plotgenesinregion(ens, chr, mi, ma, plot = F)
	  m = apply(d[,c(2,3,4)], 2, max)
	  m = max(m)
	  geno_barplot(d[,c(1,2)], ex, m)	      
          tmp = polya_plot(polya, chr, mi, ma)
  	  ex = plotgenesinregion(ens, chr, mi, ma)
}	  		  



polya_plot_new = function(d, chr, st, sp, m){	       
	   d = d[d[,1]==chr & d[,2]>= st & d[,2] <sp,,drop = F]
	   if (nrow(d) > 0){
	      toplot = matrix(nrow = sp-st+1, ncol = 3)
	      toplot[,1] = st:sp
	      toplot[,2] = 0
	      toplot[,3] = 0
	      f = d[,4]/d[,3]
	      for(i in 1:nrow(d)){
   	      	  if (f[i]>0.5){
	      	       toplot[toplot[,1]==d[i,2],2] = mean(as.numeric(d[i,5:ncol(d)]))
 		  }else{
		       toplot[toplot[,1]==d[i,2],3] = mean(as.numeric(d[i,5:ncol(d)]))
		 }
		}
		print(max(toplot[,2]))
   		barplot(toplot[,2], ylab = "fraction supporting reads", xaxs = "i", ylim = c(0, m), col = "red", border = "red")
	    	barplot(toplot[,3], add = T)	    
	      }
	  
}

polya_plot_by_geno = function(d, g, order, chr, st, sp, m){
		   names(g)[6:ncol(g)] = order[,1]
		   g= round(g[6:ncol(g)])
		   zero = names(d)[which(g==0)]
		   ones = names(d)[which(g==1)]
		   twos = names(d)[which(g==2)]
		   zero = zero[grep("NA", zero)]
		   ones = ones[grep("NA", ones)]
		   twos = twos[grep("NA", twos)]
		   zeroplot = d[,c(1,2,3,4, which(names(d) %in% zero))]
		   #return(zeroplot)
		   par(mfrow = c(3, 1))
		   polya_plot_new(d[,c(1,2,3,4, which(names(d) %in% zero))], chr, st, sp, m)
		   polya_plot_new(d[,c(1,2,3,4, which(names(d) %in% ones))], chr, st, sp, m)
		   polya_plot_new(d[,c(1,2,3,4, which(names(d) %in% twos))], chr, st, sp, m)


}

plot_juncreads = function(d, st, sp, filter = 1, basefilter = 6){
	d = d[d[,3]>st & d[,3]<sp,]
	#print(d)
	#t = table(d[,1])
	#t = t[t==2]
	#d = d[d[,1] %in% names(t),]
	t2 = table(d[,3])
	t2 = t2[t2>filter]
	d = d[d[,3] %in% as.numeric(names(t2)),]
	print(d)
	t2 = table(d[,3])
	#print(t2)
	#keep = rep(FALSE, length(t2))
	#for (i in 1:basefilter){
	#    tmp = as.numeric(names(t2))+i
	#    print(tmp)
	#    tmp=  tmp %in% d[,3]
	#    print(tmp)
	#    keep = keep+tmp
	#    tmp = as.numeric(names(t2))-i
	#    tmp = tmp %in% d[,3]
	#    keep = keep+tmp
	#}
	#print(keep)
	#t2 = t2[keep !=0]
	d = d[d[,3] %in% as.numeric(names(t2)),]
	t = table(d[,1])
	#t = t[t==2]
	#d = d[d[,1] %in% names(t),]
 	plot("NA", xaxs = "i", xlim = c(st,sp), ylim = c(0, 1), axes = F, ylab = "", xlab = "")
        axis(1, lab = T)
	for (i in 1:length(t)){
	      y = i*(1/length(t))
	      tmp = d[d[,1] == names(t)[i],]
	      p1 = tmp[1,3]
	      p12 = tmp[1,3]+20
	      p2 = tmp[2,3]
	      p22 = tmp[2,3]+20
	      if (is.na(tmp[2,3])){
	      	 lines(c(p1, p12), c(y,y), col = "red", lwd = 4)
              }
	      else if (abs(p12-p2)>19){
	      	 
		lines(c(p12, p2), c(y,y), col = "grey", lwd = 1)              
		lines(c(p1, p12), c(y,y), col = "black", lwd = 4)
		lines(c(p2, p22), c(y,y), col = "black", lwd = 4)

	      }
	 }	      
	 return(d)     
}

