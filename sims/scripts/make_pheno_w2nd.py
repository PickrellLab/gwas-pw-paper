#!/usr/bin/python

import sys, os, re, gzip, random

indir = sys.argv[1]
outfile = open(sys.argv[2], "w")
causalfile = open(sys.argv[3])
causalfile2 = open(sys.argv[4])

file2index = dict()
file2index2 = dict()

causalcount = list()
causalgeno = list()
causalcount2 = list()
causalgeno2 = list()
files = list()
for i in range(5000):
	causalcount.append(0)
	causalgeno.append("")
	causalcount2.append(0)
	causalgeno2.append("")

for line in causalfile.xreadlines():
	print line
	line = line.strip().split()
	file = line[6].split(".")
	file = ".".join(file[:4])
	index = int(line[5])
	file2index[file] = index	

print file2index

for line in causalfile2.xreadlines():
        print line
        line = line.strip().split()
        file = line[6].split(".")
        file = ".".join(file[:4])
        index = int(line[5])
        file2index2[file] = index

ff = os.listdir(indir)
for f in ff:
	if re.search(".controls.haps.gz$", f) == None:
		continue
	tmp = f.split(".")
	tmp = ".".join(tmp[:4])
	index2find  = file2index[tmp]
	index2find2 = file2index2[tmp]
	index = 1
	print f
	files.append(f)
	file = gzip.open(indir+f)
	line = file.readline()
	while line:
		if index == index2find:
			print f, index,  index2find
			line = line.strip().split()
			for i in range(5000):
				h0 = int(line[2*i])
				h1 = int(line[2*i+1])
				causalcount[i] = causalcount[i]+h0+h1
				causalgeno[i] = causalgeno[i]+str(h0+h1)
                elif index == index2find2:
                        print f, index,  index2find2
                        line = line.strip().split()
                        for i in range(5000):
                                h0 = int(line[2*i])
                                h1 = int(line[2*i+1])
                                causalcount2[i] = causalcount2[i]+h0+h1
                                causalgeno2[i] = causalgeno2[i]+str(h0+h1)
		index = index+1
		line = file.readline()

#print the counts of the causal snp1 and the causal snp2
print >> outfile, "ID COUNT P COUNT2",
print >> outfile, " ".join(files)
for i in range(5000):
	print >> outfile, "ind"+str(i), causalcount[i], 
	p = 0.1*float(causalcount[i])+random.gauss(0, 0.7745967)
	print >> outfile, p,
	print >> outfile, causalcount2[i],
	for j in range(len(causalgeno[i])):
		print >> outfile, causalgeno[i][j] +causalgeno2[i][j],
	print >> outfile, ""

