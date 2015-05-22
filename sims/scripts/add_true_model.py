#!/usr/bin/python

import sys, os, gzip

stem = sys.argv[1]
infile = open(sys.argv[2])
outfile = open(sys.argv[3], "w")
bffile = gzip.open(sys.argv[4])

f2model = dict()

for line in infile.xreadlines():
	line = line.strip().split()
	f = line[0]
	model = line[2]
	f = f[1:]
	f = f.split(".")
	f = ".".join(f[:4])
	f = stem+f+".m4.c0.split.Z"
	f = open(f)
	line2 = f.readline()
	line2 = f.readline()
	line2 = line2.strip().split()
	m = line2[9]
	f2model[m] = model

print f2model 

line = bffile.readline().strip()
print >> outfile, line, "truemodel"
line = bffile.readline()
while  line:
	line = line.strip().split()
	chunk = line[0]
	model = f2model[chunk]
	print >> outfile, " ".join(line), model
	line = bffile.readline()
