#!/usr/bin/python

import sys, os, gzip

infile = gzip.open(sys.argv[1])
outfile = open(sys.argv[2], "w")

chunk2best=  dict()
line = infile.readline()
line = infile.readline()
maxc =0

THOLD1 = 5.45
THOLD2 = 4.12
while line:
	line = line.strip().split()
	seg = line[9]
	snpid = line[0]
	Z1 = float(line[4])
	Z2 = float(line[6])
	if chunk2best.has_key(seg) == 0:
		if int(seg)> maxc:
			maxc = int(seg)
		chunk2best[seg] = dict()
		chunk2best[seg]["bestsnp1"] = dict()
		chunk2best[seg]["bestsnp2"] = dict()
		chunk2best[seg]["bestsnp1"]["rs"] = snpid
		chunk2best[seg]["bestsnp1"]["Z1"] = Z1
		chunk2best[seg]["bestsnp1"]["Z2"] = Z2
		chunk2best[seg]["bestsnp2"]["rs"] = snpid
		chunk2best[seg]["bestsnp2"]["Z1"] = Z1
		chunk2best[seg]["bestsnp2"]["Z2"] = Z2
	if abs(Z1) > abs(chunk2best[seg]["bestsnp1"]["Z1"]):
		chunk2best[seg]["bestsnp1"]["rs"] = snpid
		chunk2best[seg]["bestsnp1"]["Z1"] = Z1
		chunk2best[seg]["bestsnp1"]["Z2"] = Z2
	if abs(Z2) > abs(chunk2best[seg]["bestsnp2"]["Z2"]):
                chunk2best[seg]["bestsnp2"]["rs"] = snpid
                chunk2best[seg]["bestsnp2"]["Z1"] = Z1
                chunk2best[seg]["bestsnp2"]["Z2"] = Z2
	line = infile.readline()

sm1 = 0
sm2 = 0
sm3 = 0
sm4 = 0
print >> outfile, "chunk best1 best1_Z1 best1_Z2 best2 best2_Z1 best2_Z2 ism3 ism4 ism1 ism2"
for i in range( maxc+1):
	ss =str(i)
	c = chunk2best[ss]
	im3 = 0
	im4 = 0
	print >> outfile, i, c["bestsnp1"]["rs"], c["bestsnp1"]["Z1"],c["bestsnp1"]["Z2"],c["bestsnp2"]["rs"],c["bestsnp2"]["Z1"],c["bestsnp2"]["Z2"],
	if (abs(c["bestsnp1"]["Z1"]) > THOLD1 and abs(c["bestsnp1"]["Z2"] > THOLD2)) or (abs(c["bestsnp2"]["Z2"]) > THOLD1 and abs(c["bestsnp2"]["Z1"] > THOLD2)):
		print >> outfile, 1,
		im3 = 1
		sm3 = sm3+1
	else:
		print >> outfile, 0,
	if  abs(c["bestsnp2"]["Z2"]) > THOLD1 and abs(c["bestsnp1"]["Z1"]) > THOLD1  and im3 == 0:
		print >> outfile, 1,
		im4 = 1
		sm4 = sm4+1
	else:
		print >> outfile, 0,
	if abs(c["bestsnp1"]["Z1"]) > THOLD1  and im3 ==  0 and im4 == 0:
		print >> outfile, 1, 
		sm1 = sm1+1
	else:
		print >> outfile, 0,
	if abs(c["bestsnp2"]["Z2"]) > THOLD1  and im3 ==  0 and im4 == 0:
                print >> outfile, 1
		sm2 = sm2+1
        else:
                print >> outfile, 0
sm0 = 111-sm1-sm2-sm3-sm4
print >> outfile, "# p0 ", float(sm0)/111
print >> outfile, "# p1 ", float(sm1)/111
print >> outfile, "# p2 ", float(sm2)/111
print >> outfile, "# p3 ", float(sm3)/111
print >> outfile, "# p4 ", float(sm4)/111
		
	
