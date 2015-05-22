#!/usr/bin/python

import sys, os

for i in range(10, 20):
	cmd = "python add_true_model.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".pwpheno.m4.c0.model sim"+str(i)+".m4.c0.split ../sim"+str(i)+"/sim"+str(i)+".m4.c0.split.segbfs.gz"
	print cmd
	os.system(cmd)
	cmd = "python add_true_model.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".pwpheno.m4.c0.model sim"+str(i)+".m4.c0.nosplit ../sim"+str(i)+"/sim"+str(i)+".m4.c0.nosplit.segbfs.gz"
	print cmd
	os.system(cmd)
