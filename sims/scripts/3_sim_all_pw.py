#!/usr/bin/python

import sys, os

for i in range(100):
	cmd = "echo 'R --vanilla --args ../sim"+str(i)+"/sim"+str(i)+".pheno ../sim"+str(i)+"/sim"+str(i)+".pwpheno.m4.c0 0.25 0.25 0.25 0.25 0 < sim_pw_pheno_wm4.R' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	print cmd
	os.system(cmd)	
	cmd = "echo 'R --vanilla --args ../sim"+str(i)+"/sim"+str(i)+".pheno ../sim"+str(i)+"/sim"+str(i)+".pwpheno.m4.c5 0.25 0.25 0.25 0.25 0.5 < sim_pw_pheno_wm4.R' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"	
	print cmd
	os.system(cmd)
