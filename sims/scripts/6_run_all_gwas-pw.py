#!/usr/bin/python

import sys, os

for i in range(100):
	#cmd = "echo 'gwas-pw -i ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.split.gz -phenos 1 2 -numbered -v 0.5 -print -o ../sim"+str(i)+"/sim"+str(i)+".m4.c5.split.noc' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	#cmd = "echo 'gwas-pw -i ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.nosplit.gz -phenos 1 2 -numbered -v 0.5 -print -o ../sim"+str(i)+"/sim"+str(i)+".m4.c5.nosplit.noc' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	cmd = "echo 'gwas-pw -i ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.split.gz -phenos 1 2 -numbered -v 0.5 -print -o ../sim"+str(i)+"/sim"+str(i)+".m4.c5.split' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	#cmd = "echo 'gwas-pw -i ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.nosplit.gz -phenos 1 2 -cor 0.3 -numbered -v 0.5 -print -o ../sim"+str(i)+"/sim"+str(i)+".m4.c5.nosplit' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	#cmd = "echo 'gwas-pw -i ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c0.split.gz -phenos 1 2 -numbered -v 0.5 -print -o ../sim"+str(i)+"/sim"+str(i)+".m4.c0.split' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	#cmd = "echo 'gwas-pw -i ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c0.nosplit.gz -phenos 1 2 -numbered -v 0.5 -print -o ../sim"+str(i)+"/sim"+str(i)+".m4.c0.nosplit' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	print cmd
	os.system(cmd)

