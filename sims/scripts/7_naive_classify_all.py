#!/usr/bin/python

import sys, os

for i in range(100):
	cmd = "echo 'python naive_classify.py ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c0.split.gz ../sim"+str(i)+"/sim"+str(i)+".m4.c0.split.naive' | qsub -V -cwd -o /dev/null -e /dev/null"
	#cmd = "echo 'python naive_classify.py ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c0.nosplit.gz ../sim"+str(i)+"/sim"+str(i)+".m4.c0.nosplit.naive' | qsub -V -cwd -o /dev/null -e /dev/null"
	#cmd = "echo 'python naive_classify.py ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.split.gz ../sim"+str(i)+"/sim"+str(i)+".m4.c5.split.naive' | qsub -V -cwd -o /dev/null -e /dev/null"
	#cmd = "echo 'python naive_classify.py ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.nosplit.gz ../sim"+str(i)+"/sim"+str(i)+".m4.c5.nosplit.naive' | qsub -V -cwd -o /dev/null -e /dev/null"
	print cmd
	os.system(cmd)
