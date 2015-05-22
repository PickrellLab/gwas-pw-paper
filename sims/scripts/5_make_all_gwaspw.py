#!/usr/bin/python

import sys, os

for i in range(100):
	#cmd = "echo 'python make_fgwas_input_pw.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c0.nosplit.gz' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	#cmd = "echo 'python make_fgwas_input_pw.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c0.split.gz' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	cmd = "echo 'python make_fgwas_input_pw.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.nosplit.gz' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	#cmd = "echo 'python make_fgwas_input_pw.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".fgwasin.m4.c5.split.gz' | qsub -V -cwd -q res.q -e /dev/null -o /dev/null"
	print cmd
	os.system(cmd)
