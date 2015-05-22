#!/usr/bin/python

import sys, os

for i in range(100):
	#cmd = "python make_pheno_w2nd.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".pheno ../../subset/causals.txt ../../subset/causals2.txt"
	cmd = "echo 'python make_pheno_w2nd.py ../sim"+str(i)+"/ ../sim"+str(i)+"/sim"+str(i)+".pheno ../../subset/causals.txt ../../subset/causals2.txt' | qsub -V -cwd -q res.q -o /dev/null -e "+str(i)+".e"
	print cmd
	os.system(cmd)
