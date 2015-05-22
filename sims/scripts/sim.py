#!/usr/bin/python

import sys, os, re

stem = sys.argv[1]

ff = os.listdir("../../subset/")

for f in ff:
	if re.search(".hap", f) != None:
		tmp = f.split(".")
		c = tmp[1]
		outfile = stem+f+".controls.haps.gz"
		print outfile
		if os.path.isfile(outfile):
			continue
		lff = ".".join(tmp[:(len(tmp)-1)])
		lf = open("../../subset/"+lff +".legend_wf")
		line = lf.readline()
		line = lf.readline()
		line = line.strip().split()
		p = line[1]
		fr = float(line[4])
		while fr < 0.05 or fr > 0.95:
			line = lf.readline()
			line = line.strip().split()
			p = line[1]
			fr = float(line[4])
		cmd = "echo 'hapgen2 -m /nethome/jkpickrell/projects/gwas_sims/hapgen2/genmaps/genetic_map_chr"+c+"_combined_b36.txt  -l ../../subset/"+lff+".legend -h ../../subset/"+f+" -o "+stem+f+".gz  -dl "+p+" 0 2 4 -n 5000 2' | qsub -V -cwd -q res.q -o /dev/null -e /dev/null"
		print cmd
		os.system(cmd)
