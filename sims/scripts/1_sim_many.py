#!/usr/bin/python

import sys, os

for i in range(100):
	cmd = "mkdir ../sim"+str(i)
	os.system(cmd)
	cmd = "python sim.py ../sim"+str(i)+"/"
	os.system(cmd)
