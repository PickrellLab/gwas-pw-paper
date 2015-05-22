#!/usr/bin/python

import sys, os

stem = sys.argv[1]
outfile = sys.argv[2]

cmd = "head -n 1 "+stem+"0.1.9835.19835.m4.c0.nosplit.Z  | gzip - > "+sys.argv[2]
print cmd
os.system(cmd)
#cmd = "cat "+stem+"*m4.c0.nosplit.Z | grep -v NA | grep -v SEGNUMBER | sort -k 10n -k3n | gzip - >> "+sys.argv[2]
#cmd = "cat "+stem+"*m4.c0.split.Z | grep -v NA | grep -v SEGNUMBER | sort -k 10n -k3n | gzip - >> "+sys.argv[2]
cmd = "cat "+stem+"*m4.c5.nosplit.Z | grep -v NA | grep -v SEGNUMBER | sort -k 10n -k3n | gzip - >> "+sys.argv[2]
#cmd = "cat "+stem+"*m4.c5.split.Z | grep -v NA | grep -v SEGNUMBER | sort -k 10n -k3n | gzip - >> "+sys.argv[2]
print cmd
os.system(cmd)
