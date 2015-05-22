#!/usr/bin/python

import sys, os

cmd = "cat *.split | awk '{if ($20 == 1)print $0}' > bfs_split1"
print cmd
os.system(cmd)
cmd = "cat *.split | awk '{if ($20 == 2)print $0}' > bfs_split2"
print cmd
os.system(cmd)
cmd = "cat *.split | awk '{if ($20 == 3)print $0}' > bfs_split3"
print cmd
os.system(cmd)
cmd = "cat *.split | awk '{if ($20 == 4)print $0}' > bfs_split4"
print cmd
os.system(cmd)
cmd = "cat *.nosplit | awk '{if ($20 == 1)print $0}' > bfs_nosplit1"
print cmd
os.system(cmd)
cmd = "cat *.nosplit | awk '{if ($20 == 2)print $0}' > bfs_nosplit2"
print cmd
os.system(cmd)
cmd = "cat *.nosplit | awk '{if ($20 == 3)print $0}' > bfs_nosplit3"
print cmd
os.system(cmd)
cmd = "cat *.nosplit | awk '{if ($20 == 4)print $0}' > bfs_nosplit4"
print cmd
os.system(cmd)
