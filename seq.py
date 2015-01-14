#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys, subprocess, time

f = open('seq.log', 'a+')
def print_all(s):
  print (s, file=f)
  f.flush()
  print (s, file=sys.stdout)

reps = 1
versions = ["short", "intermediate", "long"]
node = {"host": "127.0.0.1", "port": 5050}

print_all("Sequential Orbit")
print_all("----------------------------------------------------------------------")
print_all("Versions: %s" % versions)
print_all("Repetitions per Configuration: %s" % reps)
print_all("Node @ %s" % node)
print_all("======================================================================")

for vsn in versions:
  for rep in range(reps):
    print_all("Version: %s, Execution: %s" % (vsn, rep))
    t1 = time.time()
    cmd = "./orbit seq %s %s %s > /dev/null" % (vsn, node["host"], node["port"])
    p = subprocess.Popen(cmd, shell=True)
    p.wait()
    t2 = time.time()
    print_all("  %s sec(s)" % (t2 - t1))
