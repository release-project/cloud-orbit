#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess, time

reps = 1
versions = ["short", "intermediate", "long"]
node = {"host": "127.0.0.1", "port": 5050}

print "Sequential Orbit"
print "----------------------------------------------------------------------"
print "Versions: %s" % versions
print "Repetitions per Configuration: %s" % reps
print "Node @ %s" % node
print "======================================================================"

for vsn in versions:
  for rep in range(reps):
    print "Version: %s, Execution: %s" % (vsn, rep)
    t1 = time.time()
    cmd = "./orbit seq %s %s %s > /dev/null" % (vsn, node["host"], node["port"])
    p = subprocess.Popen(cmd, shell=True)
    p.wait()
    t2 = time.time()
    print "  %s sec(s)" % (t2 - t1)
