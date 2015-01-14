#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess, time

reps = 1
versions = ["short", "intermediate", "long"]
iwps = [False, True]
cores = [1,2,4]
workersPerCore = 1
node = {"host": "127.0.0.1", "port": 5050}

print "Parallel Orbit"
print "----------------------------------------------------------------------"
print "Versions: %s" % versions
print "Parallel Image Computation: %s" % iwps
print "Repetitions per Configuration: %s" % reps
print "Using Cores: %s" % cores
print "Workers Per Core: %s" % workersPerCore
print "Node @ %s" % node
print "======================================================================"

for iwp in iwps:
  for vsn in versions:
    for core in cores:
      for rep in range(reps):
        workers = workersPerCore * core
        print "Version: %s, IWP: %s, Cores: %s, Workers: %s, Execution: %s" % (vsn, iwp, core, workers, rep)
        t1 = time.time()
        cmd = "./orbit +RTS -N%s -RTS par %s %s %s %s %s > /dev/null" % (core, iwp, vsn, workers, node["host"], node["port"])
        p = subprocess.Popen(cmd, shell=True)
        p.wait()
        t2 = time.time()
        print "  %s sec(s)" % (t2 - t1)


