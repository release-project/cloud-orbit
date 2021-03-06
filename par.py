#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys, subprocess, time

# Path to the file that will hold the results.
fname = 'par.log'
f = open(fname, 'a+')
def print_all(s):
  print (s, file=f)
  f.flush()
  print (s, file=sys.stdout)

# Number of repetitions per configuration.
reps = 1
# List of versions.
versions = ["short", "intermediate", "long"]
# Perform parallel image computations
iwps = [False, True]
# No of cores used
cores = [1,2,4,8,16]
# Ratio of No of workers to No of cores.
workersPerCore = 1
# Host information.
node = {"host": "127.0.0.1", "port": 5050}

print_all("Parallel Orbit")
print_all("----------------------------------------------------------------------")
print_all("Versions: %s" % versions)
print_all("Parallel Image Computation: %s" % iwps)
print_all("Repetitions per Configuration: %s" % reps)
print_all("Using Cores: %s" % cores)
print_all("Workers Per Core: %s" % workersPerCore)
print_all("Node @ %s" % node)
print_all("======================================================================")

for iwp in iwps:
  for vsn in versions:
    for core in cores:
      for rep in range(reps):
        workers = workersPerCore * core
        print_all("Version: %s, IWP: %s, Cores: %s, Workers: %s, Execution: %s" % (vsn, iwp, core, workers, rep))
        t1 = time.time()
        cmd = "./orbit +RTS -N%s -RTS par %s %s %s %s %s > /dev/null" % (core, iwp, vsn, workers, node["host"], node["port"])
        p = subprocess.Popen(cmd, shell=True)
        p.wait()
        t2 = time.time()
        print_all("  %s sec(s)" % (t2 - t1))


