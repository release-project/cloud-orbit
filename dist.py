#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys, subprocess, time, os.path

# Path to the file that will hold the results.
fname = 'dist.log'
fexists = os.path.isfile(fname)
if fexists:
  f = open(fname, 'r')
  ls = f.read().splitlines()
  f.close()
else:
  ls = []

f = open(fname, 'a+')
def print_all(s):
  print (s, file=f)
  f.flush()
  print (s, file=sys.stdout)

# Number of repetitions per configuration.
reps = 1
# List of versions.
versions = ["short", "intermediate", "long"]
# Perform parallel image computations.
iwps = [False, True]
# No of cores used by each worker node.
cores = [1]
# Ratio of No of workers to No of cores in each worker node.
workersPerCore = 1
# Master node information
master = {"host": "127.0.0.1", "port": 5050}
# Maximum number of workers nodes
maxSlaves = 60
# No of workers nodes to add after each iteration
step = 2
# Port of the 1st worker node (Assume that the host will be localhost).
workerPort = 5051
allAvailableSlaves = []
for port in range(workerPort, workerPort+maxSlaves):
  allAvailableSlaves.append({"host": "127.0.0.1", "port": port})
slaves = []
for n in range(2, maxSlaves+1, step):
  slaves.append(allAvailableSlaves[0:n])

if len(ls) == 0: 
  print_all("Distributed Orbit")
  print_all("----------------------------------------------------------------------")
  print_all("Versions: %s" % versions)
  print_all("Parallel Image Computation: %s" % iwps)
  print_all("Repetitions per Configuration: %s" % reps)
  print_all("Using Cores: %s" % cores)
  print_all("Workers Per Core: %s" % workersPerCore)
  print_all("Master @ %s" % master)
  print_all("Slaves @ %s" % [len(xs) for xs in slaves])
  print_all("======================================================================")


for iwp in iwps:
  for vsn in versions:
    for slvs in slaves:
      for core in cores:
        for rep in range(reps):
          workers = workersPerCore * core
          n = len(slvs)
          l = "Slaves: %s, Version: %s, IWP: %s, Cores: %s, Workers: %s, Execution: %s" % (n, vsn, iwp, core, workers, rep)
          if l in ls:
            continue
          print_all("Slaves: %s, Version: %s, IWP: %s, Cores: %s, Workers: %s, Execution: %s" % (n, vsn, iwp, core, workers, rep))
          time.sleep(2)
          for slv in slvs:
            cmd = "./orbit +RTS -N%s -RTS dist slave %s %s > /dev/null" % (core, slv["host"], slv["port"])
            prcs = subprocess.Popen(cmd, shell=True)
          time.sleep(2)
          t1 = time.time()
          cmd = "./orbit +RTS -N1 -RTS dist master %s %s %s %s %s > /dev/null" % (iwp, vsn, workers, master["host"], master["port"])
          p = subprocess.Popen(cmd, shell=True)
          p.wait()
          t2 = time.time()
          print_all("  %s sec(s)" % (t2 - t1))
          
