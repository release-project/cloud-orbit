#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys, subprocess, time

f = open('dist.log', 'a+')
def print_all(s):
  print (s, file=f)
  f.flush()
  print (s, file=sys.stdout)

reps = 1
versions = ["short", "intermediate", "long"]
iwps = [False, True]
cores = [1]
workersPerCore = 1
master = {"host": "127.0.0.1", "port": 5050}
slaves = [ {"host": "127.0.0.1", "port": 5051}
         , {"host": "127.0.0.1", "port": 5052}
         , {"host": "127.0.0.1", "port": 5053}
         , {"host": "127.0.0.1", "port": 5054}
         , {"host": "127.0.0.1", "port": 5055}
         , {"host": "127.0.0.1", "port": 5056}
         , {"host": "127.0.0.1", "port": 5057}
         , {"host": "127.0.0.1", "port": 5058}
         ]

print_all("Parallel Orbit")
print_all("----------------------------------------------------------------------")
print_all("Versions: %s" % versions)
print_all("Parallel Image Computation: %s" % iwps)
print_all("Repetitions per Configuration: %s" % reps)
print_all("Using Cores: %s" % cores)
print_all("Workers Per Core: %s" % workersPerCore)
print_all("Master @ %s" % master)
print_all("Slaves @ %s" % slaves)
print_all("======================================================================")

for n in range(1, len(slaves)+1):
  for iwp in iwps:
    for vsn in versions:
      for core in cores:
        for rep in range(reps):
          workers = workersPerCore * core
          slvs = slaves[0:n]
          print_all("Slaves: %s, Version: %s, IWP: %s, Cores: %s, Workers: %s, Execution: %s" % (n, vsn, iwp, core, workers, rep))
          for slv in slvs:
            cmd = "./orbit +RTS -N%s -RTS dist slave %s %s > /dev/null" % (core, slv["host"], slv["port"])
            prcs = subprocess.Popen(cmd, shell=True)
          t1 = time.time()
          cmd = "./orbit +RTS -N%s -RTS dist master %s %s %s %s %s > /dev/null" % (n, iwp, vsn, workers, master["host"], master["port"])
          p = subprocess.Popen(cmd, shell=True)
          p.wait()
          t2 = time.time()
          print_all("  %s sec(s)" % (t2 - t1))
          
