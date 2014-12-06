#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import subprocess, time

def run_dist(master, slaves, version, workers, iwp):
  # Setup the slave nodes
  for slave in slaves:
    cmd = "./orbit +RTS -N2 -RTS dist slave %s %s" % (slave["host"], slave["port"])
    process = subprocess.Popen(cmd, shell=True)
  time.sleep(2)
  # Run the master node
  t1 = time.time()
  cmd = "./orbit dist master %s %s %s %s %s" % (
    iwp, version, workers, master["host"], master["port"]
  )
  p = subprocess.Popen(cmd, shell=True)
  p.wait()
  t2 = time.time()
  return t2 - t1

# Configuration

reps = 2
iwp = False
max_cpu = 16
versions = ["short", "intermediate", "long"]
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

f = open('statistics.txt', 'w')

### 1 host

for nSlaves in range(1, len(slaves)+1):
  for iwp in [False, True]:
    for vsn in versions:
      for n in range(2, 2*max_cpu+1, 2):
        slvs = slaves[0:nSlaves]
        print ("Slaves: %s, Workers: %s, Version: %s, IWP: %s" % (nSlaves, n, vsn, iwp), file=f)
        ts = []
        for _ in range(reps):
          t = run_dist(master, slvs, vsn, n, iwp)
          ts.append(t)
        print ("%s" % ts, file=f)
        f.flush()

f.close()
