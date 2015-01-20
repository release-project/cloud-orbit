#!/usr/bin/env python
# -*- coding: utf-8 -*-

import itertools, math, re, sys, uuid
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as nps

class Conf(object):
  """
  A configuration scenario
  """
  def __init__(self, n):
    self.exectime = []
    self.nodes = n

  def __str__(self):
    return ("Nodes: %s" % self.nodes)
  
  def add_exectime(self, exectime):
    self.exectime.append(exectime)

  def get_exectime(self, x):
    """
    For now, returns the average min exectime
    """
    l = self.exectime
    if x == "min":
      return min(l)
    elif x == "avg":
      return sum(l) / float(len(l))
    elif x == "med":
      return sorted(l)[1]

def make_plot(x, lbls, ys, xaxis_label, yaxis_label, fname):
  fig = plt.figure()
  ax = fig.add_subplot(111)
  ax.set_xlabel(xaxis_label)
  ax.set_ylabel(yaxis_label)
  ls = []
  for (y,ylbl) in ys:
    line, = ax.plot(x, y, '--o', lw=2, ms=8, label=ylbl)
    ls.append(line)
  plt.legend(ls)
  ax.set_xticks(x)
  ax.set_xticklabels(lbls)
  ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%0.0f'))
  plt.xticks(fontsize=8)
  plt.yticks(fontsize=9)
  plt.tight_layout()
#  plt.show()
  plt.savefig(fname)

if len(sys.argv) < 3:
  print "Too few arguments..."
  print "Usage: ./plot.py seq-statistics-file dist-statistics-file"
  sys.exit(1)

# Parse the file with the seq statistics
seq = None
with open(sys.argv[1]) as f:
  s = f.read()
  seq = float(re.search(r'([\d\.]+) sec', s, re.I).group(1))
  f.close()

# Parse the file with the dist statistics
curr, confs = None, {}
with open(sys.argv[2]) as f:
  for l in f.readlines():
    # Declaration of a configuration
    if l.startswith("Slaves:"):
      n = int(re.search(r'Slaves: (\d+),', l, re.I).group(1))
      if n in confs:
        cf = confs[n]
      else:
        cf = Conf(n)
      curr = cf
    elif curr != None:
      t = float(re.search(r'([\d\.]+) sec', l, re.I).group(1))
      curr.add_exectime(t)
      confs[curr.nodes] = curr
      curr = None
  f.close()

x = sorted([float(k) for k in confs.keys()])
lbls, y, yy, yyy = [], [seq], [seq], [seq]
z, zz, zzz = [], [], []
for k in x:
  i = int(k)
  lbls.append(str(i))
  mmin = confs[i].get_exectime("min")
  y.append(mmin)
  z.append(seq / mmin)
  avg = confs[i].get_exectime("avg")
  yy.append(avg)
  zz.append(seq / avg)
  med = confs[i].get_exectime("med")
  yyy.append(med)
  zzz.append(seq / med)
xaxis_label = "Nodes"
yaxis_label = "Execution time (sec)"
yaxis_label2 = "Speedup"

make_plot([0.0] + x, [0] + lbls, [(y,"min"),(yy,"avg"),(yyy,"med")], xaxis_label, yaxis_label, "exectime.png")
make_plot(x, lbls, [(z,"min"),(zz,"avg"),(zzz,"med")], xaxis_label, yaxis_label2, "speedup.png")

