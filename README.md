Cloud-orbit
===========
Orbit for the cloud in Cloud Haskell!

This is a shameless translation from the original Orbit-int written in Erlang
by Patrick Maier <P.Maier@hw.ac.uk>.


Required Haskell packages
-------------------------

- [hashable](https://hackage.haskell.org/package/hashable)
- [dequeue](https://hackage.haskell.org/package/dequeue)
- [distributed-process](https://hackage.haskell.org/package/distributed-process)
- [network-transport-tcp](https://hackage.haskell.org/package/network-transport-tcp)
- [distributed-process-simplelocalnet](https://hackage.haskell.org/package/distributed-process-simplelocalnet)

Execution
---------

- Sequential Orbit
  ```bash
  ./orbit seq short|intermediate|long host port
  
  # Example: executing the 'long' benchmark
  ./orbit seq long 127.0.0.1 1555
  ```

- Parallel Orbit
  ```bash
  ./orbit par True|False short|intermediate|long nWorkers host port
  
  # Example: executing the 'long' benchmark with parallel image
  # computations using 4 workers on 4 cores
  ./orbit +RTS -N4 -RTS par True long 4 127.0.0.1 1555
  ```

- Distributed Orbit

  First, start the worker nodes.
  ```bash
  # Run for every worker node
  ./orbit dist slave host port
  
  # Example: run a worker node on 4 cores
  ./orbit +RTS -N4 -RTS dist slave 127.0.0.1 1554
  ```
  
  Once all the workers nodes have been deployed on the local network,
  start the master node.
  ```bash
  ./orbit dist master True|False short|intermediate|long nWorkers host port
  
  # Example: executing the 'long' benchmark without parallel image
  # computations, using 4 workers on each node and the master node
  # running on 2 cores
  ./orbit +RTS -N2 -RTS dist master long 4 127.0.0.1 1555
  ```

Memory Profiling
----------------

- Compile with enabled profiling
  ```bash
  make prof
  ```

- Run with the proper RTS options
  ```bash
  # Example: executing the 'long' benchmark
  ./orbit  +RTS -hd -p -RTS seq long 127.0.0.1 1555
  ```

- Prepare the report
  ```bash
  make prof-results
  ```

Profiling with Threadscope
--------------------------

- Compile with enabled event logging
  ```bash
  make threadscope
  ```

- Run with the proper RTS options
  ```bash
  # Example: executing the 'long' benchmark
  ./orbit +RTS -ls -RTS seq long 127.0.0.1 1555
  ```

- Open the report with threadscope
  ```bash
  threadscope orbit.eventlog
  ```

Credits
-------
Written by:

- Yiannis Tsiouris <gtsiour@softlab.ntua.gr>
- Aggelos Giantsios <aggelgian@softlab.ntua.gr>
