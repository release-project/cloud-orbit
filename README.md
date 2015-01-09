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

Memory Profiling
----------------

- Compile with enabled profiling
  ```bash
  make prof
  ```

- Run the proper RTS options
  ```bash
  # Example: executing the 'long' benchmark
  ./orbit  +RTS -hd -p -RTS seq long 127.0.0.1 1555
  ```

- Prepare the report
  ```bash
  make prof-results
  ```

Credits
-------
Written by:

- Yiannis Tsiouris <gtsiour@softlab.ntua.gr>
- Aggelos Giantsios <aggelgian@softlab.ntua.gr>
