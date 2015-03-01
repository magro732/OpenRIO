This project started when Bombardier Transportation Sweden decided to donate some RapidIO-IP blocks that were developed in an internal project. It was initialy hosed on http://opencores.org/project,rio but has now moved to GitHub to take advantage of all the Git features.

The cores that were initially commited were aiming to use RapidIO in proprietary systems with a legazy infrastructure and did not support the full standard PCS (Physical Coding Sublayer) implementation. It instead showed a way of building RapidIO fully compliant networks based on non-RapidIO compliant, low-speed, UART links. This shows that the RapidIO protocol is very flexible and can be scaled not only up in bandwidth but also down.

Since the initial releases an effort has started to write a fully compliant PCS layer.

Currently this project contains VHDL IP-blocks to create stand-alone RapidIO-endpoints, RapidIO-switches and RapidIO-switches with local endpoints. These three configuration can be acheived by changing how the IP-blocks in the project are interconnected.
