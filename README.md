
[![Build Status](https://travis-ci.org/Quid2/flat.svg?branch=master)](https://travis-ci.org/Quid2/flat)
[![Hackage version](https://img.shields.io/hackage/v/flat.svg)](http://hackage.haskell.org/package/flat)

Haskell implementation of [Flat](http://quid2.org/docs/Flat.pdf), a principled, language-independent and efficient binary data format.

### Performance

For some hard data, see this [comparison of the major haskell serialisation libraries](https://github.com/haskell-perf/serialization).

Briefly:
 * Transfer time (serialisation time + transport time on the network + deserialisation at the receiving end): `flat` is usually faster for all but the highest network speeds
 * Size: `flat` produces significantly smaller binaries than all other libraries (3/4 times usually)
 * Serialization: `store`, `persist` and `flat` are faster
 * Deserialization: `store`, `flat`, `persist` and `cereal` are faster

### Documentation

* [Tutorial](docs/src/Flat-Tutorial.html)

* [Full Package Docs](docs/src)

* [Flat Format Specification](http://quid2.org)

### Installation

Get the latest stable version from [hackage](https://hackage.haskell.org/package/flat).

### Acknowledgements

 `flat` reuses ideas and readapts code from various packages, mainly: `store`, `binary-bits` and `binary` and includes contributions from Justus Sagemüller.