
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

### Other Stuff You Might Like

#### [ZM - Language independent, reproducible, absolute types](https://github.com/Quid2/zm)

To decode `flat` encoded data you need to know the type of the serialised data.

This is ok for applications that do not require long-term storage and that do not operate in open distributed systems.

For those who do, you might want to supplement `flat` with something like [ZM](https://github.com/Quid2/zm).

#### Ports for other languages

[TypeScript-JavaScript](https://github.com/Quid2/ts) and [Purescript](https://www.purescript.org/) ports are under development.

Get in touch if you would like to help porting `flat` to other languages.