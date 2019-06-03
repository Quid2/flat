# For internal testing only

dev: 
	stack test :spec --file-watch --fast

clean:
	 find . -name "*.dump*" -exec rm -- {} +
	 cd src;find . -name "*.o" -delete;find . -name "*.hi" -delete;find . -name "*.dyn_*" -delete;find . -name "*.dump-*" -delete
	 cd test;find . -name "*.o" -delete;find . -name "*.hi" -delete;find . -name "*.dyn_*" -delete


opt=2 -DENUM_LARGE

speed : cspeed rspeed

cspeed:
	# /usr/bin/time -l stack build :compy
	# touch test/Test/E.hs
	# touch test/Test/E/Binary.hs
	touch test/Test/E/Flat.hs
	#cd test;/usr/bin/time -l stack ghc -- -cpp -i../src -O$(opt) Test.E;/usr/bin/time -l stack ghc -- -i../src -cpp  -O$(opt) Test.E.Binary;/usr/bin/time -l stack ghc -- -i../src -cpp -O$(opt) Test.E.Flat
	cd test;/usr/bin/time -l stack ghc -- -cpp -i../src -O$(opt) Test.E;/usr/bin/time -l stack ghc -- -i../src -cpp -O$(opt) Test.E.Flat	

tspeed:
	cd test;/usr/bin/time -l stack ghc -- -cpp -i../src -O$(opt) Spec;

tst: 
	/usr/bin/time -l stack test :spec

jtst:
	stack test :spec --fast

rspeed: 
	stack bench :sbench
	ls -l /tmp/dump/test/Test/E/Flat.dump-simpl

bench: 
	stack bench :miniBench  --file-watch

jstest:
	stack clean;stack test --fast --file-watch --stack-yaml=stack-ghcjs-9.21.yaml  --ghc-options "-UENUM_LARGE -UTEST_DECBITS"

jsbench:
	stack bench :sbench --stack-yaml=stack-ghcjs.yaml

jsprof:
	node --prof /Users/titto/workspace/flat/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/spec/spec
	node --prof-process isolate-0x102801c00-v8.log > processed.txt

docs:
	stack haddock --no-haddock-deps --open

cab:
	cabal sandbox init
	#cabal configure --enable-tests --enable-benchmarks
	cabal install --only-dependencies --enable-tests --enable-benchmarks
	cabal test
	cabal bench	
	# cabal sandbox delete

eta:
	#etlas update;etlas select latest
	#- etlas sandbox delete
	#rm -rf dist 
	#etlas sandbox init;etlas configure --enable-tests --enable-benchmarks
	etlas build --enable-tests --enable-benchmarks
	# etlas install --dependencies-only
	etlas test
	# etlas bench
	#etlas build --enable-tests --enable-benchmarks;/Users/titto/workspace/flat/dist/build/eta-0.8.6.2/flat-0.3.5/b/miniBench/build/miniBench/miniBench
	etlas run listTest
	echo "Remove doctest from cabal!"

trep:
	git clone https://github.com/Quid2/flat;cd flat;git checkout d66cc7fc6880b50d81730fb9c075f7681889064f

	etlas run listTest

	# build benchmarks (this is built correctly)
	etlas sandbox init;etlas configure --enable-tests;etlas build



rep:
	# get a project with a benchmark
	git clone https://github.com/Quid2/flat;cd flat;git checkout a2d064ec0ed5201168c5a26e63b686f33e185e34

	# build benchmarks (this is built correctly)
	etlas sandbox init;etlas configure --enable-benchmarks;etlas build

	# can be run 
	dist/build/eta-0.8.6.2/flat-0.3.5/b/miniBench/build/miniBench/miniBench

	# run it (missing dependencies error)
	etlas bench
	etlas: Encountered missing dependencies:
	...
