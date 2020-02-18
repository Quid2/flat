# For internal testing only
include ../Makefile

dev: 
	stack test :doc --file-watch --fast --test-arguments="Data.Flat.Instances.Array"

dev2: 
	stack test :spec --file-watch --fast

clean:
	 find . -name "*.dump*" -exec rm -- {} +
	 cd src;find . -name "*.o" -delete;find . -name "*.hi" -delete;find . -name "*.dyn_*" -delete;find . -name "*.dump-*" -delete
	 cd test;find . -name "*.o" -delete;find . -name "*.hi" -delete;find . -name "*.dyn_*" -delete



opt=2 -DENUM_LARGE

speed : cspeed rspeed


tspeed:
	cd test;/usr/bin/time -l stack ghc -- -cpp -i../src -O$(opt) Spec;

tst: 
	/usr/bin/time -l stack test :spec

jtst:
	stack test :spec --fast

cspeed:
	# /usr/bin/time -l stack build :compy
	# touch test/Test/E.hs
	# touch test/Test/E/Binary.hs
	echo " " >> test/Spec.hs;/usr/bin/time -l stack test :spec --resolver lts-14.22
	#echo " " >> test/Spec.hs;stack test :spec
	#cd test;/usr/bin/time -l stack ghc -- -cpp -i../src -O$(opt) Test.E;/usr/bin/time -l stack ghc -- -i../src -cpp  -O$(opt) Test.E.Binary;/usr/bin/time -l stack ghc -- -i../src -cpp -O$(opt) Test.E.Flat
	#cd test;/usr/bin/time -l stack ghc -- -cpp -i../src -O$(opt) Test.E;/usr/bin/time -l stack ghc -- -i../src -cpp -O$(opt) Test.E.Flat	

# 8.0.2 1m56s
# 8.4.4
# 8.6.5 (flat 0.37) 2m51 (flat 0.4) 5m1
# 8.8.2  
# 8.6.5
#ver = lts-14.22 
# 8.4.4
#ver = lts-12.26
# 8.8.2 (O1 no BIG 221s)
ver = nightly-2020-01-25
cspeed:	
	# touch test/Spec.hs;time stack test :spec
	echo " " >> test/Spec.hs;/usr/bin/time -l stack test :spec --resolver $(ver)

rspeed: 
	stack bench :sbench
	ls -l /tmp/dump/test/Test/E/Flat.dump-simpl

bench: 
	stack bench :miniBench  --file-watch

# in /Users/titto/workspace/top-apps-ghcjs/reflex-platform/try-reflex
# js:
# 	stack build --system-ghc --stack-yaml=stack-ghcjs.yaml -v
	# -v 
	#cabal new-build --ghcjs

#  /Users/titto/workspace/flat/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/spec/spec:9065
#      h$currentThread.interruptible = true;
#                                    
# TypeError: Cannot set property 'interruptible' of null
jstest7:
	#cp ~/.local/bin/cabal-1.24.0.2 ~/.local/bin/cabal
	#cp ~/.local/bin/stack-1.9.3 ~/.local/bin/stack
	stack-1.9.3 clean
	stack-1.9.3 test --fast --file-watch --stack-yaml=stack-ghcjs.yaml  
	# --ghc-options "-UENUM_LARGE -UTEST_DECBITS"
	#cp ~/.local/bin/cabal-2.4.1.0 ~/.local/bin/cabal
	#cp ~/.local/bin/stack-2.1.3.1 ~/.local/bin/stack

#/Users/titto/workspace/flat/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/spec/spec:9101
#      h$currentThread.interruptible = true;
jstest9:
	#stack	 clean
	stack build --stack-yaml=stack-ghcjs.yaml  
	stack test --fast --file-watch --stack-yaml=stack-ghcjs-9.21.yaml  

# :spec TOO SLOW (or even get stuck)
# :doc won't compile
jstest:
		nix-shell -A env --run 'cabal new-test :spec --constraint "tasty -clock" --ghcjs'

jstests:
	stack clean;stack test --fast --file-watch --stack-yaml=stack-ghcjs-9.21.yaml  --ghc-options "-UENUM_LARGE -UTEST_DECBITS"

jsbench:
	stack bench :sbench --stack-yaml=stack-ghcjs.yaml

jsprof:
	node --prof /Users/titto/workspace/flat/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/spec/spec
	node --prof-process isolate-0x102801c00-v8.log > processed.txt


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
