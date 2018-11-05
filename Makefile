dev: 
	stack test :spec --file-watch --fast

clean:
	 find . -name "*.dump*" -exec rm -- {} +
	 cd src;find . -name "*.o" -delete;find . -name "*.hi" -delete;find . -name "*.dyn_*" -delete
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


rspeed: 
	stack bench :sbench
	ls -l /tmp/dump/test/Test/E/Flat.dump-simpl

bench: 
	stack bench :sbench  --file-watch

jstest:
	stack test --fast --file-watch --stack-yaml=stack-ghcjs.yaml

docs: 
	stack haddock --no-haddock-deps --open

eta:
	etlas update
	etlas select latest
	etlas build
	etlas install --dependencies-only
	etlas install tasty tasty-hunit tasty-quickcheck
	etlas test
	echo "Remove doctest from cabal!"
