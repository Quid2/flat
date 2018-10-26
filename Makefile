dev: 
	stack test :spec --file-watch --fast

clean:
	 find . -name "*.dump*" -exec rm -- {} +
	 cd src;find . -name "*.o" -delete;find . -name "*.hi" -delete
	 cd test;find . -name "*.o" -delete;find . -name "*.hi" -delete


opt=2

cspeed:
	# time stack build :compy
	touch test/Test/E.hs
	touch test/Test/E/Flat.hs
	touch test/Test/E/Binary.hs
	cd test;time ghc -i../src -O$(opt) Test.E;time ghc -i../src -O$(opt) Test.E.Binary;time ghc -i../src -O$(opt) Test.E.Flat


rspeed: 
	stack bench :sbench

jstest:
	cd test-ghcjs;make tst
